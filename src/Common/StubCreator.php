<?php

/** @noinspection PhpArrayShapeAttributeCanBeAddedInspection */

/** @noinspection PhpInternalEntityUsedInspection */

namespace TgScraper\Common;

use Illuminate\Support\Str;
use Nette\PhpGenerator\ClassType;
use Nette\PhpGenerator\Helpers;
use Nette\PhpGenerator\InterfaceType;
use Nette\PhpGenerator\Literal;
use Nette\PhpGenerator\Method;
use Nette\PhpGenerator\Parameter;
use Nette\PhpGenerator\PhpFile;
use Nette\PhpGenerator\PhpNamespace;
use Nette\PhpGenerator\PromotedParameter;
use Nette\PhpGenerator\Property;
use Nette\PhpGenerator\Type;
use Nette\Utils\Validators;
use TgScraper\Common\AbstractClassResolvers\AbstractClassResolverInterface;
use TgScraper\Common\RedefinedTypes\RedefinedTypeInterface;
use TgScraper\TgScraper;

class StubCreator
{
    private string $namespace;

    /**
     * @var array<string>
     */
    private array $abstractClasses = [];

    /**
     * @var array<string>
     */
    private array $extendedClasses = [];

    /**
     * @var array<string, array<string>>
     */
    private array $extendedBy = [];

    /**
     * @var array<AbstractClassResolverInterface>
     */
    private array $abstractClassResolvers = [];

    /**
     * @var array<class-string, RedefinedTypeInterface>
     */
    private array $redefinedTypes = [];

    /**
     * StubCreator constructor.
     *
     * @throws \InvalidArgumentException
     */
    public function __construct(private array $schema, string $namespace = '')
    {
        if (str_ends_with($namespace, '\\')) {
            $namespace = substr($namespace, 0, -1);
        }

        if (!empty($namespace)) {
            if (!Helpers::isNamespaceIdentifier($namespace)) {
                throw new \InvalidArgumentException('Namespace invalid');
            }
        }

        if (!TgScraper::validateSchema($this->schema)) {
            throw new \InvalidArgumentException('Schema invalid');
        }

        $this->parseAbstractTypes();
        $this->parseRedefinedTypes();

        $this->namespace = $namespace;
    }

    /**
     * Builds the abstract and the extended class lists.
     */
    private function parseAbstractTypes(): void
    {
        $abstractResolversNamespace = 'TgScraper\Common\AbstractClassResolvers';

        foreach ($this->schema['types'] as $type) {
            if (!empty($type['extended_by'])) {
                $typeName = $type['name'];
                $this->abstractClasses[] = $typeName;
                $this->extendedBy[$typeName] = $type['extended_by'];

                foreach ($type['extended_by'] as $extendedType) {
                    $this->extendedClasses[$extendedType] = $typeName;
                }

                if (class_exists($abstractResolversNamespace . '\\' . $typeName)) {
                    $this->abstractClassResolvers[$typeName] = $abstractResolversNamespace . '\\' . $typeName;
                }
            }
        }
    }

    private function parseRedefinedTypes(): void
    {
        $redefinedTypesNamespace = 'TgScraper\Common\RedefinedTypes';

        foreach ($this->schema['types'] as $type) {
            // replace class constructor is this type exist:
            if (class_exists($redefinedTypesNamespace . '\\' . $type['name'])) {
                $redefinedType = $redefinedTypesNamespace . '\\' . $type['name'];

                if (is_subclass_of($redefinedType, RedefinedTypeInterface::class)) {
                    $this->redefinedTypes[$type['name']] = $redefinedType;
                }
            }
        }
    }

    private static function toCamelCase(string $str): string
    {
        return lcfirst(str_replace(' ', '', ucwords(str_replace('_', ' ', $str))));
    }

    private function parseFieldTypes(array $fieldTypes, PhpNamespace $phpNamespace): array
    {
        $types = [];
        $comments = [];
        $arrayType = null;

        foreach ($fieldTypes as $fieldType) {
            if (str_starts_with($fieldType, 'Array')) {
                $fieldType = str_replace('Array', 'array', $fieldType);
                $types[] = 'array';

                $innerType = explode('<', $fieldType);

                $arrayLevels = 0;
                $innerType = $innerType[count($innerType) - 1];

                while (str_ends_with($innerType, '>')) {
                    ++$arrayLevels;
                    $innerType = substr($innerType, 0, -1);
                }

                $arrayType = [$arrayLevels, $innerType];

                $commentType = $fieldType;

                if (ucfirst($innerType) == $innerType) {
                    $commentType = str_replace($innerType, $innerType . 'Interface', $fieldType);

                    $innerType = $phpNamespace->getName() . '\\Interfaces\\' . $innerType . 'Interface';

                    $phpNamespace->addUse($innerType);
                }

                $comments[] = $commentType;

                continue;
            }

            $commentType = $fieldType;

            if (ucfirst($fieldType) == $fieldType) {
                $commentType .= 'Interface';

                $fieldType = $phpNamespace->getName() . '\\Interfaces\\' . $fieldType . 'Interface';
                $phpNamespace->addUse($fieldType);
            }

            $comments[] = $commentType;
            $types[] = $fieldType;
        }

        $comments = empty($comments) ? '' : sprintf('@param %s', implode('|', $comments));

        return [
            'types' => implode('|', $types),
            'comments' => $comments,
            'arrayType' => $arrayType,
        ];
    }

    private function parseApiFieldTypes(
        array $apiTypes,
        PhpNamespace $phpNamespace,
        PhpNamespace $interfaceNamespace,
    ): array {
        $types = [];
        $comments = [];

        foreach ($apiTypes as $apiType) {
            if (str_starts_with($apiType, 'Array')) {
                $types[] = 'array';

                $comment = str_replace('Array', 'array', $apiType);

                $text = $apiType;

                while (preg_match('/Array<(.+)>/', $text, $matches) === 1) {
                    $text = $matches[1];
                }

                $subTypes = explode('|', $text);
                foreach ($subTypes as $subType) {
                    if (ucfirst($subType) == $subType) {
                        $comment = str_replace($subType, $subType . 'Interface', $comment);

                        $subType = "$this->namespace\\Types\\Interfaces\\{$subType}Interface";
                        $phpNamespace->addUse($subType);
                        if ($interfaceNamespace !== null) {
                            $interfaceNamespace->addUse($subType);
                        }
                    }
                }

                $comments[] = $comment;

                continue;
            }

            $commentType = $apiType;

            if (ucfirst($apiType) == $apiType) {
                $commentType .= 'Interface';
                $apiType = "$this->namespace\\Types\\Interfaces\\{$apiType}Interface";
                $phpNamespace->addUse($apiType);

                if ($interfaceNamespace !== null) {
                    $interfaceNamespace->addUse($apiType);
                }
            }

            $comments[] = $commentType;

            $types[] = $apiType;
        }

        $comments = empty($comments) ? '' : sprintf('@param %s', implode('|', $comments));

        return [
            'types' => implode('|', $types),
            'comments' => $comments,
        ];
    }

    /**
     * @return array{
     *     Response: array{class:PhpFile,interface:PhpFile},
     *     TypeInterface: PhpFile
     * }
     */
    private function generateDefaultTypes(string $namespace): array
    {
        $interfaceFile = new PhpFile();

        $interfaceNamespace = $interfaceFile->addNamespace($namespace . '\\Interfaces');
        $interfaceNamespace->addInterface('TypeInterface');

        $responseFile = new PhpFile();
        $responseNamespace = $responseFile->addNamespace($namespace);

        $interfacesNamespace = $namespace . '\\Interfaces';

        $responseInterfaceFile = new PhpFile();
        $responseInterfaceNamespace = $responseInterfaceFile->addNamespace($interfacesNamespace);

        $comment = <<<TXT
            The response contains a JSON object, which always has a Boolean field 'ok' and may 
            have an optional String field 'description' with a human-readable description
            of the result.
            
            If 'ok' equals True, the request was successful and the result of the query
            can be found in the 'result' field. In case of an unsuccessful request,
            'ok' equals false and the error is explained in the 'description'.
            
            An Integer 'error_code' field is also returned, but its contents are subject to change in the future.
            Some errors may also have an optional field 'parameters' of the type ResponseParameters,
            which can help to automatically handle the error.
            TXT;

        $response = $responseNamespace->addClass('Response');
        $response->addComment($comment);

        $responseInterface = $responseInterfaceNamespace->addInterface('ResponseInterface');
        $responseInterface->addComment($comment);

        $constructor = $response->addMethod('__construct');

        $constructor->addPromotedParameter('ok')
            ->setPublic()
            ->setType(Type::Bool);

        $okProperty = $responseInterface->addProperty('ok')
            ->setType(Type::Bool)
            ->setPublic();
        $okProperty->addGetHook('');
        $okProperty->addSetHook('');

        $constructor->addPromotedParameter('result')
            ->setPublic()
            ->setType(Type::Mixed)
            ->setNullable()
            ->setDefaultValue(null)
            ->setComment('associative JSON decoded value of the result field');

        $resultProperty = $responseInterface->addProperty('result')
            ->setType(Type::Mixed)
            ->setNullable()
            ->setPublic();
        $resultProperty->addGetHook('');
        $resultProperty->addSetHook('');

        $constructor->addPromotedParameter('errorCode')
            ->setPublic()
            ->setType(Type::Int)
            ->setNullable()
            ->setDefaultValue(null);

        $errorCodeProperty = $responseInterface->addProperty('errorCode')
            ->setType(Type::Int)
            ->setNullable()
            ->setPublic();
        $errorCodeProperty->addGetHook('');
        $errorCodeProperty->addSetHook('');

        $constructor->addPromotedParameter('description')
            ->setPublic()
            ->setType(Type::String)
            ->setNullable()
            ->setDefaultValue(null);

        $descriptionProperty = $responseInterface->addProperty('description')
            ->setType(Type::String)
            ->setNullable()
            ->setPublic();
        $descriptionProperty->addGetHook('');
        $descriptionProperty->addSetHook('');

        $constructor->addPromotedParameter('parameters')
            ->setPublic()
            ->setType($interfacesNamespace . '\\ResponseParametersInterface')
            ->setNullable()
            ->setDefaultValue(null);

        $parametersProperty = $responseInterface->addProperty('parameters')
            ->setType($interfacesNamespace . '\\ResponseParametersInterface')
            ->setNullable()
            ->setPublic();
        $parametersProperty->addGetHook('');
        $parametersProperty->addSetHook('');

        $response->addImplement($interfacesNamespace . '\\ResponseInterface');

        $responseInterface->addExtend($interfacesNamespace  . '\\TypeInterface');

        return [
            'Response' => [
                'class' => $responseFile,
                'interface' => $responseInterfaceFile,
            ],
            'TypeInterface' => $interfaceFile,
        ];
    }

    /**
     * @return array{
     *     types: array<string, array{class: PhpFile, interface: PhpFile}>,
     *     files: array<string,PhpFile>,
     *     defaultTypeInterface: PhpFile
     * }
     */
    public function generateTypes(): array
    {
        $namespace = $this->namespace . '\\Types';
        $interfaceNamespace = $namespace . '\\Interfaces';

        [
            'Response' => $responseType,
            'TypeInterface' => $defaultTypeInterface,
        ] = $this->generateDefaultTypes($namespace);

        $types = [
            'Response' => $responseType,
        ];

        $denormalizers = [];
        $factoryMethods = [];

        foreach ($this->schema['types'] as $type) {
            $file = new PhpFile();
            $phpNamespace = $file->addNamespace($namespace);
            $typeClass = $phpNamespace->addClass($type['name']);

            $interfaceFile = new PhpFile();
            $phpInterfaceNamespace = $interfaceFile->addNamespace($interfaceNamespace);
            $typeInterface = $phpInterfaceNamespace->addInterface($type['name'] . 'Interface');
            $typeInterface->addExtend($interfaceNamespace . '\\TypeInterface');

            $constructor = $typeClass->addMethod('__construct');

            $params = [];
            $properties = [];

            if (isset($type['description'])) {
                $typeClass->addComment($type['description']);
                $typeInterface->addComment($type['description']);
            }

            if (in_array($type['name'], $this->abstractClasses)) {
                $typeClass->setAbstract();

                foreach ($this->extendedBy[$type['name']] as $extendedType) {
                    $typeClass->addComment("@see $extendedType");
                    $typeInterface->addComment("@see {$extendedType}Interface");
                }
            }

            if (array_key_exists($type['name'], $this->extendedClasses)) {
                $typeClass->setExtends($namespace . '\\' . $this->extendedClasses[$type['name']]);
            }

            $typeClass->addImplement($interfaceNamespace . '\\' . $type['name'] . 'Interface');

            $arrayTypes = [];

            foreach ($type['fields'] as $field) {
                [
                    'types' => $fieldType,
                    'comments' => $fieldComment,
                    'arrayType' => $arrayType,
                ] = $this->parseFieldTypes(
                    $field['types'],
                    $phpNamespace
                );

                if ($arrayType !== null) {
                    $arrayTypes[$field['name']] = $arrayType;
                }

                $fieldName = self::toCamelCase($field['name']);
                $param = new PromotedParameter($fieldName)->setType($fieldType);
                $property = new Property($fieldName)->setType($fieldType)->setPublic();
                $property->addSetHook('');
                $property->addGetHook('');

                if ($field['optional']) {
                    $param->setNullable();
                    $param->setDefaultValue(null);

                    $property->setNullable();
                    $property->setValue(null);

                    if ($fieldComment !== '') {
                        $fieldComment .= '|null';
                    }
                } else {
                    if (isset($field['default'])) {
                        $param->setDefaultValue($field['default']);
                        $property->setValue($field['default']);
                    }
                }

                if ($fieldComment !== '') {
                    $fieldComment .= sprintf(' $%s %s', $fieldName, $field['description']);

                    $property->addComment(
                        str_replace('@param', '@var', $fieldComment)
                    );
                }

                $params[] = [$param, $fieldComment];
                $properties[] = $property;
            }

            usort($params, function ($a, $b) {
                return (int) $a[0]->hasDefaultValue() - (int) $b[0]->hasDefaultValue();
            });

            if (isset($this->redefinedTypes[$type['name']])) {
                $redefinedType = $this->redefinedTypes[$type['name']];

                $constructorParams = $redefinedType::getConstructorParams();
                $constructorComment = $redefinedType::getConstructorComment();
                $properties = $redefinedType::getInterfaceProperties();
            } else {
                $constructorParams = array_map(fn ($a) => $a[0], $params);
                $constructorComment = implode("\n", array_map(fn ($a) => $a[1], $params));
            }

            $constructor->setParameters($constructorParams);
            $constructor->setComment($constructorComment);

            $typeInterface->setProperties($properties);

            $types[$type['name']] = [
                'class' => $file,
                'interface' => $interfaceFile,
            ];

            $denormalizers[$type['name']] = $this->generateDenormalizeTypeMethod(
                $constructorParams,
                $type['name'],
                $arrayTypes
            );

            if (!in_array($type['name'], $this->abstractClasses)) {
                $factoryMethods[$type['name']] = $this->generateFactoryMethod(
                    $constructorParams,
                    $type['name'],
                );
            }

            if (count($constructorParams) === 0) {
                $typeClass->removeMethod('__construct');
            }
        }

        return [
            'types' => $types,
            'files' => [
                ...$this->generateApi($denormalizers),
                ...$this->generateFactory($factoryMethods),
            ],
            'defaultTypeInterface' => $defaultTypeInterface,
        ];
    }

    /**
     * @param array<string,Method> $factoryMethods
     *
     * @return array<string,PhpFile>
     */
    private function generateFactory(array $factoryMethods): array
    {
        $file = new PhpFile();
        $namespace = $file->addNamespace($this->namespace);

        $factoryClass = $namespace->addClass('Factory');

        $factoryInterfaceFile = new PhpFile();
        $interfaceNamespace = $factoryInterfaceFile->addNamespace($this->namespace);
        $factoryInterface = $interfaceNamespace->addInterface('FactoryInterface');

        $factoryClass->addImplement($this->namespace . '\\FactoryInterface');

        $factoryClass->setMethods($factoryMethods);
        $factoryInterface->setMethods($factoryMethods);

        foreach ($factoryMethods as $typeName => $method) {
            $fullName = $this->namespace . '\\Types\\' . $typeName;
            $fullInterfaceName = $this->namespace . '\\Types\\Interfaces\\' . $typeName . 'Interface';

            $namespace->addUse($fullName);
            $namespace->addUse($fullInterfaceName);

            $interfaceNamespace->addUse($fullInterfaceName);
        }

        return [
            'Factory' => $file,
            'FactoryInterface' => $factoryInterfaceFile,
        ];
    }

    /**
     * @param array<string,Method> $denormalizers
     *
     * @return array<string,PhpFile>
     */
    private function generateApi(array $denormalizers): array
    {
        $file = new PhpFile();
        $apiInterfaceFile = new PhpFile();

        $phpNamespace = $file->addNamespace($this->namespace);
        $apiClass = $phpNamespace->addClass('Api');

        $apiInterfaceNamespace = $apiInterfaceFile->addNamespace($this->namespace);
        $apiInterface = $apiInterfaceNamespace->addInterface('ApiInterface');

        $apiClass->addImplement($this->namespace . '\\' . $apiInterface->getName());

        [$clientInterfaceFile, $clientInterface] = $this->generateClientInterface();
        [$serializerInterfaceFile, $serializerInterface] = $this->generateSerializerInterface();
        [$serializerFile, $serializer] = $this->generateSerializer($denormalizers);

        $constructor = $apiClass->addMethod('__construct');
        $constructor
            ->addPromotedParameter('client')
            ->setType($this->namespace . '\\' . $clientInterface->getName())
            ->setVisibility('protected');

        $constructor
            ->addPromotedParameter('serializer')
            ->setType($this->namespace . '\\' . $serializerInterface->getName())
            ->setVisibility('protected')
            ->setDefaultValue(new Literal('new Serializer()'));

        $doRequestMethod = $apiClass
            ->addMethod('doRequest')
            ->setProtected();

        $doRequestMethod
            ->addComment(
                <<<'COMMENT'
                    @param array<mixed>              $args
                    @param class-string|'bool'|'string'|'int' $returnType

                    @phpstan-ignore-next-line TODO: add generics to the promise from $returnType
                    COMMENT
            );

        $doRequestMethod
            ->addParameter('method')
            ->setType(Type::String);

        $doRequestMethod
            ->addParameter('args')
            ->setType(Type::Array);

        $doRequestMethod
            ->addParameter('returnType')
            ->setType(Type::String);

        $doRequestMethod
            ->addParameter('returnsArray')
            ->setType(Type::Bool)
            ->setDefaultValue(false);

        $doRequestMethod
            ->setReturnType('mixed');

        $doRequestMethod
            ->addBody(
                <<<'BODY'
                    $response = $this->client->sendRequest(
                        $method,
                        $this->serializer->serialize($args)
                    );
                    
                    if (!$response->ok || $response->result === null) {
                        throw new ResponseException($response);
                    }

                    return $this->serializer->deserialize(
                        $response->result,
                        $returnType,
                        $returnsArray
                    );
                    BODY
            );

        foreach ($this->schema['methods'] as $method) {
            $function = $apiClass
                ->addMethod($method['name'])
                ->setPublic()
                ->addComment($method['description']);

            $interfaceFunction = $apiInterface
                ->addMethod($method['name'])
                ->setPublic()
                ->addComment($method['description']);

            $fields = $method['fields'];
            usort(
                $fields,
                function ($a, $b) {
                    return $a['optional'] - $b['optional'];
                }
            );

            foreach ($fields as $field) {
                [
                    'types' => $types,
                    'comments' => $comment,
                ] = $this->parseApiFieldTypes($field['types'], $phpNamespace, $apiInterfaceNamespace);

                $fieldName = self::toCamelCase($field['name']);
                $parameter = $function
                    ->addParameter($fieldName)
                    ->setType($types);

                $interfaceParam = $interfaceFunction
                    ->addParameter($fieldName)
                    ->setType($types);

                $default = $field['default'] ?? null;
                if (!empty($default) and (!is_string($default) or lcfirst($default) == $default)) {
                    $parameter->setDefaultValue($default);
                    $interfaceParam->setDefaultValue($default);
                }

                if ($field['optional']) {
                    $parameter->setNullable();
                    $interfaceParam->setNullable();

                    if (!$parameter->hasDefaultValue()) {
                        $parameter->setDefaultValue(null);
                        $interfaceParam->setDefaultValue(null);
                    }

                    $comment .= '|null';
                }

                $comment .= sprintf(' $%s %s', $fieldName, $field['description']);
                $function->addComment($comment);
                $interfaceFunction->addComment($comment);
            }

            [
                'comments' => $returnComment,
            ] = $this->parseApiFieldTypes($method['return_types'], $phpNamespace, $apiInterfaceNamespace);

            $expectedReturnTypes = array_map(
                function (string $type) {
                    if (str_starts_with($type, 'array')) {
                        // find the last type in the array
                        $realType = explode('<', $type);
                        $realType = $realType[count($realType) - 1];
                        $realType = explode('>', $realType)[0];

                        return [$realType, true];
                    }

                    return [$type, false];
                },
                explode(
                    '|',
                    str_replace('@param ', '', $returnComment)
                )
            );

            if (count($expectedReturnTypes) > 1 && $expectedReturnTypes[1][0] !== 'bool') {
                throw new \LogicException('Multiple return types not supported. Get back here and figure something out again.');
            }

            [$expectedReturnType, $isArray] = $expectedReturnTypes[0];

            $body = <<<'BODY'
                return $this->doRequest(
                    method: '%s',
                    args: get_defined_vars(),
                    returnType: %s,%s
                );
                BODY;

            $returnType = Validators::isBuiltinType($expectedReturnType)
                ? "'$expectedReturnType'"
                : "$expectedReturnType::class";

            $function->addBody(sprintf(
                $body,
                $method['name'],
                $returnType,
                $expectedReturnTypes[0][1] ? "\n    returnsArray: true," : '',
            ));

            $returnComment = sprintf(
                '@return %s',
                str_replace('@param ', '', $returnComment)
            );

            if ($isArray) {
                $functionReturnType = 'array';
            } else {
                if (Validators::isBuiltinType($expectedReturnType)) {
                    $functionReturnType = $expectedReturnType;
                } else {
                    $functionReturnType = "$this->namespace\\Types\\Interfaces\\$expectedReturnType";
                }
            }

            if (count($expectedReturnTypes) > 1) {
                $functionReturnType .= '|bool';
            }

            $function
                ->setReturnType($functionReturnType)
                ->addComment(str_replace('param', 'return', $returnComment));

            $interfaceFunction
                ->setReturnType($functionReturnType)
                ->addComment(str_replace('param', 'return', $returnComment));
        }

        return [
            'Api' => $file,
            'ApiInterface' => $apiInterfaceFile,
            'ClientInterface' => $clientInterfaceFile,
            'SerializerInterface' => $serializerInterfaceFile,
            'Serializer' => $serializerFile,
        ];
    }

    private function generateFactoryMethod(
        array $params,
        string $type,
    ): Method {
        $factoryMethod = new Method(sprintf('make%s', $type));
        $factoryMethod->setPublic();
        $factoryMethod->setReturnType($this->namespace . '\\Types\\Interfaces\\' . $type . 'Interface');

        if (count($params) === 0) {
            $factoryMethod->addBody(
                sprintf('return new %s;', $type)
            );

            return $factoryMethod;
        }

        $factoryMethod->addBody(sprintf('return new %s(', $type));

        /** @var Parameter $param */
        foreach ($params as $param) {
            $factoryMethod->addBody(sprintf(
                '    %s: $%s,',
                $param->getName(),
                $param->getName(),
            ));
        }

        $factoryMethod->addBody(');');

        $factoryMethod->setParameters(array_map(
            function (PromotedParameter $promoted) {
                $param = new Parameter($promoted->getName())
                    ->setType($promoted->getType())
                    ->setNullable($promoted->isNullable())
                    ->setComment($promoted->getComment())
                    ->setAttributes($promoted->getAttributes())
                    ->setReference($promoted->isReference());

                if ($param->hasDefaultValue()) {
                    $param->setDefaultValue($promoted->getDefaultValue());
                }

                return $param;
            },
            $params
        ));

        return $factoryMethod;
    }

    /**
     * @param array<string,array{0: int, 1: string}> $arrayTypes
     */
    private function generateDenormalizeTypeMethod(
        array $params,
        string $type,
        array $arrayTypes,
    ): Method {
        $denormalizeTypeMethod = new Method(sprintf('denormalize%s', $type));
        $denormalizeTypeMethod->setPublic();
        $denormalizeTypeMethod->setReturnType($this->namespace . '\\Types\\Interfaces\\' . $type . 'Interface');
        $denormalizeTypeMethod
            ->addParameter('data')
            ->setType(Type::Array);

        if (in_array($type, $this->abstractClasses)) {
            if (!isset($this->abstractClassResolvers[$type])) {
                $denormalizeTypeMethod->addBody(
                    sprintf(
                        'throw new \RuntimeException("class %s is abstract and not yet implemented");',
                        $type
                    )
                );

                return $denormalizeTypeMethod;
            }

            $denormalizeTypeMethod->addBody(
                $this->abstractClassResolvers[$type]::getBody($params)
            );

            return $denormalizeTypeMethod;
        }

        if (count($params) === 0) {
            $denormalizeTypeMethod->addBody(
                sprintf('return $this->factory->make%s();', $type)
            );

            return $denormalizeTypeMethod;
        }

        $requiredParams = array_filter($params, fn ($param) => !$param->hasDefaultValue());

        if (count($requiredParams) > 0) {
            $denormalizeTypeMethod->addBody('$requiredFields = [');
            foreach ($requiredParams as $param) {
                $denormalizeTypeMethod->addBody(sprintf(
                    '    \'%s\',',
                    Str::snake($param->getName())
                ));
            }

            $denormalizeTypeMethod->addBody("];\n");

            $denormalizeTypeMethod->addBody(
                <<<'RequiredCheck'
            $missingFields = [];

            foreach ($requiredFields as $field) {
                if (!isset($data[$field])) {
                    $missingFields[] = $field;
                }
            }

            if (count($missingFields) > 0) {
                throw new \InvalidArgumentException(sprintf(
                    'Class 
            RequiredCheck
                    . $type .
            <<<'RequiredCheck'
             missing some fields from the data array: %s',
                    implode(', ', $missingFields),
                ));
            }

            RequiredCheck
            );
        }

        //        $denormalizeTypeMethod->addBody(sprintf('return new %s(', $type));
        $denormalizeTypeMethod->addBody(sprintf('return $this->factory->make%s(', $type));

        /** @var Parameter $param */
        foreach ($params as $param) {
            if ($param->hasDefaultValue()) {
                $defaultValue = $param->getDefaultValue();
                if (is_string($defaultValue)) {
                    $defaultValue = sprintf('\'%s\'', $defaultValue);
                } elseif (is_bool($defaultValue)) {
                    $defaultValue = sprintf('%s', $defaultValue ? 'true' : 'false');
                } elseif (is_null($defaultValue)) {
                    $defaultValue = 'null';
                } else {
                    $defaultValue = null;
                }
            } else {
                $defaultValue = null;
            }

            $snakeParamName = Str::snake($param->getName());
            $value = sprintf('$data[\'%s\']', $snakeParamName);

            $paramType = (string) $param->getType();
            $paramTypeBase = explode('\\', $paramType);
            $paramTypeBase = $paramTypeBase[count($paramTypeBase) - 1];
            $paramTypeBase = explode('|', $paramTypeBase);
            $paramTypeBase = $paramTypeBase[0];
            $paramTypeBase = str_replace('Interface', '', $paramTypeBase);

            $handlingArray = $paramTypeBase === 'array';

            if ($handlingArray) {
                [$arrayLevel, $paramTypeBase] = $arrayTypes[$snakeParamName];
            }

            if (!Validators::isBuiltinType($paramTypeBase) && $paramTypeBase !== 'InputFile') {
                if ($defaultValue !== null) {
                    if ($handlingArray) {
                        if ($arrayLevel === 1) {
                            $value = <<<VALUE
                            isset($value)
                                    ? array_map(fn (array \$item) => \$this->denormalize{$paramTypeBase}(\$item), $value)
                                    : null
                            VALUE;
                        } elseif ($arrayLevel === 2) {
                            $value = <<<VALUE
                            isset($value)
                                    ? array_map(
                                        fn (array \$item0) => array_map(
                                            fn (array \$item1) => \$this->denormalize{$paramTypeBase}(\$item1),
                                            \$item0
                                        ),
                                        $value
                                    )
                                    : null
                            VALUE;
                        } else {
                            throw new \RuntimeException('Array level >2 not supported');
                        }
                    } else {
                        $value = <<<VALUE
                        isset($value)
                                ? \$this->denormalize{$paramTypeBase}($value)
                                : null
                        VALUE;
                    }

                    $defaultValue = null;
                } else {
                    if ($handlingArray) {
                        if ($arrayLevel === 1) {
                            $value = sprintf(
                                'array_map(fn (array $item) => $this->denormalize%s($item), $data[\'%s\'])',
                                $paramTypeBase,
                                $snakeParamName
                            );
                        } elseif ($arrayLevel === 2) {
                            $value = sprintf(
                                <<<'BODY'
                                array_map(
                                        fn (array $item0) => array_map(
                                            fn (array $item1) => $this->denormalize%s($item1),
                                            $item0
                                        ),
                                        $data['%s']
                                    )
                                BODY,
                                $paramTypeBase,
                                $snakeParamName
                            );
                        } else {
                            throw new \RuntimeException('Array level >2 not supported');
                        }
                    } else {
                        $value = sprintf(
                            '$this->denormalize%s($data[\'%s\'])',
                            $paramTypeBase,
                            $snakeParamName
                        );
                    }
                }
            }

            $denormalizeTypeMethod->addBody(sprintf(
                '    %s: %s%s,',
                $param->getName(),
                $value,
                $defaultValue !== null ? sprintf(' ?? %s', $defaultValue) : ''
            ));
        }

        $denormalizeTypeMethod->addBody(');');

        return $denormalizeTypeMethod;
    }

    /**
     * @return array{0: PhpFile, 1: InterfaceType}
     */
    private function generateClientInterface(): array
    {
        $file = new PhpFile();

        $phpNamespace = $file->addNamespace($this->namespace);
        $phpNamespace->addUse("$this->namespace\\Types\\Interfaces\\ResponseInterface");

        $interface = $phpNamespace->addInterface('ClientInterface');

        $method = $interface->addMethod('sendRequest')->setPublic();

        $method->addParameter('method')->setType(Type::String);
        $method->addParameter('data')->setType(Type::Array);
        $method->setReturnType("$this->namespace\\Types\\Interfaces\\ResponseInterface");

        return [$file, $interface];
    }

    /**
     * @return array{0: PhpFile, 1: InterfaceType}
     */
    private function generateSerializerInterface(): array
    {
        $file = new PhpFile();

        $phpNamespace = $file->addNamespace($this->namespace);

        $interface = $phpNamespace->addInterface('SerializerInterface');

        $method = $interface->addMethod('serialize')->setPublic();
        $method->addParameter('data')->setType(Type::Array);
        $method->setReturnType(Type::Array);

        $method = $interface->addMethod('deserialize')->setPublic();
        $method->addParameter('data')->setType(Type::Mixed);
        $method->addParameter('type')->setType(Type::String);
        $method->addParameter('isArray')->setType(Type::Bool)->setDefaultValue(false);
        $method->setReturnType(Type::Mixed);

        return [$file, $interface];
    }

    /**
     * @param array<string, Method> $denormalizers
     *
     * @return array{0: PhpFile, 1: ClassType}
     */
    private function generateSerializer(array $denormalizers): array
    {
        $file = new PhpFile();

        $phpNamespace = $file->addNamespace($this->namespace);
        $phpNamespace->addUse($this->namespace . '\\Types\\Interfaces\\TypeInterface');
        $phpNamespace->addUse($this->namespace . '\\Types\\Interfaces\\InputFileInterface');

        $class = $phpNamespace->addClass('Serializer');
        $class->addImplement($this->namespace . '\\SerializerInterface');

        $serializeMethod = $class->addMethod('serialize');
        $serializeMethod->addParameter('data')->setType(Type::Array);
        $serializeMethod->setReturnType(Type::Array);
        $serializeMethod->setPublic();
        $serializeMethod->setBody('return $this->normalize($data);');

        $deserializeMethod = $class->addMethod('deserialize');
        $deserializeMethod->addParameter('data')->setType(Type::Mixed);
        $deserializeMethod->addParameter('type')->setType(Type::String);
        $deserializeMethod->addParameter('isArray')->setType(Type::Bool)->setDefaultValue(false);
        $deserializeMethod->setReturnType('mixed');
        $deserializeMethod->setPublic();
        $deserializeMethod->setBody(<<<'BODY'
            return is_array($data) 
                ? $this->denormalize($data, $type, $isArray)
                : $data;
            BODY
        );

        $denormalizeMethod = $class->addMethod('denormalize');
        $denormalizeMethod->addParameter('data')->setType(Type::Array);
        $denormalizeMethod->addParameter('type')->setType(Type::String);
        $denormalizeMethod->addParameter('isArray')->setType(Type::Bool)->setDefaultValue(false);
        $denormalizeMethod->setReturnType('mixed');
        $denormalizeMethod->setPublic();
        $denormalizeMethod->setBody(<<<'BODY'
            if (!interface_exists($type) || !is_subclass_of($type, TypeInterface::class)) {
                throw new \UnexpectedValueException(sprintf('Failed to decode response to the expected type: %s', $type));
            }
            
            if (!$isArray) {
                return $this->denormalizeType($data, $type);
            }
            
            return array_map(fn (array $item) => $this->denormalizeType($item, $type), $data);
            BODY
        );

        $denormalizeTypeMethod = $class->addMethod('denormalizeType');
        $denormalizeTypeMethod->addParameter('data')->setType(Type::Array);
        $denormalizeTypeMethod->addParameter('type')->setType(Type::String);
        $denormalizeTypeMethod->setReturnType($this->namespace . '\\Types\\Interfaces\\TypeInterface');
        $denormalizeTypeMethod->setPrivate();
        $denormalizeTypeMethod->addBody('
        return match ($type) {');

        foreach ($denormalizers as $type => $denormalizer) {
            if (in_array($type, $this->abstractClasses)) {
                // TODO:
                continue;
            }

            $denormalizeTypeMethod->addBody(
                "{$type}Interface::class => \$this->denormalize{$type}(\$data),"
            );

            $phpNamespace->addUse("$this->namespace\\Types\\Interfaces\\{$type}Interface");
        }

        $denormalizeTypeMethod->addBody(
            '        default => throw new \InvalidArgumentException(sprintf(\'Unknown type %s\', $type)),
    };
');

        $class->setMethods(
            array_merge(
                array_values($denormalizers),
                $class->getMethods()
            )
        );

        $normalizeMethod = $class->addMethod('normalize');
        $normalizeMethod->addParameter('data')->setType('array');
        $normalizeMethod->setReturnType('array');
        $normalizeMethod->setPrivate();
        $normalizeMethod->setBody(<<<'PHP'
            $result = [];
            
            foreach ($data as $key => $value) {
                if (is_null($value)) {
                    continue;
                }
                
                $snakeKey = $this->camelToSnake($key);
                
                if ($value instanceof TypeInterface && !$value instanceof InputFileInterface) {
                    $value = get_object_vars($value);
                }
                
                if (is_array($value)) {
                    $result[$snakeKey] = $this->normalize($value);
                } else {
                    $result[$snakeKey] = $value;
                }
            }
            
            return $result;
            PHP
        );

        $camelToSnakeMethod = $class->addMethod('camelToSnake');
        $camelToSnakeMethod->addParameter('input')->setType('string');
        $camelToSnakeMethod->setReturnType('string');
        $camelToSnakeMethod->setPrivate();
        $camelToSnakeMethod->setBody('
            return strtolower(preg_replace(\'/[A-Z]/\', \'_$0\', lcfirst($input)));
        ');

        $constructor = new Method('__construct')->setPublic();
        $constructor->addPromotedParameter('factory')
            ->setType($this->namespace . '\\FactoryInterface')
            ->setDefaultValue(new Literal('new Factory'))
            ->setReadOnly()
            ->setPrivate();

        $class->setMethods(
            array_merge(
                [$constructor],
                $class->getMethods()
            )
        );

        return [$file, $class];
    }
}
