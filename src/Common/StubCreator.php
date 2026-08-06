<?php

/** @noinspection PhpArrayShapeAttributeCanBeAddedInspection */

/** @noinspection PhpInternalEntityUsedInspection */

namespace TgScraper\Common;

use Illuminate\Support\Str;
use Nette\PhpGenerator\ClassType;
use Nette\PhpGenerator\Constant;
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
use Phenogram\Bindings\Factory;
use Phenogram\Bindings\Types\Interfaces\TypeInterface;
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
            $redefinedType = $redefinedTypesNamespace . '\\' . $type['name'];
            if (class_exists($redefinedType) && is_subclass_of($redefinedType, RedefinedTypeInterface::class)) {
                $this->redefinedTypes[$type['name']] = $redefinedType;
            }

            $redefinedTypeInterface = $redefinedType . 'Interface';
            if (class_exists($redefinedTypeInterface) && is_subclass_of($redefinedTypeInterface, RedefinedTypeInterface::class)) {
                $this->redefinedTypes[$type['name']] = $redefinedTypeInterface;
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
        $okProperty->addHook('get');
        $okProperty->addHook('set');

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
        $resultProperty->addHook('get');
        $resultProperty->addHook('set');

        $constructor->addPromotedParameter('errorCode')
            ->setPublic()
            ->setType(Type::Int)
            ->setNullable()
            ->setDefaultValue(null);

        $errorCodeProperty = $responseInterface->addProperty('errorCode')
            ->setType(Type::Int)
            ->setNullable()
            ->setPublic();
        $errorCodeProperty->addHook('get');
        $errorCodeProperty->addHook('set');

        $constructor->addPromotedParameter('description')
            ->setPublic()
            ->setType(Type::String)
            ->setNullable()
            ->setDefaultValue(null);

        $descriptionProperty = $responseInterface->addProperty('description')
            ->setType(Type::String)
            ->setNullable()
            ->setPublic();
        $descriptionProperty->addHook('get');
        $descriptionProperty->addHook('set');

        $constructor->addPromotedParameter('parameters')
            ->setPublic()
            ->setType($interfacesNamespace . '\\ResponseParametersInterface')
            ->setNullable()
            ->setDefaultValue(null);

        $parametersProperty = $responseInterface->addProperty('parameters')
            ->setType($interfacesNamespace . '\\ResponseParametersInterface')
            ->setNullable()
            ->setPublic();
        $parametersProperty->addHook('get');
        $parametersProperty->addHook('set');

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
     *     types: array<string, array{class: ?PhpFile, interface: PhpFile}>,
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
            $interfaceFile = new PhpFile();
            $phpInterfaceNamespace = $interfaceFile->addNamespace($interfaceNamespace);
            $typeInterface = $phpInterfaceNamespace->addInterface($type['name'] . 'Interface');
            $typeInterface->addExtend($interfaceNamespace . '\\TypeInterface');

            if ($type['name'] === 'InputFile') {
                $typeInterface->addComment($type['description']);

                $types[$type['name']] = [
                    'class' => null,
                    'interface' => $interfaceFile,
                ];

                continue;
            }

            $file = new PhpFile();
            $phpNamespace = $file->addNamespace($namespace);

            $typeClass = $phpNamespace->addClass($type['name']);
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
                $property->addHook('get');
                $property->addHook('set');

                if ($field['optional']) {
                    $param->setNullable();
                    $param->setDefaultValue(null);

                    $property->setNullable();

                    if ($fieldComment !== '') {
                        $fieldComment .= '|null';
                    }
                } else {
                    if (isset($field['default'])) {
                        $param->setDefaultValue($field['default']);
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

                dd('Not implemented yet', $redefinedType);
            } else {
                $constructorParams = array_map(fn ($a) => $a[0], $params);
                $constructorComment = implode("\n", array_map(fn ($a) => $a[1], $params));
            }

            if (count($constructorParams) > 0) {
                $constructor->setParameters($constructorParams);
            }

            if ($constructorComment !== '') {
                $constructor->addComment($constructorComment);
            }

            if (count($properties) > 0) {
                $typeInterface->setProperties($properties);
            }

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
                    @template T of object
                    
                    @param string                                 $method
                    @param array<mixed>                           $args
                    @param class-string<T>|'bool'|'string'|'int'  $returnType
                    @param bool                                   $returnsArray
                    
                    @return ($returnsArray is true ? array<T> : T)|bool|string|int
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

        $method = $interface->addMethod('supports')->setPublic();
        $method->addParameter('type')->setType(Type::String);
        $method->setReturnType(Type::Bool);

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

        $supportsMethod = $class->addMethod('supports');
        $supportsMethod->addParameter('type')->setType(Type::String);
        $supportsMethod->setReturnType(Type::Bool);
        $supportsMethod->setPublic();
        $supportsMethod->setBody(<<<'BODY'
            return in_array($type, self::KNOWN_INTERFACES, true);
        BODY);

        $denormalizeMethod = $class->addMethod('denormalize');
        $denormalizeMethod->addParameter('data')->setType(Type::Array);
        $denormalizeMethod->addParameter('type')->setType(Type::String);
        $denormalizeMethod->addParameter('isArray')->setType(Type::Bool)->setDefaultValue(false);
        $denormalizeMethod->setReturnType('mixed');
        $denormalizeMethod->setPublic();
        $denormalizeMethod->setBody(<<<'BODY'
            if (!$this->supports($type)) {
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

        $knownInterfaces = [];
        foreach ($denormalizers as $type => $denormalizer) {
            if (
                in_array($type, $this->abstractClasses, true)
                && !isset($this->abstractClassResolvers[$type])
            ) {
                continue;
            }

            $denormalizeTypeMethod->addBody(
                "{$type}Interface::class => \$this->denormalize{$type}(\$data),"
            );

            $knownInterfaces[] = $interface = "$this->namespace\\Types\\Interfaces\\{$type}Interface";
            $phpNamespace->addUse($interface);
        }

        $denormalizeTypeMethod->addBody(
            '        default => $this->resolveAndDenormalizeSubclass($data, $type),
    };
');

        $knownInterfacesConstant = new Constant('KNOWN_INTERFACES')
            ->setValue(array_map(fn (string $type) => new Literal("\\{$type}::class"), $knownInterfaces))
            ->setPrivate()
            ->setType('array');

        $class->setConstants([$knownInterfacesConstant]);

        $resolveAndDenormalizeSubclassMethod = $class->addMethod('resolveAndDenormalizeSubclass');
        $resolveAndDenormalizeSubclassMethod->addParameter('data')->setType(Type::Array);
        $resolveAndDenormalizeSubclassMethod->addParameter('originalType')->setType(Type::String);
        $resolveAndDenormalizeSubclassMethod->setReturnType($this->namespace . '\\Types\\Interfaces\\TypeInterface');
        $resolveAndDenormalizeSubclassMethod->setPrivate();
        $resolveAndDenormalizeSubclassMethod->setBody(<<<'PHP'
            foreach (self::KNOWN_INTERFACES as $interface) {
                if (is_subclass_of($originalType, $interface)) {
                    $this->customTypesMapping[$originalType] = $interface;

                    return $this->denormalizeType($data, $interface);
                }
            }

            throw new \InvalidArgumentException(sprintf('Unknown type %s', $originalType));
        PHP);

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

        $constructor->addPromotedParameter('customTypesMapping')
            ->setType('array')
            ->setDefaultValue([])
            ->addComment(<<<TEXT

            Runtime cache to map concrete classes to their interfaces.
            Example: [MyCustomUpdate::class => UpdateInterface::class]
            
            @var array<class-string, class-string<TypeInterface>>
            TEXT)
            ->setPrivate();

        $class->setMethods(
            array_merge(
                [$constructor],
                $class->getMethods()
            )
        );

        return [$file, $class];
    }

    /**
     * Generates test factories for concrete types using Faker.
     *
     * @return array<string, PhpFile> Array of generated factory files keyed by class name (e.g., 'UserFactory').
     */
    public function generateTestFactories(): array
    {
        $factoryNamespaceStr = $this->namespace . '\\Factories';
        $typesNamespaceStr = $this->namespace . '\\Types';
        $interfacesNamespaceStr = $typesNamespaceStr . '\\Interfaces';

        $generatedFactories = [];

        // 1. Generate AbstractFactory
        $abstractFactoryFile = new PhpFile();
        $abstractFactoryNamespace = $abstractFactoryFile->addNamespace($factoryNamespaceStr);
        $abstractFactoryNamespace->addUse('\Faker\Generator');
        $abstractFactoryNamespace->addUse($this->namespace . '\\Factory');

        $abstractFactoryClass = $abstractFactoryNamespace->addClass('AbstractFactory')
            ->setAbstract();

        $abstractFactoryClass->addProperty('faker')
            ->setPrivate()
            ->setStatic()
            ->setType('\Faker\Generator');

        $fakeMethod = $abstractFactoryClass->addMethod('fake')
            ->setProtected()
            ->setStatic()
            ->setReturnType('\Faker\Generator');

        $fakeMethod->addBody(
            <<<'PHP'
            if (!isset(self::$faker)) {
                self::$faker = \Faker\Factory::create();
            }
            return self::$faker;
            PHP
        );

        $abstractFactoryNamespace->addUse($this->namespace . '\\FactoryInterface');

        $abstractFactoryClass->addProperty('factory')
            ->setPrivate()
            ->setStatic()
            ->setType($this->namespace . '\\FactoryInterface');

        $factoryMethod = $abstractFactoryClass->addMethod('factory')
            ->setProtected()
            ->setStatic()
            ->setReturnType($this->namespace . '\\FactoryInterface');

        $factoryMethod->addBody(
            <<<'PHP'
            if (!isset(self::$factory)) {
                self::$factory = new Factory();
            }
            return self::$factory;
            PHP
        );

        $setFactoryMethod = $abstractFactoryClass->addMethod('setFactory')
            ->setPublic()
            ->setStatic()
            ->setReturnType('void');

        $setFactoryMethod->addParameter('factory')
            ->setType($this->namespace . '\\FactoryInterface');

        $setFactoryMethod->addBody(
            <<<'PHP'
            if (isset(self::$factory)) {
                throw new \RuntimeException('Factory already set');
            }
            
            self::$factory = $factory;
            PHP
        );

        $generatedFactories['AbstractFactory'] = $abstractFactoryFile;

        // 2. Generate Factories for each concrete type
        foreach ($this->schema['types'] as $type) {
            $typeName = $type['name'];

            // Skip abstract types and InputFile for automatic factory generation
            if (in_array($typeName, $this->abstractClasses) || $typeName === 'InputFile') {
                // Consider logging this skip if necessary
                continue;
            }

            // Skip types that don't have fields (like marker interfaces, although unlikely in TG API)
            // Actually, keep them, they might just have an empty constructor, which is fine.
            // if (empty($type['fields'])) {
            //     continue;
            // }

            $factoryClassName = $typeName . 'Factory';
            $factoryFile = new PhpFile();
            $factoryNamespace = $factoryFile->addNamespace($factoryNamespaceStr);

            // Add necessary use statements
            $factoryNamespace->addUse($typesNamespaceStr . '\\' . $typeName); // Concrete class
            $factoryNamespace->addUse($interfacesNamespaceStr . '\\' . $typeName . 'Interface'); // Interface
            $factoryNamespace->addUse($factoryNamespaceStr . '\\AbstractFactory'); // Base factory

            // Add uses for nested type factories
            $nestedFactoryUses = [];
            foreach ($type['fields'] ?? [] as $field) {
                foreach ($field['types'] as $fieldTypeStr) {
                    [$baseType, $isArray] = $this->getBaseTypeAndArrayInfo($fieldTypeStr);
                    if (!Validators::isBuiltinType($baseType) && $baseType !== 'InputFile' && !in_array($baseType, $this->abstractClasses)) {
                        $nestedFactoryClass = $baseType . 'Factory';
                        if ($nestedFactoryClass !== $factoryClassName) { // Avoid self-import
                            $nestedFactoryUses[$baseType] = $factoryNamespaceStr . '\\' . $nestedFactoryClass;
                        }

                        // Also need the interface for type hints
                        $nestedFactoryUses[$baseType . 'Interface'] = $interfacesNamespaceStr . '\\' . $baseType . 'Interface';
                    }
                }
            }

            foreach ($nestedFactoryUses as $alias => $use) {
                // Check if it's already used before adding
                if (!isset($factoryNamespace->getUses()[$alias === $use ? basename(str_replace('\\', '/', $use)) : $alias])) {
                    $factoryNamespace->addUse($use, $alias);
                }
            }

            $factoryClass = $factoryNamespace
                ->addClass($factoryClassName)
                ->setExtends($factoryNamespaceStr . '\\AbstractFactory');

            $makeMethod = $factoryClass->addMethod('make')
                ->setStatic()
                ->setPublic()
                ->setReturnType($interfacesNamespaceStr . '\\' . $typeName . 'Interface');

            $makeMethod->addComment("Creates a new {$typeName} instance with default fake data.");
            $makeMethod->addComment(''); // Empty line

            $constructorParams = []; // To store ['name' => $camelCaseName, 'info' => $field]

            // Add parameters to make() method
            foreach ($type['fields'] ?? [] as $field) {
                $paramName = self::toCamelCase($field['name']);
                $constructorParams[] = ['name' => $paramName, 'info' => $field];

                // Determine parameter type hint (use interface for objects)
                $paramTypes = [];
                $isNullable = $field['optional']; // If the field is optional, the param must be nullable
                $onlyObjectType = null;
                $hasObject = false;
                $hasBuiltin = false;

                foreach ($field['types'] as $fieldTypeStr) {
                    [$baseType, $isArray] = $this->getBaseTypeAndArrayInfo($fieldTypeStr);

                    if ($isArray) {
                        $paramTypes[] = 'array';
                        $hasBuiltin = true; // Treat array as built-in for simplicity here
                    } elseif (!Validators::isBuiltinType($baseType)) {
                        if ($baseType !== 'InputFile') { // Skip InputFile type hint
                            $paramTypes[] = '\\' . $interfacesNamespaceStr . '\\' . $baseType . 'Interface';
                            $onlyObjectType = '\\' . $interfacesNamespaceStr . '\\' . $baseType . 'Interface';
                            $hasObject = true;
                        } else {
                            $paramTypes[] = 'mixed'; // Use mixed for InputFile params
                            $hasBuiltin = true;
                        }
                    } else {
                        $paramTypes[] = $baseType;
                        $hasBuiltin = true;
                    }
                }

                $paramTypes = array_unique($paramTypes);

                // Simplify type hint: if only one object type and no built-ins, use it. Otherwise use mixed or specific built-in.
                $paramTypeHint = null;
                if (count($paramTypes) === 1) {
                    $paramTypeHint = $paramTypes[0];
                } elseif ($hasObject && !$hasBuiltin && count($paramTypes) === 1) {
                    $paramTypeHint = $onlyObjectType;
                } elseif (count($paramTypes) > 1) {
                    $paramTypeHint = implode('|', $paramTypes); // PHP 8 union types
                    if (str_contains($paramTypeHint, 'mixed')) {
                        $paramTypeHint = 'mixed'; // Prefer mixed if it's part of the union
                    }
                } else {
                    $paramTypeHint = 'mixed'; // Fallback
                }

                $parameter = $makeMethod->addParameter($paramName)
                    ->setType($paramTypeHint)
                    ->setNullable($isNullable) // Optional fields are nullable
                    ->setDefaultValue(null);

                $makeMethod->addComment("@param {$paramTypeHint}|null \${$paramName} Optional. " . strip_tags($field['description']));
            }

            // Generate make() method body
            $bodyLines = ["return self::factory()->make{$typeName}("];
            foreach ($constructorParams as $paramData) {
                $paramName = $paramData['name'];
                $field = $paramData['info'];
                $fieldName = $field['name']; // snake_case name for fake value generation hint

                $fakeValueExpression = $this->generateFakeValueExpression($typeName, $fieldName, $field, $factoryNamespaceStr, $interfacesNamespaceStr);

                // Wrap optional fields with `$paramName ?? ...`
                // Handle default values specified in the schema? Let's prioritize Faker.
                // The example factories sometimes do `?? self::fake()->boolean() ? null : ...` for non-required optional fields. Let's implement that.

                if ($field['optional']) {
                    $bodyLines[] = "    {$paramName}: \${$paramName},";
                } else {
                    // Required field: use provided value or generate fake one
                    $bodyLines[] = "    {$paramName}: \${$paramName} ?? {$fakeValueExpression},";
                }
            }

            $bodyLines[] = ');';

            $makeMethod->setBody(implode("\n", $bodyLines));

            $generatedFactories[$factoryClassName] = $factoryFile;
        }

        return $generatedFactories;
    }

    /**
     * Helper to get base type and array status.
     * e.g., "Array<User|Chat>" -> ["User|Chat", true, 1]
     * e.g., "Array<Array<string>>" -> ["string", true, 2]
     * e.g., "int" -> ["int", false, 0].
     */
    private function getBaseTypeAndArrayInfo(string $type): array
    {
        $isArray = false;
        $arrayLevel = 0;
        $baseType = $type;

        while (str_starts_with($baseType, 'Array<') && str_ends_with($baseType, '>')) {
            $isArray = true;
            ++$arrayLevel;
            $baseType = substr($baseType, 6, -1);
        }

        return [$baseType, $isArray, $arrayLevel];
    }

    /**
     * Generates a PHP string representing the Faker call for a given field.
     */
    private function generateFakeValueExpression(
        string $containingTypeName,
        string $fieldName, // snake_case
        array $fieldInfo,
        string $factoryNamespace,
        string $interfacesNamespace,
    ): string {
        $types = $fieldInfo['types'];
        $description = strip_tags($fieldInfo['description']);
        $isOptional = $fieldInfo['optional'];

        // Prioritize the first type for simplicity, but consider others for hints
        $mainTypeStr = $types[0];
        [$baseType, $isArray, $arrayLevel] = $this->getBaseTypeAndArrayInfo($mainTypeStr);

        // Handle simple array case first
        if ($isArray) {
            // Determine inner type for factory call or basic faker generation
            [$innerBaseType, $innerIsArray] = $this->getBaseTypeAndArrayInfo($baseType); // Handles nested arrays

            if (!Validators::isBuiltinType($innerBaseType) && $innerBaseType !== 'InputFile' && !in_array($innerBaseType, $this->abstractClasses)) {
                // Array of objects: Call the corresponding factory
                $innerFactoryName = $innerBaseType . 'Factory';
                $factoryCall = "{$innerFactoryName}::make()";

                // Handle nested arrays of objects
                for ($i = 1; $i < $arrayLevel; ++$i) {
                    $factoryCall = "array_map(fn() => {$factoryCall}, range(0, self::fake()->numberBetween(0, 1)))"; // Nested arrays smaller
                }

                // Generate 0 to 3 items
                return "array_map(fn() => {$factoryCall}, range(0, self::fake()->numberBetween(0, 2)))";
            }

            // Array of built-in types or InputFile
            switch ($innerBaseType) {
                case 'int':
                    return 'self::fake()->randomElements(range(1, 100), self::fake()->numberBetween(1, 5))';
                case 'string':
                    // Check field name for hints
                    if (str_contains($fieldName, 'emoji')) {
                        return 'self::fake()->randomElements(["👍", "❤️", "😂", "🚀"], self::fake()->numberBetween(1, 3))';
                    }

                    if (str_contains($fieldName, 'entities')) { // Special case for Message entities
                        return '[]'; // Default to empty array, too complex to fake well automatically
                    }

                    if (str_contains($fieldName, 'photo')) { // e.g. photo sizes
                        return '[]'; // Default to empty array
                    }

                    return 'self::fake()->words(self::fake()->numberBetween(1, 5))';
                case 'bool':
                    return '[self::fake()->boolean(), self::fake()->boolean()]';
                case 'float':
                    return '[self::fake()->randomFloat(2), self::fake()->randomFloat(2)]';
                default: // InputFile, mixed, etc.
                    return '[]'; // Sensible default for arrays of complex/unknown types
            }
        }

        // Handle non-array types
        if (!Validators::isBuiltinType($baseType)) {
            // Object type
            if ($baseType === 'InputFile') {
                // InputFile needs special handling - often represented as string ('attach://<name>') or resource
                return "'attach://' . self::fake()->word . '.txt'";
            }

            if (in_array($baseType, $this->abstractClasses)) {
                // Cannot directly instantiate abstract classes via factory easily
                // Maybe return null if optional, otherwise throw error or return placeholder?
                // For now, let's return a string placeholder, assuming it might be optional or handled manually
                return "'[Abstract type: {$baseType}]'";
            }

            // Concrete object type: Call its factory
            $factoryName = $baseType . 'Factory';

            return "{$factoryName}::make()";
        }

        // Handle built-in types with Faker, using field name hints
        switch ($baseType) {
            case 'int':
            case 'Integer': // Just in case schema uses this sometimes
                if ($fieldName === 'date' || str_ends_with($fieldName, '_date') || $fieldName === 'until_date') {
                    return 'self::fake()->unixTime()';
                }

                if ($fieldName === 'id' || str_ends_with($fieldName, '_id')) {
                    // Generate positive IDs, maybe higher numbers
                    return 'self::fake()->numberBetween(100000, 999999999)';
                }

                if ($fieldName === 'offset' || $fieldName === 'length') {
                    return 'self::fake()->numberBetween(0, 100)';
                }

                if ($fieldName === 'width' || $fieldName === 'height' || $fieldName === 'duration') {
                    return 'self::fake()->numberBetween(10, 1000)';
                }

                if ($fieldName === 'limit') {
                    return 'self::fake()->numberBetween(1, 100)';
                }

                return 'self::fake()->randomNumber()';

            case 'string':
            case 'String':
                if ($fieldName === 'type' || $fieldName === 'status') {
                    // Try to find enum values in description (simple check)
                    if (preg_match('/must be one of (.+)/i', $description, $matches)) {
                        $options = array_map('trim', explode(',', str_replace(['“', '”', '`', "'"], '', $matches[1])));
                        $options = array_filter($options);
                        if (count($options) > 0) {
                            // Need to format as PHP array string
                            $optionsStr = implode(', ', array_map(fn ($s) => "'$s'", $options));

                            return "self::fake()->randomElement([{$optionsStr}])";
                        }
                    }

                    // Fallback for common types/statuses
                    if ($fieldName === 'type' && $containingTypeName === 'Chat') {
                        return "self::fake()->randomElement(['private', 'group', 'supergroup', 'channel'])";
                    }

                    if ($fieldName === 'status' && str_starts_with($containingTypeName, 'ChatMember')) {
                        return "self::fake()->randomElement(['creator', 'administrator', 'member', 'restricted', 'left', 'kicked'])";
                    }

                    if ($fieldName === 'type' && $containingTypeName === 'MessageEntity') {
                        return "self::fake()->randomElement(['mention', 'hashtag', 'cashtag', 'bot_command', 'url', 'email', 'phone_number', 'bold', 'italic', 'underline', 'strikethrough', 'code', 'pre', 'text_link', 'text_mention', 'custom_emoji'])";
                    }

                    return 'self::fake()->word()'; // Fallback
                }

                if ($fieldName === 'id' || str_ends_with($fieldName, '_id')) { // String IDs like custom_emoji_id
                    return 'self::fake()->bothify(\'?#?#?#?#?#?#?#???\')';
                }

                if ($fieldName === 'username') {
                    return 'self::fake()->userName()';
                }

                if ($fieldName === 'first_name') {
                    return 'self::fake()->firstName()';
                }

                if ($fieldName === 'last_name') {
                    return 'self::fake()->lastName()';
                }

                if ($fieldName === 'title') {
                    return 'self::fake()->sentence(3)';
                }

                if ($fieldName === 'text' || $fieldName === 'caption' || $fieldName === 'description' || $fieldName === 'query') {
                    return 'self::fake()->sentence()';
                }

                if ($fieldName === 'url' || str_ends_with($fieldName, '_url')) {
                    return 'self::fake()->url()';
                }

                if ($fieldName === 'language' || $fieldName === 'language_code') {
                    return 'self::fake()->languageCode()';
                }

                if ($fieldName === 'phone_number') {
                    return 'self::fake()->phoneNumber()';
                }

                if ($fieldName === 'email') {
                    return 'self::fake()->email()';
                }

                if (str_ends_with($fieldName, '_hash') || $fieldName === 'file_unique_id') {
                    return 'self::fake()->sha1()';
                }

                if ($fieldName === 'file_id') {
                    return 'self::fake()->uuid()'; // File IDs are usually long strings
                }

                if ($fieldName === 'parse_mode') {
                    return "self::fake()->randomElement(['MarkdownV2', 'HTML', 'Markdown'])";
                }

                return 'self::fake()->text(50)'; // Default string

            case 'bool':
            case 'Boolean':
            case 'True': // Schema sometimes uses True
                // Use default from schema if 'True'? Already handled by Field parser
                return 'self::fake()->boolean()';

            case 'float':
            case 'Float':
                if ($fieldName === 'latitude') {
                    return 'self::fake()->latitude()';
                }

                if ($fieldName === 'longitude') {
                    return 'self::fake()->longitude()';
                }

                return 'self::fake()->randomFloat()';

            default: // Should not happen for built-ins
                return "'[UNKNOWN BUILTIN: {$baseType}]'";
        }
    }
}
