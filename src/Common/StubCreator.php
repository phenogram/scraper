<?php

/** @noinspection PhpArrayShapeAttributeCanBeAddedInspection */

/** @noinspection PhpInternalEntityUsedInspection */

namespace TgScraper\Common;

use Illuminate\Support\Str;
use Nette\PhpGenerator\ClassType;
use Nette\PhpGenerator\Helpers;
use Nette\PhpGenerator\InterfaceType;
use Nette\PhpGenerator\Method;
use Nette\PhpGenerator\Parameter;
use Nette\PhpGenerator\PhpFile;
use Nette\PhpGenerator\PhpNamespace;
use Nette\PhpGenerator\PromotedParameter;
use Nette\PhpGenerator\Type;
use Nette\Utils\Validators;
use TgScraper\Common\AbstractClassResolvers\AbstractClassResolverInterface;
use TgScraper\TgScraper;

/**
 * Class StubCreator.
 */
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
     * @var array<AbstractClassResolverInterface>
     */
    private array $abstractClassResolvers = [];

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

                foreach ($type['extended_by'] as $extendedType) {
                    $this->extendedClasses[$extendedType] = $typeName;
                }

                if (class_exists($abstractResolversNamespace . '\\' . $typeName)) {
                    $this->abstractClassResolvers[$typeName] = $abstractResolversNamespace . '\\' . $typeName;
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
                $types[] = 'array';
                $comments[] = str_replace('Array', 'array', $fieldType);

                $innerType = explode('<', $fieldType);

                $arrayLevels = 0;
                $innerType = $innerType[count($innerType) - 1];

                while (str_ends_with($innerType, '>')) {
                    ++$arrayLevels;
                    $innerType = substr($innerType, 0, -1);
                }

                $arrayType = [$arrayLevels, $innerType];

                continue;
            }

            $comments[] = $fieldType;

            if (ucfirst($fieldType) == $fieldType) {
                $fieldType = $phpNamespace->getName() . '\\' . $fieldType;
            }

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
        PhpNamespace $interfaceNamespace
    ): array {
        $types = [];
        $comments = [];

        foreach ($apiTypes as $apiType) {
            if (str_starts_with($apiType, 'Array')) {
                $types[] = 'array';

                $comments[] = str_replace('Array', 'array', $apiType);

                $text = $apiType;

                while (preg_match('/Array<(.+)>/', $text, $matches) === 1) {
                    $text = $matches[1];
                }

                $subTypes = explode('|', $text);
                foreach ($subTypes as $subType) {
                    if (ucfirst($subType) == $subType) {
                        $subType = $this->namespace . '\\Types\\' . $subType;
                        $phpNamespace->addUse($subType);
                        if ($interfaceNamespace !== null) {
                            $interfaceNamespace->addUse($subType);
                        }
                    }
                }

                continue;
            }

            $comments[] = $apiType;

            if (ucfirst($apiType) == $apiType) {
                $apiType = $this->namespace . '\\Types\\' . $apiType;
                $phpNamespace->addUse($apiType);

                if ($interfaceNamespace !== null) {
                    $interfaceNamespace->addUse($apiType);
                }
            }

            $types[] = $apiType;
        }

        $comments = empty($comments) ? '' : sprintf('@param %s', implode('|', $comments));

        return [
            'types' => implode('|', $types),
            'comments' => $comments,
        ];
    }

    /**
     * @return PhpFile[]
     */
    private function generateDefaultTypes(string $namespace): array
    {
        $interfaceFile = new PhpFile();

        $interfaceNamespace = $interfaceFile->addNamespace($namespace);
        $interfaceNamespace->addInterface('TypeInterface');

        $responseFile = new PhpFile();
        $responseNamespace = $responseFile->addNamespace($namespace);
        $responseNamespace->addUse('stdClass');
        $response = $responseNamespace->addClass('Response');
        $response->addProperty('ok')
            ->setPublic()
            ->setType(Type::Bool);
        $response->addProperty('result')
            ->setPublic()
            ->setType(sprintf('stdClass|%s\\TypeInterface|array|int|string|bool', $namespace))
            ->setNullable()
            ->setValue(null);
        $response->addProperty('errorCode')
            ->setPublic()
            ->setType(Type::INT)
            ->setNullable()
            ->setValue(null);
        $response->addProperty('description')
            ->setPublic()
            ->setType(Type::STRING)
            ->setNullable()
            ->setValue(null);
        $response->addProperty('parameters')
            ->setPublic()
            ->setType(sprintf('stdClass|%s\\ResponseParameters', $namespace))
            ->setNullable()
            ->setValue(null);
        $response->addImplement($namespace . '\\TypeInterface');

        return [
            'Response' => $responseFile,
            'TypeInterface' => $interfaceFile,
        ];
    }

    /**
     * @return array{0: PhpFile[], 1: array<string,Method>}
     */
    private function generateTypes(): array
    {
        $namespace = $this->namespace . '\\Types';
        $types = $this->generateDefaultTypes($namespace);

        $denormalizers = [];

        foreach ($this->schema['types'] as $type) {
            $file = new PhpFile();
            $phpNamespace = $file->addNamespace($namespace);
            $typeClass = $phpNamespace->addClass($type['name']);

            $constructor = $typeClass->addMethod('__construct');
            $params = [];

            if (in_array($type['name'], $this->abstractClasses)) {
                $typeClass->setAbstract();
            }

            if (array_key_exists($type['name'], $this->extendedClasses)) {
                $typeClass->setExtends($namespace . '\\' . $this->extendedClasses[$type['name']]);
            } else {
                $typeClass->addImplement($namespace . '\\TypeInterface');
            }

            $typeClass->setComment($type['description'] ?? null);

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
                $param = (new PromotedParameter($fieldName))->setType($fieldType);

                if ($field['optional']) {
                    $param->setNullable();
                    $param->setDefaultValue(null);

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
                }

                $params[] = [$param, $fieldComment];
            }

            usort($params, function ($a, $b) {
                return (int) $a[0]->hasDefaultValue() - (int) $b[0]->hasDefaultValue();
            });

            $constructor->setParameters(array_map(fn ($a) => $a[0], $params));
            $constructor->setComment(implode("\n", array_map(fn ($a) => $a[1], $params)));

            $types[$type['name']] = $file;

            $denormalizers[$type['name']] = $this->generateDenormalizeTypeMethod(
                array_map(fn ($a) => $a[0], $params),
                $type['name'],
                $arrayTypes
            );
        }

        return [$types, $denormalizers];
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
        $phpNamespace->addUse('React\Promise\PromiseInterface');
        $apiClass = $phpNamespace->addClass('TelegramBotApi');

        $apiInterfaceNamespace = $apiInterfaceFile->addNamespace($this->namespace);
        $apiInterfaceNamespace->addUse('React\Promise\PromiseInterface');
        $apiInterface = $apiInterfaceNamespace->addInterface('TelegramBotApiInterface');

        $apiClass->addImplement($this->namespace . '\\' . $apiInterface->getName());

        [$clientInterfaceFile, $clientInterface] = $this->generateTelegramBotApiClientInterface();
        [$serializerInterfaceFile, $serializerInterface] = $this->generateTelegramBotApiSerializerInterface();
        [$serializerFile, $serializer] = $this->generateTelegramBotApiSerializer($denormalizers);

        $constructor = $apiClass->addMethod('__construct');
        $constructor
            ->addPromotedParameter('client')
            ->setType($this->namespace . '\\' . $clientInterface->getName())
            ->setVisibility('protected');

        $constructor
            ->addPromotedParameter('serializer')
            ->setType($this->namespace . '\\' . $serializerInterface->getName())
            ->setVisibility('protected');

        $doRequestMethod = $apiClass
            ->addMethod('doRequest')
            ->setPrivate();

        $doRequestMethod
            ->addParameter('method')
            ->setType(Type::String);

        $doRequestMethod
            ->addParameter('args')
            ->setType(Type::Array);

        $doRequestMethod
            ->addParameter('returnTypes')
            ->setType(Type::Array);

        $doRequestMethod
            ->setReturnType('React\Promise\PromiseInterface');

        $doRequestMethod
            ->addBody(
                <<<'BODY'
                    
                    return $this->client
                        ->sendRequest(
                            $method,
                            $this->serializer->serialize($args)
                        )
                        ->then(fn ($response) => $this->serializer->deserialize(
                            $response,
                            $returnTypes
                        ));
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
                    'comments' => $comment
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
                'types' => $returnTypes,
                'comments' => $returnComment
            ] = $this->parseApiFieldTypes($method['return_types'], $phpNamespace, $apiInterfaceNamespace);

            $expectedReturnTypes = array_map(
                function (string $type) {
                    if (Validators::isBuiltinType($type)) {
                        return $type;
                    }

                    if (str_starts_with($type, 'array')) {
                        // find the last type in the array
                        $realType = explode('<', $type);
                        $realType = $realType[count($realType) - 1];
                        $realType = explode('>', $realType)[0];

                        if (Validators::isBuiltinType($realType)) {
                            return $type;
                        }

                        return str_replace(
                            $realType,
                            $this->namespace . '\\Types\\' . $realType,
                            $type
                        );
                    }

                    return $this->namespace . '\\Types\\' . $type;
                },
                explode(
                    '|',
                    str_replace('@param ', '', $returnComment)
                )
            );

            $function
                ->addBody('return $this->doRequest(
    __FUNCTION__,
    get_defined_vars(),
    ["' . implode('", "', $expectedReturnTypes) . '"]
);
');

            $returnComment = sprintf(
                '@return PromiseInterface<%s>',
                str_replace('@param ', '', $returnComment)
            );

            $function
                ->setReturnType('React\Promise\PromiseInterface')
                ->addComment(str_replace('param', 'return', $returnComment));

            $interfaceFunction
                ->setReturnType('React\Promise\PromiseInterface')
                ->addComment(str_replace('param', 'return', $returnComment));
        }

        return [
            'TelegramBotApi' => $file,
            'TelegramBotApiInterface' => $apiInterfaceFile,
            'TelegramBotApiClientInterface' => $clientInterfaceFile,
            'TelegramBotApiSerializerInterface' => $serializerInterfaceFile,
            'TelegramBotApiSerializer' => $serializerFile,
        ];
    }

    /**
     * @param array<string,array{0: int, 1: string}> $arrayTypes
     */
    private function generateDenormalizeTypeMethod(
        array $params,
        string $type,
        array $arrayTypes
    ): Method {
        $denormalizeTypeMethod = new Method(sprintf('denormalize%s', $type));
        $denormalizeTypeMethod->setPublic();
        $denormalizeTypeMethod->setReturnType($this->namespace . '\\Types\\' . $type);
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
                sprintf('return new %s();', $type)
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

        $denormalizeTypeMethod->addBody(sprintf('return new %s(', $type));

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

            $handlingArray = $paramTypeBase === 'array';

            if ($handlingArray) {
                [$arrayLevel, $paramTypeBase] = $arrayTypes[$snakeParamName];
            }

            if (!Validators::isBuiltinType($paramTypeBase)) {
                if ($defaultValue !== null) {
                    if ($handlingArray) {
                        if ($arrayLevel === 1) {
                            $value = <<<VALUE
                            ($value ?? null) !== null
                                    ? array_map(fn (array \$item) => \$this->denormalize{$paramTypeBase}(\$item), $value)
                                    : null
                            VALUE;
                        } elseif ($arrayLevel === 2) {
                            $value = <<<VALUE
                            ($value ?? null) !== null
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
                        ($value ?? null) !== null
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
    private function generateTelegramBotApiClientInterface(): array
    {
        $file = new PhpFile();

        $phpNamespace = $file->addNamespace($this->namespace);
        $phpNamespace->addUse('React\Promise\PromiseInterface');

        $interface = $phpNamespace->addInterface('TelegramBotApiClientInterface');

        $method = $interface->addMethod('sendRequest')->setPublic();

        $method->addParameter('method')->setType(Type::String);
        $method->addParameter('json')->setType(Type::String);
        $method->setReturnType('React\Promise\PromiseInterface');

        return [$file, $interface];
    }

    /**
     * @return array{0: PhpFile, 1: InterfaceType}
     */
    private function generateTelegramBotApiSerializerInterface(): array
    {
        $file = new PhpFile();

        $phpNamespace = $file->addNamespace($this->namespace);

        $interface = $phpNamespace->addInterface('TelegramBotApiSerializerInterface');

        $method = $interface->addMethod('serialize')->setPublic();
        $method->addParameter('data')->setType(Type::Array);
        $method->setReturnType(Type::String);

        $method = $interface->addMethod('deserialize')->setPublic();
        $method->addParameter('data')->setType(Type::String);
        $method->addParameter('types')->setType(Type::Array);
        $method->setReturnType(Type::Mixed);

        return [$file, $interface];
    }

    /**
     * @param array<string, Method> $denormalizers
     *
     * @return array{0: PhpFile, 1: ClassType}
     */
    private function generateTelegramBotApiSerializer(array $denormalizers): array
    {
        $file = new PhpFile();

        $phpNamespace = $file->addNamespace($this->namespace);
        $phpNamespace->addUse($this->namespace . '\\Types\\TypeInterface');

        $class = $phpNamespace->addClass('TelegramBotApiSerializer');
        $class->addImplement($this->namespace . '\\TelegramBotApiSerializerInterface');

        $serializeMethod = $class->addMethod('serialize');
        $serializeMethod->addParameter('data')->setType(Type::Array);
        $serializeMethod->setReturnType(Type::String);
        $serializeMethod->setPublic();
        $serializeMethod->setBody('return json_encode($this->normalize($data));');

        $deserializeMethod = $class->addMethod('deserialize');
        $deserializeMethod->addParameter('data')->setType(Type::String);
        $deserializeMethod->addParameter('types')->setType(Type::Array);
        $deserializeMethod->setReturnType('mixed');
        $deserializeMethod->setPublic();
        $deserializeMethod->setBody(<<<'BODY'
            $decoded = json_decode($data, true, 512, JSON_THROW_ON_ERROR);

            return is_array($decoded) 
                ? $this->denormalize($decoded, $types)
                : $decoded;
            BODY
        );

        $denormalizeMethod = $class->addMethod('denormalize');
        $denormalizeMethod->addParameter('data')->setType(Type::Array);
        $denormalizeMethod->addParameter('types')->setType(Type::Array);
        $denormalizeMethod->setReturnType('mixed');
        $denormalizeMethod->setPublic();
        $denormalizeMethod->setBody(<<<'BODY'
            foreach ($types as $type) {
                if (class_exists($type) && is_subclass_of($type, TypeInterface::class)) {
                    return $this->denormalizeType($data, $type);
                } elseif (str_starts_with($type, 'array<')) {
                    preg_match('/array<(.+)>/', $type, $matches);
                    $innerType = $matches[1];
                    $resultArray = [];

                    foreach ($data as $item) {
                        $resultArray[] = $this->denormalize($item, [$innerType]);
                    }

                    return $resultArray;
                }
            }

            throw new \UnexpectedValueException(sprintf('Failed to decode response to any of the expected types: %s', implode(', ', $types)));
            BODY
        );

        $denormalizeTypeMethod = $class->addMethod('denormalizeType');
        $denormalizeTypeMethod->addParameter('data')->setType(Type::Array);
        $denormalizeTypeMethod->addParameter('type')->setType(Type::String);
        $denormalizeTypeMethod->setReturnType($this->namespace . '\\Types\\TypeInterface');
        $denormalizeTypeMethod->setPrivate();
        $denormalizeTypeMethod->addBody('
        return match ($type) {
    ');

        foreach ($denormalizers as $type => $denormalizer) {
            if (in_array($type, $this->abstractClasses)) {
                // TODO:
                continue;
            }

            $denormalizeTypeMethod->addBody(sprintf(
                '        %s::class => $this->denormalize%s($data),',
                $type,
                $type
            ));

            $phpNamespace->addUse($this->namespace . '\\Types\\' . $type);
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
        $normalizeMethod->setBody('
            $result = [];
            
            foreach ($data as $key => $value) {
                if (is_null($value)) {
                    continue;
                }
                
                $snakeKey = $this->camelToSnake($key);
                
                if ($value instanceof TypeInterface) {
                    $value = get_object_vars($value);
                }
                
                if (is_array($value)) {
                    $result[$snakeKey] = $this->normalize($value);
                } else {
                    $result[$snakeKey] = $value;
                }
            }
            
            return $result;
        ');

        $camelToSnakeMethod = $class->addMethod('camelToSnake');
        $camelToSnakeMethod->addParameter('input')->setType('string');
        $camelToSnakeMethod->setReturnType('string');
        $camelToSnakeMethod->setPrivate();
        $camelToSnakeMethod->setBody('
            return strtolower(preg_replace(\'/[A-Z]/\', \'_$0\', lcfirst($input)));
        ');

        return [$file, $class];
    }

    /**
     * @return array{
     *     types: PhpFile[],
     *     files: array<string,PhpFile>,
     * }
     */
    public function generateCode(): array
    {
        [$types, $typesDenormalizers] = $this->generateTypes();

        return [
            'types' => $types,
            'files' => $this->generateApi($typesDenormalizers),
        ];
    }
}
