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
use Shanginn\TelegramBotApiBindings\Types\TypeInterface;
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

        $this->getExtendedTypes();
        $this->namespace = $namespace;
    }

    /**
     * Builds the abstract and the extended class lists.
     */
    private function getExtendedTypes(): void
    {
        foreach ($this->schema['types'] as $type) {
            if (!empty($type['extended_by'])) {
                $this->abstractClasses[] = $type['name'];
                foreach ($type['extended_by'] as $extendedType) {
                    $this->extendedClasses[$extendedType] = $type['name'];
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
        foreach ($fieldTypes as $fieldType) {
            if (str_starts_with($fieldType, 'Array')) {
                $types[] = 'array';
                $comments[] = str_replace('Array', 'array', $fieldType);

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

            $isAbstract = in_array($type['name'], $this->abstractClasses);

            if ($isAbstract) {
                $typeClass->setAbstract();
            }

            if (array_key_exists($type['name'], $this->extendedClasses)) {
                $typeClass->setExtends($namespace . '\\' . $this->extendedClasses[$type['name']]);
            } else {
                $typeClass->addImplement($namespace . '\\TypeInterface');
            }

            $typeClass->setComment($type['description'] ?? null);

            foreach ($type['fields'] as $field) {
                ['types' => $fieldType, 'comments' => $fieldComment] = $this->parseFieldTypes(
                    $field['types'],
                    $phpNamespace
                );

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

            $denormalizers[$type['name']] = $this->generateDeserializeTypeMethod(
                array_map(fn ($a) => $a[0], $params),
                $type['name']
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

    private function generateDeserializeTypeMethod(array $params, string $type): Method
    {
        $fromResponseResultMethod = new Method(sprintf('denormalize%s', $type));
        $fromResponseResultMethod->setPublic();
        $fromResponseResultMethod->setReturnType($this->namespace . '\\Types\\' . $type);
        $fromResponseResultMethod
            ->addParameter('data')
            ->setType(Type::Array);

        if (in_array($type, $this->abstractClasses)) {
            $fromResponseResultMethod->addBody(
                sprintf(
                    'throw new \RuntimeException("class %s is abstract and not yet implemented");',
                    $type
                )
            );

            return $fromResponseResultMethod;
        }

        if (count($params) === 0) {
            $fromResponseResultMethod->addBody(
                sprintf('return new %s();', $type)
            );

            return $fromResponseResultMethod;
        }

        $requiredParams = array_filter($params, fn ($param) => !$param->hasDefaultValue());

        if (count($requiredParams) > 0) {
            $fromResponseResultMethod->addBody('$requiredFields = [');
            foreach ($requiredParams as $param) {
                $fromResponseResultMethod->addBody(sprintf(
                    '    \'%s\',',
                    Str::snake($param->getName())
                ));
            }

            $fromResponseResultMethod->addBody("];\n");

            $fromResponseResultMethod->addBody(
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

        $fromResponseResultMethod->addBody(sprintf('return new %s(', $type));

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

            $value = sprintf('$data[\'%s\']', Str::snake($param->getName()));

            $paramType = $param->getType();
            $paramTypeBase = explode('\\', $paramType);
            $paramTypeBase = $paramTypeBase[count($paramTypeBase) - 1];
            $paramTypeBase = explode('|', $paramTypeBase);
            $paramTypeBase = $paramTypeBase[0];

            if (!Validators::isBuiltinType($paramTypeBase)) {
                if ($defaultValue !== null) {
                    $value = <<<VALUE
                    ($value ?? null) !== null
                            ? \$this->denormalize{$paramTypeBase}($value)
                            : null
                    VALUE;

                    $defaultValue = null;
                } else {
                    $value = sprintf(
                        '$this->denormalize%s($data[\'%s\'])',
                        $paramTypeBase,
                        Str::snake($param->getName())
                    );
                }
            }

            $fromResponseResultMethod->addBody(sprintf(
                '    %s: %s%s,',
                $param->getName(),
                $value,
                $defaultValue !== null ? sprintf(' ?? %s', $defaultValue) : ''
            ));
        }

        $fromResponseResultMethod->addBody(');');

        return $fromResponseResultMethod;
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

        // public function denormalize(array $data, array $types): mixed
        //    {
        //        foreach ($types as $type) {
        //            if (class_exists($type) && is_subclass_of($type, TypeInterface::class)) {
        //                return $this->denormalizeType($data, $type);
        //            } elseif ($type === 'bool') {
        //                return (bool) $data;
        //            } elseif ($type === 'int') {
        //                return (int) $data;
        //            } elseif ($type === 'string') {
        //                return (string) $data;
        //            } elseif (str_starts_with($type, 'array<')) {
        //                preg_match('/array<(.+)>/', $type, $matches);
        //                $innerType = $matches[1];
        //                $resultArray = [];
        //
        //                foreach ($data as $item) {
        //                    $resultArray[] = $this->denormalize($item, [$innerType]);
        //                }
        //
        //                return $resultArray;
        //            }
        //        }
        //
        //        throw new \UnexpectedValueException(sprintf('Failed to decode response to any of the expected types: %s', implode(', ', $types)));
        //    }
        //
        //    public function deserialize(string $data, array $types): mixed
        //    {
        //        $response = json_decode($data, true);
        //
        //        return $this->denormalize($response, $types);
        //    }

        $deserializeMethod = $class->addMethod('deserialize');
        $deserializeMethod->addParameter('data')->setType(Type::String);
        $deserializeMethod->addParameter('types')->setType(Type::Array);
        $deserializeMethod->setReturnType('mixed');
        $deserializeMethod->setPublic();
        $deserializeMethod->setBody('
            $response = json_decode($data, true);
            
            return $this->denormalize($response, $types);
        ');

        $denormalizeMethod = $class->addMethod('denormalize');
        $denormalizeMethod->addParameter('data')->setType(Type::Array);
        $denormalizeMethod->addParameter('types')->setType(Type::Array);
        $denormalizeMethod->setReturnType('mixed');
        $denormalizeMethod->setPublic();
        $denormalizeMethod->setBody(<<<'BODY'
            foreach ($types as $type) {
                if (class_exists($type) && is_subclass_of($type, TypeInterface::class)) {
                    return $this->denormalizeType($data, $type);
                } elseif ($type === 'bool') {
                    return (bool) $data;
                } elseif ($type === 'int') {
                    return (int) $data;
                } elseif ($type === 'string') {
                    return (string) $data;
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
                    $result[$snakeKey] = json_encode($this->normalize($value));
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
