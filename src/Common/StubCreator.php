<?php

/** @noinspection PhpArrayShapeAttributeCanBeAddedInspection */

/** @noinspection PhpInternalEntityUsedInspection */

namespace TgScraper\Common;

use Nette\PhpGenerator\Helpers;
use Nette\PhpGenerator\InterfaceType;
use Nette\PhpGenerator\PhpFile;
use Nette\PhpGenerator\PhpNamespace;
use Nette\PhpGenerator\PromotedParameter;
use Nette\PhpGenerator\Type;
use Nette\Utils\Validators;
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
     * @return PhpFile[]
     */
    private function generateTypes(): array
    {
        $namespace = $this->namespace . '\\Types';
        $types = $this->generateDefaultTypes($namespace);

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
        }

        return $types;
    }

    /**
     * @return array<string,PhpFile>
     */
    private function generateApi(): array
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
        ];
    }

    /**
     * @return array{0: PhpFile, 1: PhpNamespace}
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
        $method->addParameter('data')->setType(Type::Mixed);
        $method->setReturnType(Type::String);

        $method = $interface->addMethod('deserialize')->setPublic();
        $method->addParameter('data')->setType(Type::String);
        $method->addParameter('types')->setType(Type::Array);
        $method->setReturnType(Type::Mixed);

        return [$file, $interface];
    }

    /**
     * @return array{
     *     types: PhpFile[],
     *     files: array<string,PhpFile>,
     * }
     */
    public function generateCode(): array
    {
        return [
            'types' => $this->generateTypes(),
            'files' => $this->generateApi(),
        ];
    }
}
