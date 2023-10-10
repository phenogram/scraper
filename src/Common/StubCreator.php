<?php
/** @noinspection PhpArrayShapeAttributeCanBeAddedInspection */

/** @noinspection PhpInternalEntityUsedInspection */


namespace TgScraper\Common;

use InvalidArgumentException;
use Nette\PhpGenerator\Helpers;
use Nette\PhpGenerator\PhpFile;
use Nette\PhpGenerator\PhpNamespace;
use Nette\PhpGenerator\PromotedParameter;
use Nette\PhpGenerator\Type;
use TgScraper\TgScraper;

/**
 * Class StubCreator
 * @package TgScraper\Common
 */
class StubCreator
{


    /**
     * @var string
     */
    private string $namespace;
    /**
     * @var array
     */
    private array $abstractClasses = [];
    /**
     * @var array
     */
    private array $extendedClasses = [];

    /**
     * StubCreator constructor.
     * @param array $schema
     * @param string $namespace
     * @throws InvalidArgumentException
     */
    public function __construct(private array $schema, string $namespace = '')
    {
        if (str_ends_with($namespace, '\\')) {
            $namespace = substr($namespace, 0, -1);
        }
        if (!empty($namespace)) {
            if (!Helpers::isNamespaceIdentifier($namespace)) {
                throw new InvalidArgumentException('Namespace invalid');
            }
        }
        if (!TgScraper::validateSchema($this->schema)) {
            throw new InvalidArgumentException('Schema invalid');
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

    /**
     * @param string $str
     * @return string
     */
    private static function toCamelCase(string $str): string
    {
        return lcfirst(str_replace(' ', '', ucwords(str_replace('_', ' ', $str))));
    }

    /**
     * @param array $fieldTypes
     * @param PhpNamespace $phpNamespace
     * @return array
     */
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
            'comments' => $comments
        ];
    }

    /**
     * @param array $apiTypes
     * @param PhpNamespace $phpNamespace
     * @return array
     */
    private function parseApiFieldTypes(array $apiTypes, PhpNamespace $phpNamespace): array
    {
        $types = [];
        $comments = [];
        foreach ($apiTypes as $apiType) {
            $comments[] = $apiType;
            if (str_starts_with($apiType, 'Array')) {
                $types[] = 'array';
                $text = $apiType;
                while (preg_match('/Array<(.+)>/', $text, $matches) === 1) {
                    $text = $matches[1];
                }
                $subTypes = explode('|', $text);
                foreach ($subTypes as $subType) {
                    if (ucfirst($subType) == $subType) {
                        $subType = $this->namespace . '\\Types\\' . $subType;
                        $phpNamespace->addUse($subType);
                    }
                }
                continue;
            }
            if (ucfirst($apiType) == $apiType) {
                $apiType = $this->namespace . '\\Types\\' . $apiType;
                $phpNamespace->addUse($apiType);
            }
            $types[] = $apiType;
        }
        $comments = empty($comments) ? '' : sprintf('@param %s', implode('|', $comments));
        return [
            'types' => implode('|', $types),
            'comments' => $comments
        ];
    }

    /**
     * @param string $namespace
     * @return PhpFile[]
     */
    private function generateDefaultTypes(string $namespace): array
    {
        $interfaceFile = new PhpFile;
        $interfaceNamespace = $interfaceFile->addNamespace($namespace);
        $interfaceNamespace->addInterface('TypeInterface');
        $responseFile = new PhpFile;
        $responseNamespace = $responseFile->addNamespace($namespace);
        $responseNamespace->addUse('stdClass');
        $response = $responseNamespace->addClass('Response');
        $response->addProperty('ok')
            ->setPublic()
            ->setType(Type::BOOL);
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
            'TypeInterface' => $interfaceFile
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
            $file = new PhpFile;
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

            foreach ($type['fields'] as $field) {
                ['types' => $fieldType, 'comments' => $fieldComment] = $this->parseFieldTypes(
                    $field['types'],
                    $phpNamespace
                );

                $fieldName = self::toCamelCase($field['name']);
                $param = (new PromotedParameter($fieldName))->setType($fieldType);

                if (isset($field['default'])) {
                    $param->setDefaultValue($field['default']);
                }

                if ($field['optional']) {
                    $param->setNullable();
                    if (!$param->hasDefaultValue()) {
                        $param->setDefaultValue(null);
                    }

                    if ($fieldComment !== '') {
                        $fieldComment .= '|null';
                    }
                }

                if ($fieldComment !== '') {
                    $fieldComment .= sprintf(' $%s %s', $fieldName, $field['description']);
                }

                $params[] = [$param, $fieldComment];
            }

            usort($params, function($a, $b) {
                $aHasDefault = $a[0]->hasDefaultValue();
                $bHasDefault = $b[0]->hasDefaultValue();

                if ($aHasDefault === $bHasDefault) {
                    return 0;
                }

                return $aHasDefault ? 1 : -1;
            });

            $constructor->setParameters(array_map(fn($a) => $a[0], $params));
            $constructor->setComment(implode("\n", array_map(fn($a) => $a[1], $params)));

            $types[$type['name']] = $file;
        }

        return $types;
    }


    /**
     * @return PhpFile
     */
    private function generateApi(): PhpFile
    {
        $file = new PhpFile;
        $file->addComment('@noinspection PhpUnused');
        $file->addComment('@noinspection PhpUnusedParameterInspection');
        $phpNamespace = $file->addNamespace($this->namespace);
        $apiClass = $phpNamespace->addTrait('API');
        $sendRequest = $apiClass->addMethod('sendRequest')
            ->setPublic()
            ->setAbstract()
            ->setReturnType(Type::MIXED);
        $sendRequest->addParameter('method')
            ->setType(Type::STRING);
        $sendRequest->addParameter('args')
            ->setType(Type::ARRAY);
        foreach ($this->schema['methods'] as $method) {
            $function = $apiClass->addMethod($method['name'])
                ->setPublic()
                ->addBody('$args = get_defined_vars();')
                ->addBody('return $this->sendRequest(__FUNCTION__, $args);');
            $function->addComment($method['description']);
            $fields = $method['fields'];
            usort(
                $fields,
                function ($a, $b) {
                    return $a['optional'] - $b['optional'];
                }
            );
            foreach ($fields as $field) {
                ['types' => $types, 'comments' => $comment] = $this->parseApiFieldTypes($field['types'], $phpNamespace);
                $fieldName = self::toCamelCase($field['name']);
                $parameter = $function->addParameter($fieldName)
                    ->setType($types);
                $default = $field['default'] ?? null;
                if (!empty($default) and (!is_string($default) or lcfirst($default) == $default)) {
                    $parameter->setDefaultValue($default);
                }
                if ($field['optional']) {
                    $parameter->setNullable();
                    if (!$parameter->hasDefaultValue()) {
                        $parameter->setDefaultValue(null);
                    }
                    $comment .= '|null';
                }
                $comment .= sprintf(' $%s %s', $fieldName, $field['description']);
                $function->addComment($comment);
            }
            ['types' => $returnTypes, 'comments' => $returnComment] = $this->parseApiFieldTypes(
                $method['return_types'],
                $phpNamespace
            );
            $function->setReturnType($returnTypes);
            $function->addComment(str_replace('param', 'return', $returnComment));
        }
        return $file;
    }

    /**
     * @return array{types: PhpFile[], api: PhpFile}
     */
    public function generateCode(): array
    {
        return [
            'types' => $this->generateTypes(),
            'api' => $this->generateApi()
        ];
    }

}