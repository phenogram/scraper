<?php

namespace TgScraper\Parsers;

use JetBrains\PhpStorm\ArrayShape;

/**
 * Class Field.
 */
class Field
{
    /**
     * Parsed types map.
     */
    public const TYPES = [
        'Integer' => 'int',
        'Float' => 'float',
        'String' => 'string',
        'Boolean' => 'bool',
        'True' => 'bool',
        'False' => 'bool',
    ];

    private string $name;

    private array $types;

    private FieldDescription $description;

    private bool $optional;

    private mixed $defaultValue;

    public function __construct(string $name, string $types, bool $optional, string $description)
    {
        $this->name = $name;
        $this->types = $this->parseTypesString($types);
        $this->optional = $optional;
        $this->description = new FieldDescription($description);
    }

    private function parseTypeString(string $type): string
    {
        if ($type == 'True') {
            $this->defaultValue = true;

            return self::TYPES['Boolean'];
        } elseif ($type == 'False') {
            $this->defaultValue = false;

            return self::TYPES['Boolean'];
        }

        $type = trim(str_replace('number', '', $type));

        return trim(str_replace(array_keys(self::TYPES), array_values(self::TYPES), $type));
    }

    private function parseTypesString(string $text): array
    {
        $types = [];
        $parts = explode(' or ', $text);
        foreach ($parts as $part) {
            $part = trim(str_replace(' and', ',', $part));
            $arrays = 0;
            while (stripos($part, 'array of') === 0) {
                $part = substr($part, 9);
                ++$arrays;
            }

            $pieces = explode(',', $part);
            foreach ($pieces as $index => $piece) {
                $pieces[$index] = $this->parseTypeString($piece);
            }

            $type = implode('|', $pieces);
            for ($i = 0; $i < $arrays; ++$i) {
                $type = sprintf('Array<%s>', $type);
            }

            $types[] = $type;
        }

        return $types;
    }

    public function getName(): string
    {
        return $this->name;
    }

    public function getTypes(): array
    {
        return $this->types;
    }

    public function isOptional(): bool
    {
        return $this->optional;
    }

    public function getDefaultValue(): mixed
    {
        if (!isset($this->defaultValue)) {
            $this->defaultValue = $this->description->getDefaultValue();
        }

        return $this->defaultValue;
    }

    #[ArrayShape([
        'name' => 'string',
        'types' => 'array',
        'optional' => 'bool',
        'description' => 'string',
        'default' => 'mixed',
    ])]
    public function toArray(): array
    {
        $result = [
            'name' => $this->name,
            'types' => $this->types,
            'optional' => $this->optional,
            'description' => (string) $this->description,
        ];
        $defaultValue = $this->getDefaultValue();
        if (null !== $defaultValue) {
            $result['default'] = $defaultValue;
        }

        return $result;
    }
}
