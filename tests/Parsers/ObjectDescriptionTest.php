<?php

namespace TgScraper\Tests\Parsers;

use PHPUnit\Framework\TestCase;
use TgScraper\Parsers\ObjectDescription;

class ObjectDescriptionTest extends TestCase
{
    public function testPrimitiveReturnTypesAreNormalizedToPhpTypes(): void
    {
        $types = [
            'Integer' => 'int',
            'Int' => 'int',
            'Float' => 'float',
            'String' => 'string',
            'Boolean' => 'bool',
            'True' => 'bool',
        ];

        foreach ($types as $telegramType => $phpType) {
            $description = new ObjectDescription("Returns <em>{$telegramType}</em>.");

            self::assertSame([$phpType], $description->getTypes(), $telegramType);
        }
    }
}
