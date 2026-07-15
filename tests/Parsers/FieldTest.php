<?php

namespace TgScraper\Tests\Parsers;

use PHPUnit\Framework\TestCase;
use TgScraper\Parsers\Field;

class FieldTest extends TestCase
{
    public function testOnlyCompletePrimitiveTypeNamesAreNormalized(): void
    {
        self::assertSame(['int'], (new Field('count', 'Int', false, ''))->getTypes());
        self::assertSame(
            ['BusinessIntro'],
            (new Field('business_intro', 'BusinessIntro', false, ''))->getTypes(),
        );
        self::assertSame(
            ['BusinessOpeningHoursInterval'],
            (new Field('opening_hours', 'BusinessOpeningHoursInterval', false, ''))->getTypes(),
        );
    }
}
