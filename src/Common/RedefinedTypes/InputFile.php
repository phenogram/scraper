<?php

declare(strict_types=1);

namespace TgScraper\Common\RedefinedTypes;

use Nette\PhpGenerator\Parameter;
use Nette\PhpGenerator\PromotedParameter;
use Nette\PhpGenerator\Property;
use Nette\PhpGenerator\Type;

class InputFile implements RedefinedTypeInterface
{
    /**
     * @return list<Parameter>
     */
    public static function getConstructorParams(): array
    {
        return [
            new PromotedParameter(name: 'filePath')->setType(Type::String),
        ];
    }

    public static function getConstructorComment(): string
    {
        return '';
    }

    /**
     * @return array<Property>
     */
    public static function getInterfaceProperties(): array
    {
        $filePath = new Property('filePath')->setType(Type::String)->setPublic();
        $filePath->addSetHook('');
        $filePath->addGetHook('');

        return [
            $filePath,
        ];
    }
}
