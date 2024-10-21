<?php

declare(strict_types=1);

namespace TgScraper\Common\RedefinedTypes;

use Nette\PhpGenerator\Parameter;
use Nette\PhpGenerator\PromotedParameter;
use Nette\PhpGenerator\Type;

class InputFile implements RedefinedTypeInterface
{
    /**
     * @return list<Parameter>
     */
    public static function getConstructorParams(): array
    {
        return [
            (new PromotedParameter(name: 'filePath'))->setType(Type::String),
        ];
    }

    public static function getConstructorComment(): string
    {
        return '';
    }
}
