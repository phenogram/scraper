<?php

declare(strict_types=1);

namespace TgScraper\Common\RedefinedTypes;

use Nette\PhpGenerator\Parameter;

interface RedefinedTypeInterface
{
    /**
     * @return list<Parameter>
     */
    public static function getConstructorParams(): array;

    public static function getConstructorComment(): string;
}
