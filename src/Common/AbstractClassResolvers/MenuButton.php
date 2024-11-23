<?php

declare(strict_types=1);

namespace TgScraper\Common\AbstractClassResolvers;

class MenuButton implements AbstractClassResolverInterface
{
    public static function getBody(array $params): string
    {
        return <<<'PHP'
            return match ($data['type']) {
                'commands' => $this->denormalizeMenuButtonCommands($data),
                'web_app' => $this->denormalizeMenuButtonWebApp($data),
                'default' => $this->denormalizeMenuButtonDefault($data),
                default => throw new \InvalidArgumentException(sprintf('Invalid type value for MenuButton: %s', $data['type'])),
            };
            PHP;
    }
}
