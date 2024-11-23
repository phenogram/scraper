<?php

declare(strict_types=1);

namespace TgScraper\Common\AbstractClassResolvers;

class BackgroundType implements AbstractClassResolverInterface
{
    public static function getBody(array $params): string
    {
        return <<<'PHP'
            return match ($data['type']) {
                'fill' => $this->denormalizeBackgroundTypeFill($data),
                'wallpaper' => $this->denormalizeBackgroundTypeWallpaper($data),
                'pattern' => $this->denormalizeBackgroundTypePattern($data),
                'chat_theme' => $this->denormalizeBackgroundTypeChatTheme($data),
                default => throw new \InvalidArgumentException(sprintf('Invalid type value for BackgroundType: %s', $data['type'])),
            };
            PHP;
    }
}
