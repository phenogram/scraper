<?php

declare(strict_types=1);

namespace TgScraper\Common\AbstractClassResolvers;

class BackgroundFill implements AbstractClassResolverInterface
{
    public static function getBody(array $params): string
    {
        return <<<'PHP'
            return match ($data['type']) {
                'solid' => $this->denormalizeBackgroundFillSolid($data),
                'gradient' => $this->denormalizeBackgroundFillGradient($data),
                'freeform_gradient' => $this->denormalizeBackgroundFillFreeformGradient($data),
                default => throw new \InvalidArgumentException(sprintf('Invalid type value for BackgroundFill: %s', $data['type'])),
            };
            PHP;
    }
}
