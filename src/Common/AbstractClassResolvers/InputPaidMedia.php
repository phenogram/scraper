<?php

declare(strict_types=1);

namespace TgScraper\Common\AbstractClassResolvers;

class InputPaidMedia implements AbstractClassResolverInterface
{
    public static function getBody(array $params): string
    {
        return <<<'PHP'
            return match ($data['type']) {
                'photo' => $this->denormalizeInputPaidMediaPhoto($data),
                'video' => $this->denormalizeInputPaidMediaVideo($data),
                default => throw new \InvalidArgumentException(sprintf('Invalid type value for InputPaidMedia: %s', $data['type'])),
            };
            PHP;
    }
}
