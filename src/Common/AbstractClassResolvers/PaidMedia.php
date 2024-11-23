<?php

declare(strict_types=1);

namespace TgScraper\Common\AbstractClassResolvers;

class PaidMedia implements AbstractClassResolverInterface
{
    public static function getBody(array $params): string
    {
        return <<<'PHP'
            return match ($data['type']) {
                'preview' => $this->denormalizePaidMediaPreview($data),
                'photo' => $this->denormalizePaidMediaPhoto($data),
                'video' => $this->denormalizePaidMediaVideo($data),
                default => throw new \InvalidArgumentException(sprintf('Invalid type value for PaidMedia: %s', $data['type'])),
            };
            PHP;
    }
}
