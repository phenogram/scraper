<?php

declare(strict_types=1);

namespace TgScraper\Common\AbstractClassResolvers;

class InputMedia implements AbstractClassResolverInterface
{
    public static function getBody(array $params): string
    {
        return <<<'PHP'
            return match ($data['type']) {
                'photo' => $this->denormalizeInputMediaPhoto($data),
                'video' => $this->denormalizeInputMediaVideo($data),
                'animation' => $this->denormalizeInputMediaAnimation($data),
                'audio' => $this->denormalizeInputMediaAudio($data),
                'document' => $this->denormalizeInputMediaDocument($data),
                default => throw new \InvalidArgumentException(sprintf('Invalid type value for InputMedia: %s', $data['type'])),
            };
            PHP;
    }
}
