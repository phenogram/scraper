<?php

declare(strict_types=1);

namespace TgScraper\Common\AbstractClassResolvers;

class ReactionType implements AbstractClassResolverInterface
{
    public static function getBody(array $params): string
    {
        return <<<'PHP'
            return match ($data['type']) {
                'emoji' => $this->denormalizeReactionTypeEmoji($data),
                'custom_emoji' => $this->denormalizeReactionTypeCustomEmoji($data),
                'paid' => $this->denormalizeReactionTypePaid($data),
                default => throw new \InvalidArgumentException(sprintf('Invalid type value for ReactionType: %s', $data['type'])),
            };
            PHP;
    }
}
