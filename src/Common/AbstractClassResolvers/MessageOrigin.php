<?php

namespace TgScraper\Common\AbstractClassResolvers;

class MessageOrigin implements AbstractClassResolverInterface
{
    public static function getBody(array $params): string
    {
        return <<<'PHP'
            return match ($data['type']) {
                'user' => $this->denormalizeMessageOriginUser($data),
                'hidden_user' => $this->denormalizeMessageOriginHiddenUser($data),
                'chat' => $this->denormalizeMessageOriginChat($data),
                'channel' => $this->denormalizeMessageOriginChannel($data),
                default => throw new \InvalidArgumentException(sprintf('Invalid type "%s" given. Supported types are: "user", "hidden_user", "chat", "channel"', $data['type'])),
            };
            PHP;
    }
}
