<?php

declare(strict_types=1);

namespace TgScraper\Common\AbstractClassResolvers;

class ChatBoostSource implements AbstractClassResolverInterface
{
    public static function getBody(array $params): string
    {
        return <<<'PHP'
            return match ($data['source']) {
                'premium' => $this->denormalizeChatBoostSourcePremium($data),    
                'gift_code' => $this->denormalizeChatBoostSourceGiftCode($data),
                'giveaway' => $this->denormalizeChatBoostSourceGiveaway($data),
                default => throw new \InvalidArgumentException(sprintf('Invalid source value for ChatBoostSource: %s', $data['source'])),
            };
            PHP;
    }
}
