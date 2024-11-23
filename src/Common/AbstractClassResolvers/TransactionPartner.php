<?php

declare(strict_types=1);

namespace TgScraper\Common\AbstractClassResolvers;

class TransactionPartner implements AbstractClassResolverInterface
{
    public static function getBody(array $params): string
    {
        return <<<'PHP'
            return match ($data['type']) {
                'user' => $this->denormalizeTransactionPartnerUser($data),
                'fragment' => $this->denormalizeTransactionPartnerFragment($data),
                'telegram_ads' => $this->denormalizeTransactionPartnerTelegramAds($data),
                'telegram_api' => $this->denormalizeTransactionPartnerTelegramApi($data),
                'other' => $this->denormalizeTransactionPartnerOther($data),
                default => throw new \InvalidArgumentException(sprintf('Invalid type value for TransactionPartner: %s', $data['type'])),
            };
            PHP;
    }
}
