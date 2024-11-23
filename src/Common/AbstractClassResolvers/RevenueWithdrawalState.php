<?php

declare(strict_types=1);

namespace TgScraper\Common\AbstractClassResolvers;

class RevenueWithdrawalState implements AbstractClassResolverInterface
{
    public static function getBody(array $params): string
    {
        return <<<'PHP'
            return match ($data['type']) {
                'pending' => $this->denormalizeRevenueWithdrawalStatePending($data),
                'succeeded' => $this->denormalizeRevenueWithdrawalStateSucceeded($data),
                'failed' => $this->denormalizeRevenueWithdrawalStateFailed($data),
                default => throw new \InvalidArgumentException(sprintf('Invalid type value for RevenueWithdrawalState: %s', $data['type'])),
            };
            PHP;
    }
}
