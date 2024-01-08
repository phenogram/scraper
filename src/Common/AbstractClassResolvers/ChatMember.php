<?php

namespace TgScraper\Common\AbstractClassResolvers;

class ChatMember implements AbstractClassResolverInterface
{
    public static function getBody(array $params): string
    {
        return <<<'BODY'
            return match ($data['status']) {
                'creator' => $this->denormalizeChatMemberOwner($data),
                'administrator' => $this->denormalizeChatMemberAdministrator($data),
                'member' => $this->denormalizeChatMemberMember($data),
                'restricted' => $this->denormalizeChatMemberRestricted($data),
                'left' => $this->denormalizeChatMemberLeft($data),
                'kicked' => $this->denormalizeChatMemberBanned($data),
                default => throw new \InvalidArgumentException(sprintf('Invalid status value: %s', $data['status'])),
            };
            BODY;
    }
}
