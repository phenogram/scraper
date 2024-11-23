<?php

declare(strict_types=1);

namespace TgScraper\Common\AbstractClassResolvers;

class BotCommandScope implements AbstractClassResolverInterface
{
    public static function getBody(array $params): string
    {
        return <<<'PHP'
            return match ($data['type']) {
                'default' => $this->denormalizeBotCommandScopeDefault($data),
                'all_private_chats' => $this->denormalizeBotCommandScopeAllPrivateChats($data),
                'all_group_chats' => $this->denormalizeBotCommandScopeAllGroupChats($data),
                'all_chat_administrators' => $this->denormalizeBotCommandScopeAllChatAdministrators($data),
                'chat' => $this->denormalizeBotCommandScopeChat($data),
                'chat_administrators' => $this->denormalizeBotCommandScopeChatAdministrators($data),
                'chat_member' => $this->denormalizeBotCommandScopeChatMember($data),
                default => throw new \InvalidArgumentException(sprintf('Invalid type value for BotCommandScope: %s', $data['type'])),
            };
            PHP;
    }
}
