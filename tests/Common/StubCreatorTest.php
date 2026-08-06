<?php

namespace TgScraper\Tests\Common;

use PHPUnit\Framework\TestCase;
use TgScraper\Common\StubCreator;

final class StubCreatorTest extends TestCase
{
    public function testSerializerDispatchesResolvableAbstractResponseTypes(): void
    {
        $schema = [
            'version' => 'test',
            'types' => [
                [
                    'name' => 'ChatMember',
                    'description' => 'A chat member.',
                    'fields' => [],
                    'extended_by' => [
                        'ChatMemberOwner',
                        'ChatMemberAdministrator',
                        'ChatMemberMember',
                        'ChatMemberRestricted',
                        'ChatMemberLeft',
                        'ChatMemberBanned',
                    ],
                ],
                ...array_map(
                    static fn (string $name): array => [
                        'name' => $name,
                        'description' => 'A concrete chat member.',
                        'fields' => [
                            [
                                'name' => 'status',
                                'types' => ['string'],
                                'optional' => false,
                                'description' => 'Member status.',
                            ],
                        ],
                        'extended_by' => [],
                    ],
                    [
                        'ChatMemberOwner',
                        'ChatMemberAdministrator',
                        'ChatMemberMember',
                        'ChatMemberRestricted',
                        'ChatMemberLeft',
                        'ChatMemberBanned',
                    ],
                ),
            ],
            'methods' => [
                [
                    'name' => 'getChatMember',
                    'description' => 'Returns a chat member.',
                    'fields' => [],
                    'return_types' => ['ChatMember'],
                ],
            ],
        ];

        $generated = (new StubCreator($schema, 'Phenogram\\Bindings'))->generateTypes();
        $serializer = (string) $generated['files']['Serializer'];

        self::assertStringContainsString(
            'ChatMemberInterface::class => $this->denormalizeChatMember($data)',
            $serializer,
        );
    }
}
