<?php

namespace TgScraper\Tests\Common;

use PHPUnit\Framework\TestCase;
use TgScraper\Common\StubCreator;

final class BotApi104GenerationTest extends TestCase
{
    private const GENERATED_NAMESPACE = 'TgScraper\\Tests\\Generated\\BotApi104';

    public function testNewAbstractTypesDecodeEveryDocumentedVariant(): void
    {
        $serializer = $this->createSerializer();
        $cases = [
            ['WidgetQueryType', 'WidgetQueryTypeData', ['type' => 'data']],
            ['WidgetQueryType', 'WidgetQueryTypeSignData', ['type' => 'sign_data', 'sign_data' => 'signed']],
            ['WidgetQueryType', 'WidgetQueryTypeSentTransaction', ['type' => 'sent_transaction', 'msg_hash' => 'hash']],
            ['EmojiType', 'EmojiTypeEmoji', ['type' => 'emoji', 'emoji' => '👍']],
            ['EmojiType', 'EmojiTypeCustomEmoji', ['type' => 'custom_emoji', 'custom_emoji_id' => '123']],
            ['ChatShareTheme', 'ChatShareThemeEmoji', ['type' => 'emoji', 'emojis' => [['type' => 'emoji', 'emoji' => '👍']]]],
            ['ChatShareTheme', 'ChatShareThemeUniqueGift', ['type' => 'unique_gift', 'gift' => ['name' => 'test-gift']]],
        ];
        foreach ($cases as [$abstract, $concrete, $input]) {
            $interface = self::GENERATED_NAMESPACE . '\\Types\\Interfaces\\' . $abstract . 'Interface';
            self::assertTrue($serializer->supports($interface));
            $decoded = $serializer->deserialize($input, $interface);
            self::assertInstanceOf(self::GENERATED_NAMESPACE . '\\Types\\' . $concrete, $decoded);
            self::assertSame(['value' => $input], $serializer->serialize(['value' => $decoded]));
        }

        $input = ['type' => ['type' => 'sign_data', 'sign_data' => 'signed']];
        $decoded = $serializer->deserialize($input, self::GENERATED_NAMESPACE . '\\Types\\Interfaces\\WidgetQueryInterface');
        self::assertInstanceOf(self::GENERATED_NAMESPACE . '\\Types\\WidgetQueryTypeSignData', $decoded->type);
    }

    public function testNewAbstractTypeFixturesProduceUsableObjects(): void
    {
        $serializer = $this->createSerializer();
        $queryFactory = self::GENERATED_NAMESPACE . '\\Factories\\WidgetQueryFactory';
        $query = $queryFactory::make();
        self::assertInstanceOf(self::GENERATED_NAMESPACE . '\\Types\\WidgetQueryTypeData', $query->type);
        self::assertSame('data', $query->type->type);

        $themeFactory = self::GENERATED_NAMESPACE . '\\Factories\\ThemeContainerFactory';
        $theme = $themeFactory::make();
        self::assertInstanceOf(self::GENERATED_NAMESPACE . '\\Types\\ChatShareThemeEmoji', $theme->theme);
        self::assertSame('emoji', $theme->theme->type);
        self::assertInstanceOf(self::GENERATED_NAMESPACE . '\\Types\\EmojiTypeEmoji', $theme->theme->emojis[0]);
        self::assertSame('👍', $theme->theme->emojis[0]->emoji);
        $encoded = $serializer->serialize(['theme' => $theme->theme]);
        $decoded = $serializer->deserialize($encoded['theme'], self::GENERATED_NAMESPACE . '\\Types\\Interfaces\\ChatShareThemeInterface');
        self::assertSame($encoded, $serializer->serialize(['theme' => $decoded]));
    }

    public function testUnknownDiscriminatorsAreRejected(): void
    {
        $serializer = $this->createSerializer();
        foreach (['WidgetQueryType', 'ChatShareTheme', 'EmojiType'] as $type) {
            foreach ([[], ['type' => 'unknown']] as $input) {
                try {
                    $serializer->deserialize($input, self::GENERATED_NAMESPACE . '\\Types\\Interfaces\\' . $type . 'Interface');
                    self::fail('Unknown discriminator accepted for ' . $type);
                } catch (\InvalidArgumentException $exception) {
                    self::assertSame('Unknown ' . $type . ' type', $exception->getMessage());
                }
            }
        }
    }

    private function createSerializer(): object
    {
        $class = self::GENERATED_NAMESPACE . '\\Serializer';
        if (!class_exists($class, false)) {
            $field = static fn (string $name, string $type, string $description = ''): array => [
                'name' => $name, 'types' => [$type], 'optional' => false, 'description' => $description,
            ];
            $type = static fn (string $name, array $fields = [], array $children = []): array => [
                'name' => $name, 'description' => $name, 'fields' => $fields, 'extended_by' => $children,
            ];
            $discriminator = static fn (string $value): array => $field('type', 'string', 'Type, always “' . $value . '”');
            $schema = [
                'version' => 'test',
                'types' => [
                    $type('WidgetQueryType', children: ['WidgetQueryTypeData', 'WidgetQueryTypeSignData', 'WidgetQueryTypeSentTransaction']),
                    $type('WidgetQueryTypeData', [$discriminator('data')]),
                    $type('WidgetQueryTypeSignData', [$discriminator('sign_data'), $field('sign_data', 'string')]),
                    $type('WidgetQueryTypeSentTransaction', [$discriminator('sent_transaction'), $field('msg_hash', 'string')]),
                    $type('WidgetQuery', [$field('type', 'WidgetQueryType')]),
                    $type('EmojiType', children: ['EmojiTypeEmoji', 'EmojiTypeCustomEmoji']),
                    $type('EmojiTypeEmoji', [$discriminator('emoji'), $field('emoji', 'string')]),
                    $type('EmojiTypeCustomEmoji', [$discriminator('custom_emoji'), $field('custom_emoji_id', 'string')]),
                    $type('ChatShareTheme', children: ['ChatShareThemeEmoji', 'ChatShareThemeUniqueGift']),
                    $type('ChatShareThemeEmoji', [$discriminator('emoji'), $field('emojis', 'Array<EmojiType>')]),
                    $type('ChatShareThemeUniqueGift', [$discriminator('unique_gift'), $field('gift', 'UniqueGift')]),
                    $type('UniqueGift', [$field('name', 'string')]),
                    $type('ThemeContainer', [$field('theme', 'ChatShareTheme')]),
                ],
                'methods' => [],
            ];
            $creator = new StubCreator($schema, self::GENERATED_NAMESPACE);
            $generated = $creator->generateTypes();
            $files = [self::GENERATED_NAMESPACE . '\\Types\\Interfaces\\TypeInterface' => (string) $generated['defaultTypeInterface']];
            foreach ($generated['types'] as $name => $definitions) {
                $files[self::GENERATED_NAMESPACE . '\\Types\\Interfaces\\' . $name . 'Interface'] = (string) $definitions['interface'];
                if ($definitions['class'] !== null) {
                    $files[self::GENERATED_NAMESPACE . '\\Types\\' . $name] = (string) $definitions['class'];
                }
            }
            foreach ($generated['files'] as $name => $definition) {
                $files[self::GENERATED_NAMESPACE . '\\' . $name] = (string) $definition;
            }
            foreach ($creator->generateTestFactories() as $name => $definition) {
                $files[self::GENERATED_NAMESPACE . '\\Factories\\' . $name] = (string) $definition;
            }
            spl_autoload_register(static function (string $class) use ($files): void {
                if (isset($files[$class])) {
                    eval(substr($files[$class], strlen('<?php')));
                }
            });
        }

        return new $class();
    }
}
