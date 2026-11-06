<?php

namespace TgScraper\Tests\Common;

use PHPUnit\Framework\TestCase;
use TgScraper\Common\StubCreator;

final class RichTextGenerationTest extends TestCase
{
    private const GENERATED_NAMESPACE = 'TgScraper\\Tests\\Generated\\RichMessages';

    public function testGeneratedRichTextAndBlockTypesRoundTrip(): void
    {
        $serializer = $this->createSerializer();
        $text = ['Before ', ['type' => 'bold', 'text' => ['nested ', 'text']], ' after'];
        $button = ['text' => $text, 'callback_data' => 'open'];

        $decoded = $serializer->deserialize($button, self::GENERATED_NAMESPACE . '\\Types\\Interfaces\\RichMessageButtonInterface');
        self::assertSame('Before ', $decoded->text[0]);
        self::assertInstanceOf(self::GENERATED_NAMESPACE . '\\Types\\RichTextBold', $decoded->text[1]);
        self::assertSame(['nested ', 'text'], $decoded->text[1]->text);
        self::assertSame(['button' => $button], $serializer->serialize(['button' => $decoded]));

        $richTextInterface = self::GENERATED_NAMESPACE . '\\Types\\Interfaces\\RichTextInterface';
        self::assertTrue($serializer->supports($richTextInterface));
        self::assertSame('plain', $serializer->deserialize('plain', $richTextInterface));
        self::assertSame([], $serializer->deserialize([], $richTextInterface));
        self::assertSame(['plain', ['nested']], $serializer->deserialize(['plain', ['nested']], $richTextInterface, true));
        self::assertSame(['plain', ['nested']], $serializer->denormalize(['plain', ['nested']], $richTextInterface, true));
        self::assertSame(['text' => $text], $serializer->serialize(['text' => $serializer->deserialize($text, $richTextInterface)]));

        foreach (['RichBlock', 'InputRichBlock'] as $type) {
            $block = ['type' => 'buttons', 'buttons' => [$button]];
            $decoded = $serializer->deserialize($block, self::GENERATED_NAMESPACE . '\\Types\\Interfaces\\' . $type . 'Interface');
            self::assertInstanceOf(self::GENERATED_NAMESPACE . '\\Types\\' . $type . 'Buttons', $decoded);
            self::assertSame(['block' => $block], $serializer->serialize(['block' => $decoded]));
        }
    }

    public function testGeneratedRichTypeResolverRejectsUnknownDiscriminator(): void
    {
        $serializer = $this->createSerializer();
        $this->expectException(\InvalidArgumentException::class);
        $this->expectExceptionMessage('Unknown RichBlock type');
        $serializer->deserialize(['type' => 'unknown'], self::GENERATED_NAMESPACE . '\\Types\\Interfaces\\RichBlockInterface');
    }

    public function testInputRichMessageMediaDispatchesEveryAllowedMediaKind(): void
    {
        $serializer = $this->createSerializer();
        $interface = self::GENERATED_NAMESPACE . '\\Types\\Interfaces\\InputRichMessageMediaInterface';
        foreach (['Animation' => 'animation', 'Audio' => 'audio', 'Document' => 'document', 'Photo' => 'photo', 'Video' => 'video', 'VoiceNote' => 'voice_note'] as $class => $type) {
            $input = ['id' => 'attachment', 'media' => ['type' => $type, 'media' => 'file-id']];
            $decoded = $serializer->deserialize($input, $interface);
            self::assertInstanceOf(self::GENERATED_NAMESPACE . '\\Types\\InputMedia' . $class, $decoded->media);
            self::assertEquals(['attachment' => $input], $serializer->serialize(['attachment' => $decoded]));
        }

        $this->expectException(\InvalidArgumentException::class);
        $serializer->deserialize(['id' => 'attachment', 'media' => ['type' => 'sticker', 'media' => 'file-id']], $interface);
    }

    private function createSerializer(): object
    {
        $class = self::GENERATED_NAMESPACE . '\\Serializer';
        if (!class_exists($class, false)) {
            $field = static fn (string $name, string $type, string $description = ''): array => [
                'name' => $name,
                'types' => [$type],
                'optional' => false,
                'description' => $description,
            ];
            $type = static fn (string $name, array $fields = [], array $children = []): array => [
                'name' => $name,
                'description' => $name,
                'fields' => $fields,
                'extended_by' => $children,
            ];
            $schema = [
                'version' => 'test',
                'types' => [
                    $type('RichText', children: ['RichTextBold']),
                    $type('RichTextBold', [$field('type', 'string', 'Type, always “bold”'), $field('text', 'RichText')]),
                    $type('RichMessageButton', [$field('text', 'RichText'), $field('callback_data', 'string')]),
                    $type('RichBlock', children: ['RichBlockButtons']),
                    $type('RichBlockButtons', [$field('type', 'string', 'Type, always “buttons”'), $field('buttons', 'Array<RichMessageButton>')]),
                    $type('InputRichBlock', children: ['InputRichBlockButtons']),
                    $type('InputRichBlockButtons', [$field('type', 'string', 'Type, always “buttons”'), $field('buttons', 'Array<RichMessageButton>')]),
                ],
                'methods' => [],
            ];
            $mediaClasses = [];
            foreach (['Animation' => 'animation', 'Audio' => 'audio', 'Document' => 'document', 'Photo' => 'photo', 'Video' => 'video', 'VoiceNote' => 'voice_note'] as $name => $discriminator) {
                $mediaClasses[] = 'InputMedia' . $name;
                $typeField = $field('type', 'string', 'Type of the media, must be ' . $discriminator);
                $typeField['default'] = $discriminator;
                $schema['types'][] = $type('InputMedia' . $name, [$typeField, $field('media', 'string')]);
            }
            $media = $field('media', '');
            $media['types'] = $mediaClasses;
            $schema['types'][] = $type('InputRichMessageMedia', [$field('id', 'string'), $media]);
            $generated = (new StubCreator($schema, self::GENERATED_NAMESPACE))->generateTypes();
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
            spl_autoload_register(static function (string $class) use ($files): void {
                if (isset($files[$class])) {
                    eval(substr($files[$class], strlen('<?php')));
                }
            });
        }

        return new $class();
    }
}
