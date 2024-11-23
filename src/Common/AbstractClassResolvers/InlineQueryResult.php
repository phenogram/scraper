<?php

declare(strict_types=1);

namespace TgScraper\Common\AbstractClassResolvers;

class InlineQueryResult implements AbstractClassResolverInterface
{
    public static function getBody(array $params): string
    {
        return <<<'PHP'
            return match ($data['type']) {
                'article' => $this->denormalizeInlineQueryResultArticle($data),
                'photo' => $this->denormalizeInlineQueryResultPhoto($data),
                'gif' => $this->denormalizeInlineQueryResultGif($data),
                'mpeg4_gif' => $this->denormalizeInlineQueryResultMpeg4Gif($data),
                'video' => $this->denormalizeInlineQueryResultVideo($data),
                'audio' => $this->denormalizeInlineQueryResultAudio($data),
                'voice' => $this->denormalizeInlineQueryResultVoice($data),
                'document' => $this->denormalizeInlineQueryResultDocument($data),
                'location' => $this->denormalizeInlineQueryResultLocation($data),
                'venue' => $this->denormalizeInlineQueryResultVenue($data),
                'contact' => $this->denormalizeInlineQueryResultContact($data),
                'game' => $this->denormalizeInlineQueryResultGame($data),
                'cached_photo' => $this->denormalizeInlineQueryResultCachedPhoto($data),
                'cached_gif' => $this->denormalizeInlineQueryResultCachedGif($data),
                'cached_mpeg4_gif' => $this->denormalizeInlineQueryResultCachedMpeg4Gif($data),
                'cached_sticker' => $this->denormalizeInlineQueryResultCachedSticker($data),
                'cached_document' => $this->denormalizeInlineQueryResultCachedDocument($data),
                'cached_video' => $this->denormalizeInlineQueryResultCachedVideo($data),
                'cached_voice' => $this->denormalizeInlineQueryResultCachedVoice($data),
                'cached_audio' => $this->denormalizeInlineQueryResultCachedAudio($data),
                default => throw new \InvalidArgumentException(sprintf('Invalid type value for InlineQueryResult: %s', $data['type'])),
            };
            PHP;
    }
}
