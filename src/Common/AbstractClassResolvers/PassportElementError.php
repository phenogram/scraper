<?php

declare(strict_types=1);

namespace TgScraper\Common\AbstractClassResolvers;

class PassportElementError implements AbstractClassResolverInterface
{
    public static function getBody(array $params): string
    {
        return <<<'PHP'
            return match ($data['source']) {
                'data' => $this->denormalizePassportElementErrorDataField($data),
                'front_side' => $this->denormalizePassportElementErrorFrontSide($data),
                'reverse_side' => $this->denormalizePassportElementErrorReverseSide($data),
                'selfie' => $this->denormalizePassportElementErrorSelfie($data),
                'file' => $this->denormalizePassportElementErrorFile($data),
                'files' => $this->denormalizePassportElementErrorFiles($data),
                'translation_file' => $this->denormalizePassportElementErrorTranslationFile($data),
                'translation_files' => $this->denormalizePassportElementErrorTranslationFiles($data),
                'unspecified' => $this->denormalizePassportElementErrorUnspecified($data),
                default => throw new \InvalidArgumentException(sprintf('Invalid source value for PassportElementError: %s', $data['source'])),
            };
            PHP;
    }
}
