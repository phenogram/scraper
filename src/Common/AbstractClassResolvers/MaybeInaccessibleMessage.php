<?php

namespace TgScraper\Common\AbstractClassResolvers;

class MaybeInaccessibleMessage implements AbstractClassResolverInterface
{
    public static function getBody(array $params): string
    {
        return <<<'BODY'
            if ($data['date'] === 0) {
                return $this->denormalizeInaccessibleMessage($data);
            } else {
                return $this->denormalizeMessage($data);
            }
            BODY;
    }
}
