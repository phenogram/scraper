<?php

namespace TgScraper\Common\AbstractClassResolvers;

interface AbstractClassResolverInterface
{
    public static function getBody(array $params): string;
}