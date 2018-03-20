{-# LANGUAGE OverloadedStrings #-}

import           Hakyll
import           Meetup

main :: IO ()
main = hakyll $ do

    match "images/*.png" $ do
        route idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route idRoute
        compile compressCssCompiler

    match "content/index.html" $ do
        route idRoute
        compile getResourceBody

    create ["content/meetup.html"] $ do
        route idRoute
        compile $ unsafeCompiler getMeetup >>= makeItem
