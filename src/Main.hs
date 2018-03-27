{-# LANGUAGE OverloadedStrings #-}

import           Data.Monoid ((<>))
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

    match "index.html" $ do
        route idRoute
        compile $
            getResourceBody >>= applyAsTemplate sectionContext

    match "sections/about-us.html" $ do
        route idRoute
        compile getResourceBody

    create ["sections/meetup.html"] $ do
        route idRoute
        compile $ unsafeCompiler getMeetups >>= makeItem

sectionContext :: Context String
sectionContext =
    functionField "section" (\[name] _ -> loadBody (fromFilePath name)) <>
    defaultContext
