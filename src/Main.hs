{-# LANGUAGE OverloadedStrings #-}

import           Data.Monoid ((<>))
import qualified Data.Time   as Time
import           Hakyll
import           Meetup

main :: IO ()
main = hakyll $ do

    match ("images/*.png" .||. "images/*.jpg") $ do
        route idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route idRoute
        compile compressCssCompiler

    -- TODO (jaspervdj): Unify these two guys
    match "content/index.html" $ do
        route $ constRoute "index.html"
        compile $
            getResourceBody >>= applyAsTemplate sectionContext
    match "content/zurihac2019/index.html" $ do
        route $ constRoute "zurihac2019/index.html"
        compile $
            getResourceBody >>= applyAsTemplate sectionContext

    match "content/sections/*.html" $ compile getResourceBody

    create ["content/sections/meetup.html"] $ do
        route idRoute
        compile $ unsafeCompiler getMeetups >>= makeItem

sectionContext :: Context String
sectionContext =
    functionField "section" (\[name] _ -> loadBody (fromFilePath name)) <>
    field "copyrightYears" (\_ -> do
        let founded = 2018
        year <- unsafeCompiler getCurrentYear
        return $ if year == founded
            then show year
            else show founded ++ " - " ++ show year) <>
    defaultContext

getCurrentYear :: IO Integer
getCurrentYear = do
    (y, _m, _d) <- Time.toGregorian . Time.utctDay <$> Time.getCurrentTime
    return y
