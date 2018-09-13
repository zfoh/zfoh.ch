{-# LANGUAGE OverloadedStrings #-}

import           Data.Monoid     ((<>))
import qualified Data.Time       as Time
import           Hakyll
import           Meetup
import           System.FilePath (joinPath, splitPath)

main :: IO ()
main = hakyll $ do

    match ("images/*.png" .||. "images/*.jpg") $ do
        route idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route idRoute
        compile compressCssCompiler

    match "content/index.html" $ do
        route dropContentRoute
        compile $
            getResourceBody >>=
            applyAsTemplate sectionContext >>=
            loadAndApplyTemplate "templates/main.html" zfohContext

    match (fromList
        [ "content/privacy-policy.html"
        , "content/terms-and-conditions.html"]) $ do
        route dropContentRoute
        compile $
            getResourceBody >>=
            loadAndApplyTemplate "templates/main.html" zfohContext

    match "content/zurihac2019/index.html" $ do
        route dropContentRoute
        compile $
            getResourceBody >>= applyAsTemplate sectionContext

    match "content/index/*.html" $ compile getResourceBody

    create ["content/index/meetup.html"] $ do
        route idRoute
        compile $ unsafeCompiler getMeetups >>= makeItem

    match "templates/*.html" $ compile templateCompiler

sectionContext :: Context String
sectionContext =
    functionField "section" (\[name] _ -> loadBody (fromFilePath name)) <>
    zfohContext

zfohContext :: Context String
zfohContext =
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

-- | Drop the `content/` part from a route.
dropContentRoute :: Routes
dropContentRoute = customRoute $ \ident ->
    let path0 = toFilePath ident in
    case splitPath path0 of
        "content/" : path1 -> joinPath path1
        _                  -> path0
