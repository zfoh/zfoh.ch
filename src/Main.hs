{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Time        as Time
import           Hakyll
import           Meetup
import           System.FilePath  (joinPath, splitPath)

main :: IO ()
main = hakyll $ do

    ----------------------------------------------------------------------------
    -- Images, CSS, etc.

    match ("images/**.png" .||. "images/**.jpg" .||. "images/**.gif" .||. "images/**.svg") $ do
        route idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route idRoute
        compile compressCssCompiler

    match "content/zurihac20*/files/*" $ do
        route dropContentRoute
        compile copyFileCompiler

    ----------------------------------------------------------------------------
    -- Simple static pages.

    match "content/index.html" $ do
        route dropContentRoute
        compile $
            getResourceBody >>=
            applyAsTemplate sectionContext >>=
            loadAndApplyTemplate "templates/zfoh.html" zfohContext

    match (fromList
        [ "content/privacy-policy.html"
        , "content/terms-and-conditions.html"]) $ do
        route dropContentRoute
        compile $
            getResourceBody >>=
            loadAndApplyTemplate "templates/zfoh.html" zfohContext

    ----------------------------------------------------------------------------
    -- ZuriHac 2019.

    match "content/zurihac2019/index.html" $ do
        route dropContentRoute
        compile $
            getResourceBody >>=
            applyAsTemplate sectionContext >>=
            loadAndApplyTemplate "templates/zurihac2019.html" zfohContext

    ----------------------------------------------------------------------------
    -- ZuriHac 2020.

    match "content/zurihac2020/index.html" $ do
        route dropContentRoute
        compile $
            getResourceBody >>=
            applyAsTemplate sectionContext >>=
            loadAndApplyTemplate "templates/zurihac2020.html" zfohContext

    ----------------------------------------------------------------------------
    -- ZuriHac 2021.

    match "content/zurihac2021/index.html" $ do
        route dropContentRoute
        compile $
            getResourceBody >>=
            applyAsTemplate sectionContext >>=
            loadAndApplyTemplate "templates/zurihac2021.html" zfohContext

    ----------------------------------------------------------------------------
    -- Projects page.

    let projectsStaticFiles =
            [ "content/zurihac2019/projects.json"
            , "content/zurihac2019/projects/bookmark-regular.svg"
            , "content/zurihac2019/projects/bookmark-solid.svg"
            , "content/zurihac2019/projects/main.js"
            , "content/zurihac2019/projects/projects.css"
            , "content/zurihac2019/projects/projects.js"
            -- 2020
            , "content/zurihac2020/projects.json"
            , "content/zurihac2020/projects/bookmark-regular.svg"
            , "content/zurihac2020/projects/bookmark-solid.svg"
            , "content/zurihac2020/projects/main.js"
            , "content/zurihac2020/projects/projects.css"
            , "content/zurihac2020/projects/projects.js"
            -- 2021
            , "content/zurihac2021/projects.json"
            , "content/zurihac2021/projects/bookmark-regular.svg"
            , "content/zurihac2021/projects/bookmark-solid.svg"
            , "content/zurihac2021/projects/main.js"
            , "content/zurihac2021/projects/projects.css"
            , "content/zurihac2021/projects/projects.js"
            ]

    match (fromList projectsStaticFiles) $ do
        route dropContentRoute
        compile copyFileCompiler

    match "content/zurihac2019/projects.html" $ do
        route dropContentRoute
        compile $
            getResourceBody >>=
            loadAndApplyTemplate "templates/zurihac2019.html" zfohContext

    match "content/zurihac2020/projects.html" $ do
        route dropContentRoute
        compile $
            getResourceBody >>=
            loadAndApplyTemplate "templates/zurihac2020.html" zfohContext

    match "content/zurihac2021/projects.html" $ do
        route dropContentRoute
        compile $
            getResourceBody >>=
            loadAndApplyTemplate "templates/zurihac2021.html" zfohContext

    ----------------------------------------------------------------------------
    -- Sections that we can pull in from anywhere, they're just strings.

    match "content/sections/*.html" $ compile getResourceBody
    match "content/zurihac2019/sections/*.html" $ compile getResourceBody
    match "content/zurihac2020/sections/*.html" $ compile getResourceBody
    match "content/zurihac2021/sections/*.html" $ compile getResourceBody
    match "content/zurihac2021/sections/*.js" $ compile getResourceBody

    ----------------------------------------------------------------------------
    -- Meetup section is dynamically generated.

    create ["content/sections/meetup.html"] $ do
        route idRoute
        compile $ unsafeCompiler getMeetups >>= makeItem

    ----------------------------------------------------------------------------
    -- Templates.

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
