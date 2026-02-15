{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Aeson       as A
import           Data.Foldable    (forM_)
import qualified Data.Time        as Time
import           Hakyll
import           Meetup
import           System.FilePath  (joinPath, splitPath)
import qualified ZuriHac.Projects as Projects

main :: IO ()
main = hakyll $ do

    ----------------------------------------------------------------------------
    -- Images, CSS, etc.

    match ("images/**.png" .||. "images/**.jpg" .||. "images/**.gif"
            .||. "images/**.svg" .||. "images/**.webp") $ do
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
    -- Main pages.

    let zurihacs =
            [ ("content/zurihac2019/index.html", "templates/zurihac2019.html")
            , ("content/zurihac2020/index.html", "templates/zurihac2020.html")
            , ("content/zurihac2021/index.html", "templates/zurihac2021.html")
            , ("content/zurihac2022/index.html", "templates/zurihac2022.html")
            , ("content/zurihac2023/index.html", "templates/zurihac2023.html")
            , ("content/zurihac2024/index.html", "templates/zurihac2024.html")
            , ("content/zurihac2025/index.html", "templates/zurihac2025.html")
            , ("content/zurihac2026/index.html", "templates/zurihac2026.html")
            ]

    forM_ zurihacs $ \(index, tpl) -> match index $ do
        route dropContentRoute
        compile $
            getResourceBody >>=
            applyAsTemplate sectionContext >>=
            loadAndApplyTemplate tpl zfohContext

    ----------------------------------------------------------------------------
    -- Projects page.

    let projectsStaticFiles =
            [ "content/zurihac2019/projects.json"
            , "content/zurihac2019/projects/*"
            -- 2020
            , "content/zurihac2020/projects.json"
            , "content/zurihac2020/projects/*"
            -- 2021
            , "content/zurihac2021/projects.json"
            , "content/zurihac2021/projects/*"
            -- 2022
            , "content/zurihac2022/projects.json"
            , "content/zurihac2022/projects/*"
            -- 2023
            , "content/zurihac2023/schedule.js"  -- Not really related to projects
            , "content/zurihac2023/3d/index.html"  -- Not really related to projects
            -- 2024
            , "content/zurihac2024/logo/*"  -- Not really related to projects
            ]

    forM_ projectsStaticFiles $ \pattern -> match pattern $ do
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

    match "content/zurihac2022/projects.html" $ do
        route dropContentRoute
        compile $
            getResourceBody >>=
            loadAndApplyTemplate "templates/zurihac2022.html" zfohContext

    ----------------------------------------------------------------------------
    -- Sections that we can pull in from anywhere, they're just strings.

    match "content/sections/*.html" $ compile getResourceBody
    match "content/zurihac2019/sections/*.html" $ compile getResourceBody
    match "content/zurihac2020/sections/*.html" $ compile getResourceBody
    match "content/zurihac2021/sections/*.html" $ compile getResourceBody
    match "content/zurihac2021/sections/*.js" $ compile getResourceBody

    ----------------------------------------------------------------------------
    -- New project pages.
    --
    -- TODO: Clean up.

    match "content/zurihac2023/projects/projects.json" $
        compile $ do
            body <- itemBody <$> getResourceLBS
            case A.eitherDecode body of
                Right projects -> makeItem (projects :: Projects.Projects)
                Left err       -> fail err
    match "content/zurihac2023/projects/index.html" $ do
        route dropContentRoute
        compile $ do
            projects <- loadBody "content/zurihac2023/projects/projects.json"
            html <- getResourceBody >>=
                applyAsTemplate (Projects.projectsContext projects)
            loadAndApplyTemplate "templates/zurihac2023.html" zfohContext html

    match "content/zurihac2024/projects/projects.json" $
        compile $ do
            body <- itemBody <$> getResourceLBS
            case A.eitherDecode body of
                Right projects -> makeItem (projects :: Projects.Projects)
                Left err       -> fail err
    match "content/zurihac2024/projects/index.html" $ do
        route dropContentRoute
        compile $ do
            projects <- loadBody "content/zurihac2024/projects/projects.json"
            html <- getResourceBody >>=
                applyAsTemplate (Projects.projectsContext projects)
            loadAndApplyTemplate "templates/zurihac2024.html" zfohContext html

    match "content/zurihac2025/projects/projects.json" $
        compile $ do
            body <- itemBody <$> getResourceLBS
            case A.eitherDecode body of
                Right projects -> makeItem (projects :: Projects.Projects)
                Left err       -> fail err
    match "content/zurihac2025/projects/index.html" $ do
        route dropContentRoute
        compile $ do
            projects <- loadBody "content/zurihac2025/projects/projects.json"
            html <- getResourceBody >>=
                applyAsTemplate (Projects.projectsContext projects)
            loadAndApplyTemplate "templates/zurihac2025.html" zfohContext html

    ----------------------------------------------------------------------------
    -- Meetup section is dynamically generated.

    create ["content/sections/meetup.html"] $ do
        route idRoute
        compile $ unsafeCompiler getMeetups >>= makeItem

    ----------------------------------------------------------------------------
    -- /donate redirect

    createRedirects
        [("donate/index.html", "https://pay.sumup.com/b2c/QUWNA9F5")]

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
