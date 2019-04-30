{-# LANGUAGE OverloadedStrings #-}

import           Data.Monoid      ((<>))
import qualified Data.Time        as Time
import           Hakyll
import           Meetup
import           System.Exit      (ExitCode (..))
import           System.FilePath  (joinPath, splitPath)
import qualified System.Process   as Process
import           ZuriHac.Projects

main :: IO ()
main = hakyll $ do

    match ("images/**.png" .||. "images/**.jpg" .||. "images/**.gif") $ do
        route idRoute
        compile copyFileCompiler

    match "images/s43/*.svg" $ inkscapeRules (Nothing, Just 120)
    match "images/icons/*.svg" $ inkscapeRules (Just 64, Just 64)

    match "css/*" $ do
        route idRoute
        compile compressCssCompiler

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

    match "content/zurihac2019/index.html" $ do
        route dropContentRoute
        compile $
            getResourceBody >>=
            applyAsTemplate sectionContext >>=
            loadAndApplyTemplate "templates/zurihac2019.html" zfohContext

    match "content/zurihac2019/projects.json" $ compile compileProjects
    match "content/zurihac2019/projects.html" $ do
        route dropContentRoute
        compile $
            getResourceBody >>=
            applyAsTemplate projectsContext >>=
            loadAndApplyTemplate "templates/zurihac2019.html" projectsContext

    match "content/sections/*.html" $ compile getResourceBody
    match "content/zurihac2019/sections/*.html" $ compile getResourceBody

    create ["content/sections/meetup.html"] $ do
        route idRoute
        compile $ unsafeCompiler getMeetups >>= makeItem

    match "templates/*.html" $ compile templateCompiler

inkscapeRules :: (Maybe Int, Maybe Int) -> Rules ()
inkscapeRules wh = do
    route $ setExtension "png"
    compile $ inkscapeCompiler wh

inkscapeCompiler :: (Maybe Int, Maybe Int) -> Compiler (Item TmpFile)
inkscapeCompiler (w, h) = do
    filePath          <- toFilePath <$> getUnderlying
    png@(TmpFile tmp) <- newTmpFile "inkscape"
    exitCode <- unsafeCompiler $ Process.rawSystem "inkscape" $
        ["-e", tmp] ++
        (case w of Nothing -> []; Just w' -> ["-w", show w']) ++
        (case h of Nothing -> []; Just h' -> ["-h", show h']) ++
        [filePath]
    case exitCode of
        ExitSuccess   -> return ()
        ExitFailure e -> fail $ "inkscape exit code " ++ show e
    makeItem png

sectionContext :: Context String
sectionContext =
    functionField "section" (\[name] _ -> loadBody (fromFilePath name)) <>
    zfohContext

projectsContext :: Context String
projectsContext =
    field "projects" (\_ -> do
        ps <- loadBody "content/zurihac2019/projects.json"
        return $ renderProjects ps) <>
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
