{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module ZuriHac.Projects
    ( Projects
    , compileProjects
    , renderProjects
    ) where

import           Control.Monad                   (forM_)
import qualified Data.Aeson                      as Aeson
import           Data.Binary                     (Binary)
import qualified Data.Text                       as T
import           GHC.Generics                    (Generic)
import           Hakyll
import qualified Text.Blaze.Html.Renderer.String as H
import qualified Text.Blaze.Html5                as H
import qualified Text.Blaze.Html5.Attributes     as HA

newtype Projects = Projects [Project]
    deriving (Eq, Binary, Aeson.FromJSON, Generic, Show)

instance Writable Projects where
    write _ _ = return ()

data Project = Project
    { pName             :: !T.Text
    , pLink             :: !(Maybe T.Text)
    , pContributorLevel :: !T.Text
    , pContact          :: !T.Text
    , pDescription      :: !T.Text
    } deriving (Eq, Generic, Show)

instance Binary Project

instance Aeson.FromJSON Project where
    parseJSON = Aeson.withObject "FromJSON Project" $ \o -> Project
        <$> o Aeson..:  "name"
        <*> o Aeson..:? "link"
        <*> o Aeson..:  "contributor level"
        <*> o Aeson..:  "contact"
        <*> o Aeson..:  "description"

compileProjects :: Compiler (Item Projects)
compileProjects = do
    body <- getResourceLBS
    either fail makeItem $ Aeson.eitherDecode (itemBody body)

renderProjects :: Projects -> String
renderProjects (Projects ps) = H.renderHtml $
    H.ul H.! HA.class_ "projects" $ forM_ ps $ \p -> H.li $ do
        case pLink p of
            Nothing -> H.p H.! HA.class_ "name" $ H.toHtml (pName p)
            Just l -> H.a H.! HA.class_ "name" H.! HA.href (H.toValue l) $
                H.toHtml (pName p)
        H.p $
            H.strong "Contributor levels:" <> " " <>
            H.toHtml (pContributorLevel p)
        H.p $ H.strong "Contact" <> " " <> H.toHtml (pContact p)
        H.p $ H.toHtml (pDescription p)
