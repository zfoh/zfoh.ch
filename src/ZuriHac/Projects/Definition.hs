{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
module ZuriHac.Projects.Definition
    ( Project (..)
    , ProjectContributorLevel (..)
    ) where

import qualified Data.Aeson.TH as A
import           Data.Binary   (Binary)
import qualified Data.Text     as T
import           GHC.Generics  (Generic)

import           AesonOptions  as A

data Project = Project
    { project_name             :: T.Text
    , project_contact          :: T.Text
    , project_description      :: Maybe T.Text
    , project_link             :: Maybe T.Text
    , project_contributorLevel :: ProjectContributorLevel
    } deriving (Generic, Show)

data ProjectContributorLevel = ProjectContributorLevel
    { projectContributorLevel_beginner     :: Bool
    , projectContributorLevel_intermediate :: Bool
    , projectContributorLevel_advanced     :: Bool
    } deriving (Generic, Show)

instance Binary ProjectContributorLevel
instance Binary Project

$(A.deriveJSON A.options ''ProjectContributorLevel)
$(A.deriveJSON A.options ''Project)
