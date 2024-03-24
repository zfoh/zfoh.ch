-- | This is a program to convert between the zurihac registrations JSON export
-- format, and the project list on the website JSON input format.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
import           Control.Monad        (guard)
import qualified Data.Aeson           as A
import qualified Data.Aeson.TH        as A
import qualified Data.ByteString.Lazy as LB
import           Data.Char            (isPunctuation)
import           Data.List            (sortOn)
import           Data.Maybe           (mapMaybe)
import qualified Data.Text            as T

import qualified AesonOptions         as A
import           Projects.Definition

--------------------------------------------------------------------------------

data Registration = Registration
    { registration_info           :: RegistrationInfo
    , registration_state          :: T.Text
    , registration_additionalInfo :: RegistrationAdditionalInfo
    } deriving (Show)

data RegistrationInfo = RegistrationInfo
    { registrationInfo_name :: T.Text
    } deriving (Show)

data RegistrationAdditionalInfo = RegistrationAdditionalInfo
    { registrationAdditionalInfo_project :: RegistrationProject
    } deriving (Show)

data RegistrationProject = RegistrationProject
    { registrationProject_name             :: Maybe T.Text
    , registrationProject_website          :: Maybe T.Text
    , registrationProject_shortDescription :: Maybe T.Text
    , registrationProject_contributorLevel :: RegistrationContributorLevel
    } deriving (Show)

data RegistrationContributorLevel = RegistrationContributorLevel
    { registrationContributorLevel_beginner     :: Bool
    , registrationContributorLevel_intermediate :: Bool
    , registrationContributorLevel_advanced     :: Bool
    } deriving (Show)

$(A.deriveFromJSON A.options ''RegistrationContributorLevel)
$(A.deriveFromJSON A.options ''RegistrationProject)
$(A.deriveFromJSON A.options ''RegistrationAdditionalInfo)
$(A.deriveFromJSON A.options ''RegistrationInfo)
$(A.deriveFromJSON A.options ''Registration)

--------------------------------------------------------------------------------

convert :: Registration -> Maybe Project
convert Registration {..} = do
    name <- registrationProject_name
    guard $ registration_state `elem` ["Confirmed", "Registered"]
    guard . not $ T.toLower (T.filter (not . isPunctuation) name) `elem`
        ["no", "not yet"]
    pure Project
        { project_name        = name
        , project_contact     = registrationInfo_name
        , project_link        = registrationProject_website
        , project_description = registrationProject_shortDescription
        , project_contributorLevel = ProjectContributorLevel
            { projectContributorLevel_beginner     = registrationContributorLevel_beginner
            , projectContributorLevel_intermediate = registrationContributorLevel_intermediate
            , projectContributorLevel_advanced     = registrationContributorLevel_advanced
            }
        }
  where
    RegistrationInfo {..}             = registration_info
    RegistrationAdditionalInfo {..}   = registration_additionalInfo
    RegistrationProject {..}          = registrationAdditionalInfo_project
    RegistrationContributorLevel {..} = registrationProject_contributorLevel

--------------------------------------------------------------------------------

main :: IO ()
main = do
    contents <- LB.getContents
    registrations <- either fail pure $ A.eitherDecode contents
    LB.putStr $ A.encode $ sortOn (T.toLower . project_name) $
        mapMaybe convert registrations
