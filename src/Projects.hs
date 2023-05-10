{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module Projects where

import qualified Data.Aeson   as A
import           Data.Binary  (Binary)
import           Data.Char    (toLower)
import           Data.List    (sortOn)
import           Data.List    (intercalate)
import           GHC.Generics (Generic)
import           Hakyll

data ContributorLevel = ContributorLevel
    { clBeginner     :: !Bool
    , clIntermediate :: !Bool
    , clAdvanced     :: !Bool
    } deriving (Generic, Show)

data Project = Project
    { pName             :: String
    , pLink             :: Maybe String
    , pContact          :: String
    , pContributorLevel :: !ContributorLevel
    , pDescription      :: Maybe String
    } deriving (Generic, Show)

instance Binary ContributorLevel
instance Binary Project

instance A.FromJSON ContributorLevel where
    parseJSON = A.withObject "ContributorLevel" $ \obj -> ContributorLevel
        <$> obj A..:? "beginner" A..!= False
        <*> obj A..:? "intermediate" A..!= False
        <*> obj A..:? "advanced" A..!= False

instance A.FromJSON Project where
    parseJSON = A.withObject "Project" $ \obj -> Project
        <$> obj A..:  "name"
        <*> obj A..:? "link"
        <*> obj A..:  "contact"
        <*> obj A..:  "contributorLevel"
        <*> obj A..:? "description"

newtype Projects = Projects [Project] deriving (Binary, A.FromJSON)

instance Writable Projects where
    write _ _ = pure ()

projectsContext :: Projects -> Context a
projectsContext (Projects projects) =
    listField "projects" projectContext . traverse makeItem $
    sortOn sortKey projects
  where
    projectContext =
        field "name" (pure . pName . itemBody) <>
        field "link" (maybe (fail "no link") pure . pLink . itemBody) <>
        field "contact" (pure . pContact. itemBody) <>
        field "description" (maybe (fail "no description") pure . pDescription. itemBody) <>
        field "contributorLevel" (pure . level . pContributorLevel . itemBody)

    level cl = intercalate ", " $
        ["beginner" | clBeginner cl] ++
        ["intermediate" | clIntermediate cl] ++
        ["advanced" | clAdvanced cl]

    sortKey = map toLower . pName
