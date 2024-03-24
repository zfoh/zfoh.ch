{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module Projects where

import qualified Data.Aeson          as A
import           Data.Binary         (Binary)
import           Data.List           (intercalate, sortOn)
import qualified Data.Text           as T
import           Hakyll

import           Projects.Definition

newtype Projects = Projects [Project] deriving (Binary, A.FromJSON)

instance Writable Projects where
    write _ _ = pure ()

projectsContext :: Projects -> Context a
projectsContext (Projects projects) =
    listField "projects" projectContext . traverse makeItem $
    sortOn sortKey projects
  where
    projectContext =
        field "name" (pure . T.unpack . project_name . itemBody) <>
        field "link" (maybe (fail "no link") (pure . T.unpack) . project_link . itemBody) <>
        field "contact" (pure . T.unpack . project_contact . itemBody) <>
        field "description" (maybe (fail "no description") (pure . T.unpack) . project_description . itemBody) <>
        field "contributorLevel" (pure . level . project_contributorLevel . itemBody)

    level cl = intercalate ", " $
        ["beginner"     | projectContributorLevel_beginner     cl] ++
        ["intermediate" | projectContributorLevel_intermediate cl] ++
        ["advanced"     | projectContributorLevel_advanced     cl]

    sortKey = T.toLower . project_name
