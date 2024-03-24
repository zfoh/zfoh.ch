module AesonOptions (options) where

import qualified Data.Aeson.TH as A

options :: A.Options
options = A.defaultOptions {A.fieldLabelModifier = drop 1 . dropWhile (/= '_')}
