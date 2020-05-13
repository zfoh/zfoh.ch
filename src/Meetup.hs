{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Meetup where

import           Control.Monad                   (forM_)
import qualified Data.Aeson                      as Aeson
import qualified Data.List                       as List
import qualified Data.List.NonEmpty              as NonEmpty
import           Data.Ord                        (Down (..))
import qualified Data.Text                       as T
import qualified Data.Time                       as Time
import qualified Network.HTTP.Simple             as Http
import qualified Text.Blaze.Html.Renderer.String as H
import qualified Text.Blaze.Html5                as H
import qualified Text.Blaze.Html5.Attributes     as HA

data Status = Past | Upcoming deriving (Eq, Show)

instance Aeson.FromJSON Status where
    parseJSON = Aeson.withText "FromJSON Status" $ \t -> case t of
        "past"     -> return Past
        "upcoming" -> return Upcoming
        _          -> fail $ "Unknown meetup status: " ++ show t

newtype MeetupTime = MeetupTime {unMeetupTime :: Time.LocalTime}
    deriving (Eq, Ord, Show)

parseMeetupTime :: T.Text -> T.Text -> Maybe MeetupTime
parseMeetupTime localDate localTime = fmap MeetupTime $ Time.parseTimeM
    True
    Time.defaultTimeLocale
    "%Y-%m-%d %H:%M"
    (T.unpack $ localDate <> " " <> localTime)

data Meetup = Meetup
    { mName        :: !T.Text
    , mStatus      :: !Status
    , mLink        :: !T.Text
    , mTime        :: !MeetupTime
    , mDescription :: !T.Text
    } deriving (Show)

instance Aeson.FromJSON Meetup where
    parseJSON = Aeson.withObject "FromJSON Meetup" $ \o -> Meetup
        <$> o Aeson..: "name"
        <*> o Aeson..: "status"
        <*> o Aeson..: "link"
        <*> (do
                date <- o Aeson..: "local_date"
                time <- o Aeson..: "local_time"
                maybe (fail "Invalid date") return (parseMeetupTime date time))
        <*> o Aeson..: "description"

loadMeetups :: IO (NonEmpty.NonEmpty Meetup)
loadMeetups = do
    rsp     <- Http.getResponseBody <$> Http.httpLbs url
    meetups <- either fail return (Aeson.eitherDecode rsp)
    let (past, upcoming) = List.partition ((== Past) . mStatus) meetups
        list             =
            take 1 (List.sortOn mTime upcoming) ++
            List.sortOn (Down . mTime) past

    case list of
        []       -> fail "No meetups found"
        (x : xs) -> return $ x NonEmpty.:| xs
  where
    url = "http://api.meetup.com/HaskellerZ/events?page=10&status=upcoming,past&desc=true"

renderMeetups :: NonEmpty.NonEmpty Meetup -> H.Html
renderMeetups (m0 NonEmpty.:| meetups) =
    H.div H.! HA.class_ "twocolumn" $ do
        H.div H.! HA.class_ "left" $ do
            H.h2 $ case mStatus m0 of
                Past     -> "Last meetup"
                Upcoming -> "Next meetup"
            H.a H.! HA.href (H.toValue $ mLink m0) $ H.toHtml $ mName m0
            H.p H.! HA.class_ "meetup-time" $ H.toHtml $ Time.formatTime
                Time.defaultTimeLocale
                "%A %e %B, %H:%M"
                (unMeetupTime $ mTime m0)
            H.preEscapedToHtml (mDescription m0)
        H.div H.! HA.class_ "right past-meetups" $ do
            H.h3 $ "Past meetups"
            H.ul $ forM_ meetups $ \m -> H.li $
                H.a H.! HA.href (H.toValue $ mLink m) $ H.toHtml $ mName m

getMeetups :: IO String
getMeetups = H.renderHtml . renderMeetups <$> loadMeetups
