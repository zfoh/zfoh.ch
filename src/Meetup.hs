{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Meetup where

import qualified Data.Aeson                      as Aeson
import           Data.Monoid                     ((<>))
import qualified Data.Text                       as T
import qualified Data.Time                       as Time
import qualified Network.HTTP.Simple             as Http
import qualified Text.Blaze.Html.Renderer.String as H
import qualified Text.Blaze.Html5                as H
import qualified Text.Blaze.Html5.Attributes     as HA

newtype MeetupTime = MeetupTime {unMeetupTime :: Time.LocalTime} deriving (Show)

parseMeetupTime :: T.Text -> T.Text -> Maybe MeetupTime
parseMeetupTime localDate localTime = fmap MeetupTime $ Time.parseTimeM
    True
    Time.defaultTimeLocale
    "%Y-%m-%d %H:%M"
    (T.unpack $ localDate <> " " <> localTime)

data MeetupEvent = MeetupEvent
    { meName        :: !T.Text
    , meLink        :: !T.Text
    , meTime        :: !MeetupTime
    , meDescription :: !T.Text
    } deriving (Show)

instance Aeson.FromJSON MeetupEvent where
    parseJSON = Aeson.withObject "FromJSON MeetupEvent" $ \o -> MeetupEvent
        <$> o Aeson..: "name"
        <*> o Aeson..: "link"
        <*> (do
                date <- o Aeson..: "local_date"
                time <- o Aeson..: "local_time"
                maybe (fail "Invalid date") return (parseMeetupTime date time))
        <*> o Aeson..: "description"

renderMeetupEvent :: MeetupEvent -> H.Html
renderMeetupEvent MeetupEvent {..} = H.div $ do
    H.a H.! HA.href (H.toValue meLink) $ H.toHtml meName
    H.p H.! HA.class_ "meetup-time" $ H.toHtml $ Time.formatTime
        Time.defaultTimeLocale
        "%A %e %B, %H:%M"
        (unMeetupTime meTime)
    H.preEscapedToHtml meDescription

getMeetup :: IO String
getMeetup = do
    rsp <- Http.getResponseBody <$> Http.httpLbs url
    case Aeson.eitherDecode rsp of
        Left err      -> fail err
        Right []      -> fail "No meetups found"
        Right (m : _) -> return $ H.renderHtml $ renderMeetupEvent m
  where
    url = "https://api.meetup.com/HaskellerZ/events?scroll=future_or_past"
