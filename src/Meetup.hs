-- | This module used to grab events from the meetup API at:
--
-- > https://api.meetup.com/HaskellerZ/events?page=10&status=upcoming,past&desc=true
--
-- However, this API has since been dropped, so we are now using the iCal export
-- instead...
module Meetup
    ( getMeetups
    ) where

import           Data.Bifunctor                  (first)
import qualified Data.ByteString.Lazy            as BL
import           Data.Char                       (isSpace)
import           Data.List                       (stripPrefix)
import           Data.Maybe                      (isNothing, mapMaybe)
import qualified Data.Text                       as T
import qualified Data.Text.Encoding              as T
import qualified Data.Time                       as Time
import qualified Hakyll.Web.Pandoc               as Hakyll
import qualified Network.HTTP.Client             as Http
import qualified Network.HTTP.Client.TLS         as Http
import qualified Text.Blaze.Html.Renderer.String as H
import qualified Text.Blaze.Html5                as H
import qualified Text.Blaze.Html5.Attributes     as HA
import qualified Text.Pandoc                     as Pandoc

type Property = (String, String)

-- Parses properties from ICS, should be good enough to grab the event
-- description and time.
parseProperties :: String -> [Property]
parseProperties = go . lines
  where
    go (l : ls) =
        let (key, val) = break (== ':') l
            -- Ignore extra attributes between ';' and ':'.
            key' = takeWhile (/= ';') key
            -- Read extra continuation lines if necessary.
            (cont, ls') = break (isNothing . continues) ls
            val' = unescape $ drop 1 val ++ concat (mapMaybe continues cont) in
        (key', val') : go ls'
    go [] = []

    -- A continuation line starts with a space or tab.
    continues (' ' : x)  = Just x
    continues ('\t' : x) = Just x
    continues _          = Nothing

    -- Unescaping values.
    unescape ('\\' : 'n' : t) = '\n' : unescape t
    unescape ('\\' : x : t)   = x : unescape t
    unescape ('\r' : t)       = unescape t
    unescape (x : t)          = x : unescape t
    unescape []               = []

data Event = Event
    { eventTime        :: Time.UTCTime
    , eventURL         :: String
    , eventSummary     :: String
    , eventDescription :: H.Html
    }

propertiesToEvents :: String -> [Property] -> Either String [Event]
propertiesToEvents calendarName props
    | null moreProps = pure []
    | otherwise      = do
        dtstart <- getProp "DTSTART"
        start <- maybe (Left "could not parse DTSTART") pure $
            Time.parseTimeM False Time.defaultTimeLocale "%Y%m%dT%H%M%S" dtstart
        url <- getProp "URL"
        summary <- getProp "SUMMARY"
        description <- getProp "DESCRIPTION" >>= md2html . cleanDescription
        (Event start url summary description :) <$>
            propertiesToEvents calendarName moreProps
  where
    (eventProps, moreProps) = break (== ("END", "VEVENT")) $
        dropWhile (/= ("BEGIN", "VEVENT")) props

    getProp k = maybe (Left $ k ++ " not found") pure $ lookup k eventProps

    md2html md = first show $ Pandoc.runPure $ do
        p <- Pandoc.readMarkdown Hakyll.defaultHakyllReaderOptions $ T.pack md
        Pandoc.writeHtml5 Hakyll.defaultHakyllWriterOptions p

    cleanDescription desc
        | Just desc' <- stripPrefix calendarName desc = dropWhile isSpace desc'
        | otherwise = desc

parseCalendar :: String -> Either String [Event]
parseCalendar input = do
    name <- maybe (Left "missing calendar NAME") pure $ lookup "NAME" props
    propertiesToEvents name props
  where
    props = parseProperties input

loadCalendar :: IO [Event]
loadCalendar = do
    manager <- Http.newTlsManager
    req     <- Http.parseRequest url
    rsp     <- Http.responseBody <$> Http.httpLbs req manager
    either fail pure $ parseCalendar $ T.unpack $ T.decodeUtf8 $
        BL.toStrict rsp
  where
    url = "https://www.meetup.com/HaskellerZ/events/ical/"

renderCalendar :: [Event] -> H.Html
renderCalendar calendar = do
    H.h2 "HaskellerZ meetups"
    case calendar of
        [] -> do
            "No meetups currently scheduled. Join our "
            H.a H.! HA.href "https://www.meetup.com/HaskellerZ/events/ical/" $
                "meetup group"
            " to be notified when we will meet."
        event : _ -> do
            H.p $ do
                "The next meetup takes place "
                H.toHtml $ Time.formatTime
                    Time.defaultTimeLocale
                    "%A %e %B, %H:%M"
                    (eventTime event)
                "."
            H.p $ H.a H.! HA.href (H.toValue $ eventURL event) $
                H.toHtml $ eventSummary event
            eventDescription event

getMeetups :: IO String
getMeetups = H.renderHtml . renderCalendar <$> loadCalendar
