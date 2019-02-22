-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE OverloadedStrings #-}

module System.Logger.Settings
    ( Settings
    , Level      (..)
    , Output     (..)
    , DateFormat (..)
    , Renderer

    , defSettings
    , output
    , setOutput
    , format
    , setFormat
    , bufSize
    , setBufSize
    , delimiter
    , setDelimiter
    , setNetStrings
    , setRendererDefault
    , setRendererNetstr
    , renderDefault
    , renderNetstr
    , canonicalizeWhitespace
    , logLevel
    , logLevelMap
    , logLevelOf
    , setLogLevel
    , setLogLevelMap
    , setLogLevelOf
    , name
    , setName
    , nameMsg
    , renderer
    , setRenderer
    , iso8601UTC
    ) where

import Data.String
import Data.Char (isSpace)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Data.Map.Strict as Map
import Data.Text (Text)
import Data.UnixTime
import System.Log.FastLogger (defaultBufSize)
import System.Logger.Message

import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Lazy.Builder as B

data Settings = Settings
    { _logLevel   :: !Level              -- ^ messages below this log level will be suppressed
    , _levelMap   :: !(Map Text Level)   -- ^ log level per named logger
    , _output     :: !Output             -- ^ log sink
    , _format     :: !(Maybe DateFormat) -- ^ the timestamp format (use 'Nothing' to disable timestamps)
    , _delimiter  :: !ByteString         -- ^ text to intersperse between fields of a log line
    , _bufSize    :: !Int                -- ^ how many bytes to buffer before commiting to sink
    , _name       :: !(Maybe Text)       -- ^ logger name
    , _nameMsg    :: !(Msg -> Msg)
    , _renderer   :: !Renderer
    }

output :: Settings -> Output
output = _output

setOutput :: Output -> Settings -> Settings
setOutput x s = s { _output = x }

-- | The time and date format used for the timestamp part of a log line.
format :: Settings -> Maybe DateFormat
format = _format

setFormat :: Maybe DateFormat -> Settings -> Settings
setFormat x s = s { _format = x }

bufSize :: Settings -> Int
bufSize = _bufSize

setBufSize :: Int -> Settings -> Settings
setBufSize x s = s { _bufSize = max 1 x }

-- | Delimiter string which separates log line parts.
delimiter :: Settings -> ByteString
delimiter = _delimiter

setDelimiter :: ByteString -> Settings -> Settings
setDelimiter x s = s { _delimiter = x }

-- | Whether to use <http://cr.yp.to/proto/netstrings.txt netstring>
-- encoding for log lines.  Loads 'renderDefault' if given 'False'.
--
-- {#- DEPRECATED setNetStrings "Use setRendererNetstr or setRendererDefault instead" #-}
setNetStrings :: Bool -> Settings -> Settings
setNetStrings True  = setRendererNetstr
setNetStrings False = setRendererDefault

-- | Shortcut for calling 'setRenderer' with 'renderDefault'.
setRendererDefault :: Settings -> Settings
setRendererDefault = setRenderer renderDefault

-- | Shortcut for calling 'setRenderer' with 'renderNetstr'.
setRendererNetstr :: Settings -> Settings
setRendererNetstr = setRenderer renderNetstr

-- | Simple 'Renderer' with '=' between field names and values and a custom
-- separator.
renderDefault :: Renderer
renderDefault s _ _ = renderDefault_ s

-- | 'Renderer' that uses <http://cr.yp.to/proto/netstrings.txt netstring>
-- encoding for log lines.
renderNetstr :: Renderer
renderNetstr _ _ _ = renderNetstr_

-- | Replace all whitespace characters in the output of a renderer by @' '@.
-- Log output must be ASCII encoding.
--
-- (Many logging processors handle newlines poorly.  Instead of hunting down all
-- places and situations in your code and your dependencies that inject newlines
-- into your log messages, you can choose to call 'canonicalizeWhitespace' on
-- your renderer.)
canonicalizeWhitespace :: Renderer -> Renderer
canonicalizeWhitespace rndrRaw delim df lvl
  = B.lazyByteString . nl2sp . B.toLazyByteString . rndrRaw delim df lvl
  where
    nl2sp :: L.ByteString -> L.ByteString
    nl2sp = L.concatMap $
      \c -> if isSpace c
            then " "
            else L.singleton c

logLevel :: Settings -> Level
logLevel = _logLevel

setLogLevel :: Level -> Settings -> Settings
setLogLevel x s = s { _logLevel = x }

-- | Log level of some named logger.
logLevelOf :: Text -> Settings -> Maybe Level
logLevelOf x s = Map.lookup x (_levelMap s)

logLevelMap :: Settings -> Map Text Level
logLevelMap = _levelMap

-- | Specify a log level for the given named logger. When a logger is
-- 'clone'd and given a name, the 'logLevel' of the cloned logger will be
-- the provided here.
setLogLevelOf :: Text -> Level -> Settings -> Settings
setLogLevelOf n x s = s { _levelMap = Map.insert n x (_levelMap s) }

setLogLevelMap :: Map Text Level -> Settings -> Settings
setLogLevelMap x s = s { _levelMap = x }

name :: Settings -> Maybe Text
name = _name

setName :: Maybe Text -> Settings -> Settings
setName Nothing   s = s { _name = Nothing, _nameMsg = id }
setName (Just xs) s = s { _name = Just xs, _nameMsg = "logger" .= xs }

nameMsg :: Settings -> (Msg -> Msg)
nameMsg = _nameMsg

-- | Output format
renderer :: Settings -> Renderer
renderer = _renderer

-- | Set a custom renderer.  See 'setRendererDefault', 'setRendererNetstr' for
-- two common special cases.  Look at the code of 'renderDefault',
-- 'renderNetstr' for examples how to write custom renderers.
setRenderer :: Renderer -> Settings -> Settings
setRenderer f s = s { _renderer = f }

data Level
    = Trace
    | Debug
    | Info
    | Warn
    | Error
    | Fatal
    deriving (Eq, Ord, Read, Show)

data Output
    = StdOut
    | StdErr
    | Path FilePath
    deriving (Eq, Ord, Show)

newtype DateFormat = DateFormat
    { display :: UnixTime -> ByteString
    }

instance IsString DateFormat where
    fromString = DateFormat . formatUnixTimeGMT . pack

-- | ISO 8601 date-time format.
iso8601UTC :: DateFormat
iso8601UTC = "%Y-%0m-%0dT%0H:%0M:%0SZ"

-- | Take a custom separator, date format, log level of the event, and render
-- a list of log fields or messages into a builder.
--
-- See also: 'renderDefault', 'renderNetstr'.
type Renderer = ByteString -> DateFormat -> Level -> [Element] -> B.Builder

-- | Default settings:
--
--   * 'logLevel'   = 'Debug'
--
--   * 'output'     = 'StdOut'
--
--   * 'format'     = 'iso8601UTC'
--
--   * 'delimiter'  = \", \"
--
--   * 'netstrings' = False
--
--   * 'bufSize'    = 'FL.defaultBufSize'
--
--   * 'name'       = Nothing
--
defSettings :: Settings
defSettings = Settings
    Debug
    Map.empty
    StdOut
    (Just iso8601UTC)
    ", "
    defaultBufSize
    Nothing
    id
    renderDefault
