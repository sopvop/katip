{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Katip.Scribes.HandleUtf8 where

-------------------------------------------------------------------------------
import           Control.Applicative     as A
import           Control.Concurrent
import           Control.Exception       (bracket_)
import           Control.Monad
import           Data.Aeson
import           Data.ByteString.Builder
import qualified Data.ByteString.Lazy    as B
import qualified Data.HashMap.Strict     as HM
import           Data.Monoid
import           Data.Text               (Text)
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as T
import qualified Data.Text.Lazy.Builder  as LT
import qualified Data.Text.Lazy.Encoding as LT
import           System.IO
import           System.Posix.Types      (CPid (..))
-------------------------------------------------------------------------------
import           Katip.Core
import           Katip.Format.Time       (formatAsLogTime)
import           Katip.Scribes.Handle    (ColorStrategy (..))
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
brackets :: Builder -> Builder
brackets m = char7 '[' <> m <> char7 ']'
{-# INLINE brackets #-}

-------------------------------------------------------------------------------
getKeys :: LogItem s => Verbosity -> s -> Builder
getKeys verb a = foldPairs mempty $ HM.toList (payloadObject verb a)
  where
    renderPair :: Builder -> (Text, Value) -> Builder
    renderPair prefix (k,v) =
      case v of
        Object o -> foldPairs (keyB <> char7 '.') $ HM.toList o
        String t -> brackets $ keyB <> char7 ':' <> fromText t
        Number n -> brackets $ keyB <> char7 ':' <> fromString (show n) -- bad
        Bool   b -> brackets $ keyB <> char7 ':' <> fromString (show b)
        Null     -> brackets $ keyB <> ":null"
        _ -> mempty -- Can't think of a sensible way to handle arrays
      where
        keyB = prefix <> fromText k

    foldPairs prefix = foldr (go prefix) mempty
    go prefix (key,value) acc = renderPair prefix (key,value) <> acc

fromText :: Text -> Builder
fromText = T.encodeUtf8Builder

fromTextBuilder :: LT.Builder -> Builder
fromTextBuilder = lazyByteString . LT.encodeUtf8 . LT.toLazyText

fromString :: String -> Builder
fromString = stringUtf8
{-# INLINE fromString #-}
{-# INLINE fromTextBuilder #-}
{-# INLINE fromText #-}

fromNS :: Namespace -> Builder
fromNS (Namespace []) = mempty
fromNS (Namespace [h]) = fromText h
fromNS (Namespace hs) = foldr (\h acc -> fromText h <> char7 '.' <> acc) mempty hs
{-# INLINE fromNS #-}
-------------------------------------------------------------------------------
-- | Logs to a file handle such as stdout, stderr, or a file. Contexts
-- and other information will be flattened out into bracketed
-- fields. For example:
--
-- > [2016-05-11 21:01:15][MyApp][Info][myhost.example.com][1724][ThreadId 1154][main:Helpers.Logging Helpers/Logging.hs:32:7] Started
-- > [2016-05-11 21:01:15][MyApp.confrabulation][Debug][myhost.example.com][1724][ThreadId 1154][confrab_factor:42.0][main:Helpers.Logging Helpers/Logging.hs:41:9] Confrabulating widgets, with extra namespace and context
-- > [2016-05-11 21:01:15][MyApp][Info][myhost.example.com][1724][ThreadId 1154][main:Helpers.Logging Helpers/Logging.hs:43:7] Namespace and context are back to normal
mkHandleScribe :: ColorStrategy -> Handle -> Severity -> Verbosity -> IO Scribe
mkHandleScribe cs h sev verb = do
    hSetBinaryMode h True
    hSetBuffering h (BlockBuffering Nothing)
    colorize <- case cs of
      ColorIfTerminal -> hIsTerminalDevice h
      ColorLog b -> return b
    lock <- newMVar ()
    return $ Scribe $ \ i@Item{..} -> do
      when (permitItem sev i) $ bracket_ (takeMVar lock) (putMVar lock ()) $ do
        hPutBuilder h $ (formatItem colorize verb i <> char7 '\n')
        hFlush h



-------------------------------------------------------------------------------
formatItem :: LogItem a => Bool -> Verbosity -> Item a -> Builder
formatItem withColor verb Item{..} =
    brackets nowStr <>
    brackets (fromNS _itemNamespace) <>
    brackets (renderSeverity' _itemSeverity) <>
    brackets (fromString _itemHost) <>
    brackets (case _itemProcess of CPid p -> int32Dec p) <>
    brackets (fromText (getThreadIdText _itemThread)) <>
    ks <>
    maybe mempty (brackets . fromString . locationToString) _itemLoc <>
    fromText " " <> (fromTextBuilder $ unLogStr _itemMessage)
  where
    nowStr = fromText (formatAsLogTime _itemTime)
    ks = getKeys verb _itemPayload
    renderSeverity' s = case s of
      EmergencyS -> red $ renderSeverityB s
      AlertS     -> red $ renderSeverityB s
      CriticalS  -> red $ renderSeverityB s
      ErrorS     -> red $ renderSeverityB s
      WarningS   -> yellow $ renderSeverityB s
      _         -> renderSeverityB s
    red = colorize "31"
    yellow = colorize "33"
    colorize c s
      | withColor = "\ESC["<> c <> "m" <> s <> "\ESC[0m"
      | otherwise = s


-------------------------------------------------------------------------------
renderSeverityB :: Severity -> Builder
renderSeverityB s = case s of
      DebugS -> byteString "Debug"
      InfoS -> byteString "Info"
      NoticeS -> byteString "Notice"
      WarningS -> byteString "Warning"
      ErrorS -> byteString "Error"
      CriticalS -> byteString "Critical"
      AlertS -> byteString "Alert"
      EmergencyS -> byteString "Emergency"
