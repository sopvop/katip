{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main
    ( main
    ) where


-------------------------------------------------------------------------------
import           Control.Concurrent
import           Control.DeepSeq
import           Criterion.Main
import           Data.Aeson
import qualified Data.ByteString.Lazy         as LB
import qualified Data.ByteString.Lazy.Builder as LB
import           Data.Monoid                  as M
import qualified Data.Text.Lazy               as LT
import qualified Data.Text.Lazy.Builder       as LT
import           Data.Time.Calendar
import           Data.Time.Clock
import           System.IO
import           System.Posix
-------------------------------------------------------------------------------
import           Katip.Core
import           Katip.Scribes.Handle
import qualified Katip.Scribes.HandleUtf8     as Utf8
-------------------------------------------------------------------------------

main :: IO ()
main = defaultMain [
    handleScribeBench
  ]


-------------------------------------------------------------------------------
handleScribeBench :: Benchmark
handleScribeBench = bgroup "Katip.Scribes.Handle" [
      env setupHandleEnv $ \ ~(Scribe push, tid) ->
      bench "Text Builder" $
        whnfIO $ push (exItem tid)

    , env setupUtf8 $ \ ~(Scribe push, tid) ->
      bench "Utf8 Builder" $
        whnfIO $ push (exItem tid)

    , env setupUtf8BS $ \ ~(Scribe push, tid) ->
      bench "ByteString Builder" $
        whnfIO $ push (exItem tid)

    , bench "TextFormat" $ nf formatBench item
    , bench "ByteStringFormat" $ nf formatBench8 item
    ]

item = exItem $ ThreadIdText "111"

items = replicate 100 (exItem $ ThreadIdText "111")

-------------------------------------------------------------------------------
setupHandleEnv :: IO (Scribe, ThreadIdText)
setupHandleEnv = do
      scribe <- setup
      tid <- myThreadId
      return (scribe, mkThreadIdText tid)


-------------------------------------------------------------------------------
exItem :: ThreadIdText -> Item ExPayload
exItem tid = Item {
      _itemApp = Namespace ["app"]
    , _itemEnv = Environment "production"
    , _itemSeverity = WarningS
    , _itemThread = tid
    , _itemHost = "example"
    , _itemProcess = CPid 123
    , _itemPayload = ExPayload
    , _itemMessage = "message"
    , _itemTime = mkUTCTime 2015 3 14 1 5 9
    , _itemNamespace = Namespace ["foo"]
    , _itemLoc = Nothing
    }


-------------------------------------------------------------------------------
formatBench :: Item ExPayload -> LT.Text
formatBench = LT.toLazyText . {- mconcat . map (-} formatItem True V3 --)
{-# NOINLINE formatBench #-}

formatBench8 :: Item ExPayload -> LB.ByteString
formatBench8 = LB.toLazyByteString . {- mconcat . map (-}Utf8.formatItem True V3--)
{-# NOINLINE formatBench8 #-}

-------------------------------------------------------------------------------
data ExPayload = ExPayload

instance ToJSON ExPayload where
  toJSON _ = --Object M.mempty
    object [ "int" .= (1337::Int)
           , "text" .= ("blah" :: LT.Text)
           , "another" .= (0 :: Int)
           , "deep" .= object ["deep" .= (3321::Int)
                              ,"teext" .= ("text" :: LT.Text) ]
           ]

instance ToObject ExPayload

instance LogItem ExPayload where
  payloadKeys _ _ = AllKeys


-------------------------------------------------------------------------------
mkUTCTime :: Integer -> Int -> Int -> DiffTime -> DiffTime -> DiffTime -> UTCTime
mkUTCTime y mt d h mn s = UTCTime day dt
  where
    day = fromGregorian y mt d
    dt = h * 60 * 60 + mn * 60 + s


-------------------------------------------------------------------------------
setup :: IO Scribe
setup = do
  h <- openFile "/dev/null" WriteMode
  mkHandleScribeUtf8 ColorIfTerminal h DebugS V0


-------------------------------------------------------------------------------
setupUtf8 :: IO (Scribe, ThreadIdText)
setupUtf8 = do
  h <- openFile "/dev/null" WriteMode
  scribe <- mkHandleScribeUtf8 ColorIfTerminal h DebugS V0
  tid <- myThreadId
  return (scribe, mkThreadIdText tid)

-------------------------------------------------------------------------------
setupUtf8BS :: IO (Scribe, ThreadIdText)
setupUtf8BS = do
  h <- openFile "/dev/null" WriteMode
  scribe <- Utf8.mkHandleScribe ColorIfTerminal h DebugS V0
  tid <- myThreadId
  return (scribe, mkThreadIdText tid)

-------------------------------------------------------------------------------
deriving instance NFData ThreadIdText


instance NFData Scribe where
  rnf (Scribe _) = ()
