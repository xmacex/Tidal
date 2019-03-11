{-# LANGUAGE ScopedTypeVariables #-}

module Sound.Tidal.EspGrid (tidalEspGridLink,cpsEsp) where

import Control.Concurrent.MVar
import Control.Concurrent (forkIO,threadDelay)
import Control.Monad (forever)
import Sound.OSC.FD
import Sound.Tidal.Tempo
import System.Clock (TimeSpec (..), sec, nsec)

parseEspTempo :: [Datum] -> Maybe (Tempo -> Tempo)
parseEspTempo d = do
  on :: Integer <- datum_integral (d!!0)
  bpm <- datum_floating (d!!1)
  t1 <- datum_integral (d!!2)
  t2 <- datum_integral (d!!3)
  (n :: Int) <- datum_integral (d!!4)
  return $ \t -> t {
    atTime = TimeSpec {sec = t1, nsec = t2},
    atCycle = fromIntegral n,
    cps = bpm/60,
    paused = on == 0
    }

changeTempo :: MVar Tempo -> Packet -> IO ()
changeTempo t (Packet_Message msg) =
    case parseEspTempo (messageDatum msg) of
      Just f -> takeMVar t >>= putMVar t . f
      Nothing -> putStrLn "Warning: Unable to parse message (likely from EspGrid) as Tempo"
changeTempo _ _ = putStrLn "Serious error: Can only process Packet_Message"

tidalEspGridLink :: MVar Tempo -> IO ()
tidalEspGridLink t = do
  socket <- openUDP "127.0.0.1" 5510
  _ <- forkIO $ forever $ do
    _ <- sendMessage socket $ Message "/esp/tempo/q" []
    response <- waitAddress socket "/esp/tempo/r"
    Sound.Tidal.EspGrid.changeTempo t response
    threadDelay 200000
  return ()

cpsEsp :: Real t => t -> IO ()
cpsEsp t = do
  socket <- openUDP "127.0.0.1" 5510
  sendMessage socket $ Message "/esp/beat/tempo" [float (t*60)]
