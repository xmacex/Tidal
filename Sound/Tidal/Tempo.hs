module Sound.Tidal.Tempo where

import Sound.OSC.FD
import qualified Network.Socket as N
import Safe (readNote)
import System.Environment (lookupEnv)
import qualified Control.Exception as E
import Data.Time (getCurrentTime, UTCTime, NominalDiffTime, diffUTCTime, addUTCTime)
import Data.Time.Clock.POSIX
import Data.Maybe
import Control.Concurrent.MVar
import Control.Concurrent
import Control.Monad (foldM)

data Tempo = Tempo {at :: UTCTime,
                    beat :: Double,
                    cps :: Double,
                    paused :: Bool,
                    nudged :: Double
                   }

instance Show Tempo where
  show x = (show (at x) ++ "," ++
            show (beat x) ++ "," ++
            show (cps x) ++ "," ++
            show (paused x)
           )

getSenderIp :: IO String
getSenderIp = fromMaybe "127.0.0.1" <$> lookupEnv "TIDAL_TEMPO_IP"

getSenderPort :: IO Int
getSenderPort =
   maybe 9161 (readNote "port parse") <$> lookupEnv "TIDAL_TEMPO_PORT"

sendCPS cps = do s <- openUDP "127.0.0.1" 9161
                 sendOSC s $ Message "/cps" [Float cps]

sendNudge nudge = do s <- openUDP "127.0.0.1" 9161
                     sendOSC s $ Message "/nudge" [Float nudge]

ping = do s <- openUDP "127.0.0.1" 9161
          sendOSC s $ Message "/ping" []

updateTempo :: Tempo -> Double -> IO (Tempo)
updateTempo t cps'
  | paused t == True && cps' > 0 =
    -- unpause
    do now <- getCurrentTime
       return $ t {at = now, cps = cps', paused = False}
  | otherwise = 
    do now <- getCurrentTime
       let delta = realToFrac $ diffUTCTime now (at t)
           beat' = (beat t) + ((cps t) * delta)
           beat'' = if cps' < 0 then 0 else beat'
       return $ t {at = now, beat = beat'', cps = cps', paused = (cps' <= 0)}

nudgeTempo :: Tempo -> Double -> Tempo
nudgeTempo t secs = t {at = addUTCTime (realToFrac secs) (at t)}

tempoSender :: IO ()
tempoSender = do now <- getCurrentTime
                 ip <- getSenderIp
                 port <- getSenderPort
                 let tempoState = Tempo now 0 1 False 0
                 s <- udpServer ip port
                 c <- clients
                 sendTempo c tempoState
                 tempoSenderLoop s c tempoState
                   where tempoSenderLoop :: UDP -> UDP -> Tempo -> IO ()
                         tempoSenderLoop s c tempoState =
                           do ms <- recvMessages s
                              tempoState' <- foldM (senderAct c) tempoState ms 
                              tempoSenderLoop s c tempoState'

senderAct c tempoState (Message "/cps" [Float cps]) =
  do tempoState' <- updateTempo tempoState (realToFrac cps)
     sendTempo c tempoState'
     return tempoState'

senderAct c tempoState (Message "/ping" _) =
  do sendTempo c tempoState
     return tempoState

senderAct _ tempoState (Message s _) = do putStrLn $ "Received unknown command " ++ s
                                          return tempoState

waitForTempo :: MVar Tempo -> IO ()
waitForTempo mTempo = do t <- readMVar mTempo
                         check t
                           where check t | paused t = do ping
                                                         threadDelay 10000
                                                         waitForTempo mTempo
                                         | otherwise = return ()

tempoReceiver :: IO (MVar Tempo)
tempoReceiver = do now <- getCurrentTime
                   mTempo <- newMVar (Tempo now 0 1 True 0)
                   sock <- N.socket N.AF_INET N.Datagram 0
                   -- N.setSocketOptiSocketon sock N.NoDelay 1
                   N.setSocketOption sock N.ReuseAddr 1
                   -- N.setSocketOption sock N.ReusePort 1
                   a <- N.inet_addr "0.0.0.0"
                   let sa = N.SockAddrInet (fromIntegral 6040) a
                   N.bind sock sa
                   let s = UDP sock
                   forkIO $ tempoReceiverLoop s mTempo
                   waitForTempo mTempo
                   return mTempo

tempoReceiverLoop :: UDP -> MVar Tempo -> IO ()
tempoReceiverLoop s mTempo =
  do b <- recvBundle s
     let timestamp = addUTCTime (realToFrac $ ntpr_to_ut $ bundleTime b) ut_epoch
     mapM_ (process timestamp) (bundleMessages b)
     tempoReceiverLoop s mTempo
       where process timestamp m =
               do let address = messageAddress m
                  act address mTempo timestamp m

act "/tempo" mTempo timestamp m = swapMVar mTempo t
  where t = Tempo {at = timestamp,
                   beat = fromJust $ datum_floating $ (messageDatum m) !! 0,
                   cps = fromJust $ datum_floating $ (messageDatum m) !! 1,
                   paused = False,
                   nudged = 0
                  }

clients = do sock <- N.socket N.AF_INET N.Datagram 0
             -- N.setSocketOptiSocketon sock N.NoDelay 1
             N.setSocketOption sock N.Broadcast 1
             -- N.setSocketOption sock N.ReusePort 1
             a <- N.inet_addr "127.255.255.255"
             let sa = N.SockAddrInet (fromIntegral 6040) a
             N.connect sock sa
             let s = UDP sock
             return s

sendTempo :: UDP -> Tempo -> IO ()
sendTempo sock t = sendOSC sock b
  where m = Message "/tempo" [float (realToFrac $ beat t),
                              float (realToFrac $ cps t),
                              string (show $ paused t)
                             ]
        b = Bundle (ut_to_ntpr $ utc_to_ut $ at t) [m]

logicalTime :: Tempo -> Double -> Double
logicalTime t b = changeT + timeDelta
  where beatDelta = b - (beat t)
        timeDelta = beatDelta / (cps t)
        changeT = realToFrac $ utcTimeToPOSIXSeconds $ at t


beatNow :: Tempo -> IO (Double)
beatNow t = do now <- getCurrentTime
               let delta = realToFrac $ diffUTCTime now (at t)
               let beatDelta = cps t * delta               
               return $ beat t + beatDelta

cpsUtils' :: IO ((Float -> IO (), (Float -> IO ()), IO Rational))
cpsUtils' = do -- hack! start the server here for now..
               forkIO $ tempoSender
               mTempo <- tempoReceiver
               let currentTime = do tempo <- readMVar mTempo
                                    now <- beatNow tempo
                                    return $ toRational now
               return (sendCPS, sendNudge, currentTime)

-- backward compatibility
cpsUtils = do (cpsSetter, _, currentTime) <- cpsUtils'
              return (cpsSetter, currentTime)

-- Backwards compatibility
bpsUtils :: IO ((Float -> IO (), IO (Rational)))
bpsUtils = cpsUtils

clocked :: (Tempo -> Int -> IO ()) -> IO ()
clocked = clockedTick 1

clockedTick :: Int -> (Tempo -> Int -> IO ()) -> IO ()
clockedTick tpb callback = 
  do mTempo <- tempoReceiver
     t <- readMVar mTempo
     now <- getCurrentTime
     let delta = realToFrac $ diffUTCTime now (at t)
         beatDelta = cps t * delta
         nowBeat = beat t + beatDelta
         nextTick = ceiling (nowBeat * (fromIntegral tpb))
     loop mTempo nextTick
  where loop mTempo tick = 
          do tempo <- readMVar mTempo
             tick' <- doTick tempo tick
             loop mTempo tick'
        doTick tempo tick | paused tempo =
          do let pause = 0.01
             -- TODO - do this via blocking read on the mvar somehow
             -- rather than polling
             threadDelay $ floor (pause * 1000000)
             -- reset tick to 0 if cps is negative
             return $ if cps tempo < 0 then 0 else tick
                          | otherwise =
          do now <- getCurrentTime
             let tps = (fromIntegral tpb) * cps tempo
                 delta = realToFrac $ diffUTCTime now (at tempo)
                 actualTick = ((fromIntegral tpb) * beat tempo) + (tps * delta)
                 -- only wait by up to two ticks
                 tickDelta = min 2 $ (fromIntegral tick) - actualTick
                 delay = tickDelta / tps
             threadDelay $ floor (delay * 1000000)
             callback tempo tick
             let newTick | (abs $ (floor actualTick) - tick) > 4 = floor actualTick
                         | otherwise = tick + 1
             return $ newTick

