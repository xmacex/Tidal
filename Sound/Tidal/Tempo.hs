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

data Client = Client {port :: Int,
                      udp :: UDP,
                      failures :: Int
                     }

data Tempo = Tempo {at :: UTCTime,
                    beat :: Double,
                    cps :: Double,
                    paused :: Bool,
                    nudged :: Double
                   }

data ServerMode = Master
                | Slave UDP

data ServerState = ServerState {tempo :: Tempo,
                                clients :: [Client],
                                mode :: ServerMode
                               }

instance Show Tempo where
  show x = (show (at x) ++ ", " ++
            show (beat x) ++ ", " ++
            show (cps x) ++ ", " ++
            show (paused x) ++ ", " ++
            show (nudged x)
           )

getSenderIp :: IO String
getSenderIp = fromMaybe "127.0.0.1" <$> lookupEnv "TIDAL_TEMPO_IP"

getSenderPort :: IO Int
getSenderPort =
   maybe 6041 (readNote "port parse") <$> lookupEnv "TIDAL_TEMPO_PORT"

sendCPS cps = do port <- getSenderPort
                 s <- openUDP "127.0.0.1" port
                 sendOSC s $ Message "/cps" [Float cps]
                 close s

sendNudge nudge = do port <- getSenderPort
                     s <- openUDP "127.0.0.1" port
                     sendOSC s $ Message "/nudge" [Float nudge]
                     close s

subscribe :: Int -> IO ()
subscribe p = do senderPort <- getSenderPort
                 s <- openUDP "127.0.0.1" senderPort
                 sendOSC s $ Message "/subscribe" [Int32 $ fromIntegral p]
                 close s
                 return ()

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

tempoSender :: IO (ThreadId)
tempoSender = forkIO $ do now <- getCurrentTime
                          ip <- getSenderIp
                          port <- getSenderPort
                          let t = Tempo now 0 1 False 0
                          s <- udpServer ip port
                          tempoSenderLoop s (ServerState t [] Master)
                            where tempoSenderLoop :: UDP -> ServerState -> IO ()
                                  tempoSenderLoop s state =
                                    do ms <- recvMessages s
                                       state' <- foldM senderAct state ms
                                       tempoSenderLoop s state'

senderAct :: ServerState -> Message -> IO ServerState
senderAct state (Message "/cps" [Float cps]) =
  do t <- updateTempo (tempo state) (realToFrac cps)
     cs <- sendTempo (tempo state) (clients state)
     return state {tempo = t, clients = cs}

senderAct state (Message "/subscribe" [Int32 p]) 
  | null $ filter ((== (fromIntegral p)) . port) (clients state)
    = do s <- openUDP "127.0.0.1" (fromIntegral p)
         let c = Client {port = (fromIntegral p), udp = s, failures = 0}
         sendTempo (tempo state) [c]
         putStrLn $ "subscribed " ++ (show p)
         return $ state {clients = (c:clients state)}
  | otherwise = do putStrLn ("port " ++ (show p) ++ " already subscribed")
                   return state

senderAct state (Message s _) = do putStrLn $ "Received unknown command " ++ s
                                   return state

waitForTempo :: MVar Tempo -> Int -> IO ()
waitForTempo mTempo p = do t <- readMVar mTempo
                           check t
                             where check t | paused t = do subscribe p
                                                           threadDelay 1000000
                                                           waitForTempo mTempo p
                                           | otherwise = return ()

tempoReceiver :: IO (MVar Tempo)
tempoReceiver = do now <- getCurrentTime
                   mTempo <- newMVar (Tempo now 0 1 True 0)
                   putStrLn "make socket to receive"
                   sock <- N.socket N.AF_INET N.Datagram 0
                   -- N.setSocketOptiSocketon sock N.NoDelay 1
                   -- N.setSocketOption sock N.ReuseAddr 1
                   -- N.setSocketOption sock N.ReusePort 1
                   a <- N.inet_addr "0.0.0.0"
                   let sa = N.SockAddrInet (fromIntegral 0) a
                   N.bind sock sa
                   let s = UDP sock
                   p <- udpPort s
                   forkIO $ tempoReceiverLoop s mTempo
                   waitForTempo mTempo p
                   return mTempo

tempoReceiverLoop :: UDP -> MVar Tempo -> IO ()
tempoReceiverLoop s mTempo =
  do ms <- recvMessages s
     mapM_ (\m -> act (messageAddress m) mTempo m) ms
     tempoReceiverLoop s mTempo

act "/tempo" mTempo m | isJust t = do swapMVar mTempo (fromJust t)
                                      return ()
                      | otherwise = return ()
  where t = do beat' <- datum_floating $ (messageDatum m) !! 2
               cps' <- datum_floating $ (messageDatum m) !! 3
               return $ Tempo {at = ut,
                               beat = beat',
                               cps = cps',
                               paused = False,
                               nudged = 0
                              }
        ut = addUTCTime (realToFrac $ dsec) ut_epoch
        sec = fromJust $ datum_int32 $ (messageDatum m) !! 0
        usec = fromJust $ datum_int32 $ (messageDatum m) !! 1
        dsec = ((fromIntegral sec) + ((fromIntegral usec) / 1000000)) :: Double

act x mTempo _ = do putStrLn ("no act for" ++ x)
                    return ()

{-
clients = do putStrLn "make socket to send"
             sock <- N.socket N.AF_INET N.Datagram 0
             -- N.setSocketOptiSocketon sock N.NoDelay 1
             N.setSocketOption sock N.Broadcast 1
             -- N.setSocketOption sock N.ReusePort 1
             a <- N.inet_addr "127.255.255.255"
             let sa = N.SockAddrInet (fromIntegral 6040) a
             N.connect sock sa
             let s = UDP sock
             return s
-}

sendTempo :: Tempo -> [Client] -> IO [Client]
sendTempo t cs = do putStrLn "sendTempo"
                    -- TODO exception handling
                    mapM_ (\c -> sendOSC (udp c) m) cs
                    return cs
  where m = Message "/tempo" [int32 sec,
                              int32 usec,
                              float (realToFrac $ beat t),
                              float (realToFrac $ cps t),
                              string (show $ paused t)
                             ]
        ut = utc_to_ut $ at t
        sec = floor ut
        usec = floor ((ut - (fromIntegral sec)) * 1000000)

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
cpsUtils' = do tempoSender
               mTempo <- tempoReceiver
               let currentTime = do t <- readMVar mTempo
                                    now <- beatNow t
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
          do t <- readMVar mTempo
             tick' <- doTick t tick
             loop mTempo tick'
        doTick t tick | paused t =
          do let pause = 0.01
             -- TODO - do this via blocking read on the mvar somehow
             -- rather than polling
             threadDelay $ floor (pause * 1000000)
             -- reset tick to 0 if cps is negative
             return $ if cps t < 0 then 0 else tick
                          | otherwise =
          do now <- getCurrentTime
             let tps = (fromIntegral tpb) * cps t
                 delta = realToFrac $ diffUTCTime now (at t)
                 actualTick = ((fromIntegral tpb) * beat t) + (tps * delta)
                 -- only wait by up to two ticks
                 tickDelta = min 2 $ (fromIntegral tick) - actualTick
                 delay = tickDelta / tps
             threadDelay $ floor (delay * 1000000)
             callback t tick
             let newTick | (abs $ (floor actualTick) - tick) > 4 = floor actualTick
                         | otherwise = tick + 1
             return $ newTick

