{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Sound.Tidal.Tempo where

-- import Data.Time (getCurrentTime, UTCTime, NominalDiffTime, diffUTCTime, addUTCTime)
-- import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Control.Concurrent.MVar
import qualified Sound.Tidal.Pattern as P
import qualified Sound.OSC.FD as O
-- import qualified Sound.OSC.Transport.FD.UDP as O
import qualified Network.Socket as N
import Control.Concurrent (forkIO, ThreadId, threadDelay)
import Control.Monad (forever, when, foldM)
import Data.List (isPrefixOf, nub)
import qualified Control.Exception as E
import System.Clock

import Sound.Tidal.Config


getNow = getTime MonotonicRaw

data Tempo = Tempo {atTime  :: TimeSpec,
                    atCycle :: Rational,
                    cps     :: Double,
                    paused  :: Bool,
                    nudged  :: Double,
                    localUDP   :: O.UDP,
                    remoteAddr :: N.SockAddr,
                    synched :: Bool
                   }
           -- deriving Show

-- sendTempo udp tempo remote_sockaddr            
-- 

data State = State {ticks   :: Int,
                    start   :: TimeSpec,
                    nowTime :: TimeSpec,
                    nowArc  :: P.Arc,
                    starting :: Bool
                   }

changeTempo :: MVar Tempo -> (TimeSpec -> Tempo -> Tempo) -> IO Tempo
changeTempo tempoMV f = do t <- getNow
                           tempo <- takeMVar tempoMV
                           let tempo' = f t $ tempo
                           sendTempo tempo'
                           putMVar tempoMV tempo'
                           return tempo'


resetCycles :: MVar Tempo -> IO Tempo
resetCycles tempoMV = changeTempo tempoMV (\t tempo -> tempo {atTime = t, atCycle = 0})

setCps :: MVar Tempo -> Double -> IO Tempo
setCps tempoMV newCps = changeTempo tempoMV (\t tempo -> tempo {atTime = t,
                                                                atCycle = timeToCycles tempo t,
                                                                cps = newCps
                                                               })

defaultTempo :: TimeSpec -> O.UDP -> N.SockAddr -> Tempo
defaultTempo t local remote = Tempo {atTime   = t,
                                     atCycle  = 0,
                                     cps      = 0.5625,
                                     paused   = False,
                                     nudged   = 0,
                                     localUDP   = local,
                                     remoteAddr = remote,
                                     synched = False
                                    }

-- | Returns the given time in terms of
-- cycles relative to metrical grid of a given Tempo
timeToCycles :: Tempo -> TimeSpec -> Rational
timeToCycles tempo t = atCycle tempo + toRational cycleDelta
  where delta = t - atTime tempo
        cycleDelta = realToFrac (cps tempo) * (ts2rf delta)

{-
getCurrentCycle :: MVar Tempo -> IO Rational
getCurrentCycle t = (readMVar t) >>= (cyclesNow) >>= (return . toRational)
-}


rf2ts :: RealFrac a => a -> TimeSpec
rf2ts t = TimeSpec {sec = floor t,
                    nsec = floor $ (t - (fromIntegral $ floor t)) * 1e9
                   }

ts2rf :: RealFrac a => TimeSpec -> a
ts2rf ts = (fromIntegral $ sec ts) + ((fromIntegral $ nsec ts) / 1e9)

ts2micro :: TimeSpec -> Int
ts2micro ts = fromIntegral $ (sec ts * 1000000) + (nsec ts `div` 1000)

clocked :: Config -> (MVar Tempo -> State -> IO ()) -> IO (MVar Tempo, [ThreadId])
clocked config callback
  = do s <- getNow
       -- TODO - do something with thread id
       _ <- serverListen config
       (tempoMV, listenTid) <- clientListen config s
       let st = State {ticks = 0,
                       start = s,
                       nowTime = s,
                       nowArc = P.Arc 0 0,
                       starting = True
                      }
       clockTid <- forkIO $ loop tempoMV st
       return (tempoMV, [listenTid, clockTid])
  where frameTimespan = rf2ts $ cFrameTimespan config
        loop tempoMV st =
          do -- putStrLn $ show $ nowArc ts
             tempo <- readMVar tempoMV               
             let -- 'now' comes from clock ticks, nothing to do with cycles
                 logicalT ticks' = start st + fromIntegral ticks' * frameTimespan
                 logicalNow = logicalT $ ticks st + 1
                 -- the tempo is just used to convert logical time to cycles
                 e = timeToCycles tempo logicalNow
                 s = if starting st && synched tempo
                     then timeToCycles tempo (logicalT $ ticks st)
                     else P.stop $ nowArc st
                 st' = st {ticks = ticks st + 1, nowArc = P.Arc s e,
                           starting = not (synched tempo)
                          }
             t <- getNow
             when (t < logicalNow) $ threadDelay (ts2micro $ logicalNow - t)
             callback tempoMV st'
             loop tempoMV st'

clientListen :: Config -> TimeSpec -> IO (MVar Tempo, ThreadId)
clientListen config s =
  do -- Listen on random port
     let tempoClientPort = cTempoClientPort config
         hostname = cTempoAddr config
         port = cTempoPort config
     (remote_addr:_) <- N.getAddrInfo Nothing (Just hostname) Nothing
     local <- O.udpServer "127.0.0.1" tempoClientPort
     let (N.SockAddrInet _ a) = N.addrAddress remote_addr
         remote = N.SockAddrInet (fromIntegral port) a
         t = defaultTempo s local remote
     -- Send to clock port from same port that's listened to
     O.sendTo local (O.p_message "/hello" []) remote
     -- Make tempo mvar
     tempoMV <- newMVar t
     -- Listen to tempo changes
     tempoChild <- forkIO $ listenTempo local tempoMV
     return (tempoMV, tempoChild)

sendTempo :: Tempo -> IO ()
sendTempo tempo = O.sendTo (localUDP tempo) m (remoteAddr tempo)
  where m = O.p_message "/transmit/cps/cycle" [O.Int64 $ sec (atTime tempo),
                                               O.Int64 $ nsec (atTime tempo),
                                               O.Float $ fromRational $ atCycle tempo,
                                               O.Float $ realToFrac $ cps tempo,
                                               O.Int32 $ if paused tempo then 1 else 0
                                              ]

listenTempo :: O.UDP -> MVar Tempo -> IO ()
listenTempo udp tempoMV = forever $ do pkt <- O.recvPacket udp
                                       act pkt
                                       return ()
  where -- act _ (O.Packet_Bundle (O.Bundle ts ms)) = mapM_ (act (Just ts) . O.Packet_Message) ms
        act (O.Packet_Message (O.Message "/cps/cycle" [O.Int64 s,
                                                       O.Int64 ns,
                                                       O.Float atCycle',
                                                       O.Float cps',
                                                       O.Int32 paused'
                                                      ]
                              )
            ) =
          do tempo <- takeMVar tempoMV
             putMVar tempoMV $ tempo {atTime = TimeSpec s ns,
                                      atCycle = realToFrac atCycle',
                                      cps = realToFrac cps',
                                      paused = paused' == 1,
                                      synched = True
                                     }
        act pkt = putStrLn $ "Unknown packet: " ++ show pkt

serverListen :: Config -> IO (Maybe ThreadId)
serverListen config = catchAny run (\_ -> do putStrLn "Tempo listener failed (is one already running?)"
                                             return Nothing
                                   )
  where run = do let port = cTempoPort config
                 -- iNADDR_ANY deprecated - what's the right way to do this?
                 udp <- O.udpServer "0.0.0.0" port
                 tid <- forkIO $ loop udp []
                 return $ Just tid
        loop udp cs = do (pkt,c) <- O.recvFrom udp
                         cs' <- act udp c cs pkt
                         loop udp cs'
        act :: O.UDP -> N.SockAddr -> [N.SockAddr] -> O.Packet -> IO [N.SockAddr]
        -- act udp c _ cs (O.Packet_Bundle (O.Bundle ts ms)) = foldM (act udp c (Just ts)) cs $ map O.Packet_Message ms
        act _ c cs (O.Packet_Message (O.Message "/hello" []))
          = return $ nub $ c:cs
        act udp _ cs (O.Packet_Message (O.Message path params))
          | "/transmit" `isPrefixOf` path =
              do let path' = drop 9 path
                     msg = O.p_message path' params
                 mapM_ (O.sendTo udp msg) cs
                 return cs
        act _ _ cs pkt = do putStrLn $ "Unknown packet: " ++ show pkt
                            return cs
        catchAny :: IO a -> (E.SomeException -> IO a) -> IO a
        catchAny = E.catch


