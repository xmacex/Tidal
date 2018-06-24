{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Sound.Tidal.Simple where

import Sound.Tidal.Context

import GHC.Exts( IsString(..) )
import Sound.Tidal.Params (s)
import Sound.Tidal.Parse (Parseable)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)

{-
instance IsString (ParamPattern) where
  fromString = s . toPat . parseTPat
-}
toSound :: String -> ParamMap
toSound x = grp' [s_p, n_p] x

instance Parseable ParamMap where
  parseTPat = (toSound <$>) <$> parseRhythm pVocable

--  fromTo :: ParamMap -> ParamMap -> ParamPattern
--  fromThenTo :: ParamMap -> ParamMap -> ParamMap -> ParamPattern

instance Enumerable ParamMap where
  fromTo _ _ = silence
  fromThenTo _ _ _ = silence
{-  fromTo a b = fromMaybe silence $ do a' <- Map.lookup s_p a
                                      b' <- Map.lookup s_p b
                                      let p = fromTo a' b'
                                      return $ (toS <$>) <$> p
-}

crunch :: ParamPattern -> ParamPattern
crunch = (# crush 3)

scratch :: ParamPattern -> ParamPattern
scratch = rev . chop 32

louder :: ParamPattern -> ParamPattern
louder = (|*| gain 1.2)

quieter :: ParamPattern -> ParamPattern
quieter = (|*| gain 0.8)

silent :: ParamPattern -> ParamPattern
silent = const silence

skip :: ParamPattern -> ParamPattern
skip = (0.25 <~)

left :: ParamPattern -> ParamPattern
left = (# pan 0)

right :: ParamPattern -> ParamPattern
right = (# pan 1)

higher :: ParamPattern -> ParamPattern
higher = (|*| up 7)

lower :: ParamPattern -> ParamPattern
lower = (|*| up (-7))

faster :: ParamPattern -> ParamPattern
faster = hurry 2

slower :: ParamPattern -> ParamPattern
slower = hurry 0.5

play :: Pattern Double -> ParamPattern -> ParamPattern
play x s = n x # s
