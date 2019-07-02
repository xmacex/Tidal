{-# LANGUAGE LambdaCase, OverloadedStrings  #-}

module Sound.Tidal.AST where

import GHC.Exts ( IsString(..) )

import qualified Sound.Tidal.Pattern as T
import qualified Sound.Tidal.Core as T
import qualified Sound.Tidal.UI as T
import qualified Sound.Tidal.ParseBP as T

-- | AST versions of functions
atom = T.TPat_Atom
fast = T.TPat_Density
slow = T.TPat_Slow
zoom = T.TPat_Zoom
degradeBy = T.TPat_DegradeBy
silence = T.TPat_Silence
fastcat = T.TPat_Cat
timeCat = T.TPat_TimeCat
overlay = T.TPat_Overlay
stack = T.TPat_Stack
shiftL = T.TPat_ShiftL
