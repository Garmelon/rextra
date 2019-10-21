module Rextra.Regex where

import Data.List

data XRegex t
  = XRToken t
  | XRConcat [XRegex t]
  | XRUnion [XRegex t]
  | XRStar (XRegex t)
  | XRPlus (XRegex t)
  | XROptional (XRegex t)
  deriving (Show)

data Regex t
  = REpsilon
  | RToken t
  | RConcat (Regex t) (Regex t)
  | RUnion (Regex t) (Regex t)
  | RStar (Regex t)
  deriving (Show)

toRegex :: XRegex t -> Maybe (Regex t)
toRegex (XRToken t)      = Just $ RToken t
--toRegex (XRConcat [r])   = toRegex r -- Optimisation, not strictly necessary
toRegex (XRConcat rs)    = foldl' RConcat REpsilon <$> traverse toRegex rs
toRegex (XRUnion [])     = Nothing
--toRegex (XRUnion [r])    = toRegex r -- Optimisation, not strictly necessary
toRegex (XRUnion (r:rs)) = foldl' RUnion <$> toRegex r <*> traverse toRegex rs
toRegex (XRStar r)       = RStar <$> toRegex r
toRegex (XRPlus r)       = let rr = toRegex r
                           in  RConcat <$> rr <*> (RStar <$> rr)
toRegex (XROptional r)   = RUnion REpsilon <$> toRegex r
