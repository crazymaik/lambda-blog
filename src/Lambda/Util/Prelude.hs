module Lambda.Util.Prelude where

import Data.Char (isSpace)
import Data.List

-- | remove whitespace on both sides.
trim :: String -> String
trim = trimLeft . trimRight

-- | remove left whitespace.
trimLeft = dropWhile isSpace

-- | remove right whitespace.
trimRight = reverse . trimLeft . reverse

sortUsing :: (Ord b) => (a -> b) -> [a] -> [a]
sortUsing f = sortBy (\a b -> compare (f a) (f b))

