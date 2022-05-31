module Util where

import qualified Data.Text as T

dropPrefix :: String -> String
dropPrefix = toString . snd . T.breakOnEnd "_" . toText