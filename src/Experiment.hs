module Experiment (experiment) where

import Data.Default
import qualified Data.Map as Map
import Database.Bolt
import Db (getReaction, putReaction)
import SampleData ()
import Test.QuickCheck (Arbitrary (arbitrary), generate)
import Text.Pretty.Simple (pPrint)

getPipe :: IO Pipe
getPipe = connect $ def {user = "neo4j", password = "password"}

deleteAll :: Pipe -> IO ()
deleteAll pipe = do
  putStrLn "deleting all data"
  run pipe $ queryP_ "MATCH (n)\nDETACH DELETE n" Map.empty

-- населите базу данных представителями (хотя бы по 20 образцов каждого вида). 
-- Данные подразумеваются совершенно синтетические.
populateDatabase :: Pipe -> IO [Int]
populateDatabase pipe = do
  putStrLn "filling DB with generated sample data"
  reactions <- replicateM 1 $ generate arbitrary
  run pipe $ traverse putReaction reactions

experiment :: IO ()
experiment = do
  putStrLn "-- Experiment start --"
  pipe <- getPipe
  deleteAll pipe
  sampleReactionId <- fromMaybe (error "no id") . viaNonEmpty head <$> populateDatabase pipe
  sampleReaction <- run pipe $ getReaction sampleReactionId
  pPrint sampleReaction
  putStrLn "-- Experiment end ----"
