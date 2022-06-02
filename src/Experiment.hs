module Experiment (experiment) where

import Control.Exception.Base (bracket)
import Data.Default
import qualified Data.Map as Map
import Database.Bolt
import Db (getReaction, getShortestPath, putReaction)
import Domain
  ( Molecule (iupacName),
    Reaction (products, reactants),
    ReactionComponent (rcMolecule),
    ReactionProduct (rpMolecule),
  )
import SampleData ()
import System.Random.SplitMix (mkSMGen)
import Test.QuickCheck (Arbitrary (arbitrary))
import Test.QuickCheck.Gen (Gen (unGen))
import Test.QuickCheck.Random (QCGen (QCGen))
import Text.Pretty.Simple (pPrint)

boltCfg :: BoltCfg
boltCfg =
  def
    { host = "localhost",
      user = "neo4j",
      password = "password"
    }

runQueryDB :: BoltActionT IO a -> IO a
runQueryDB act = bracket (connect boltCfg) close (`run` act)

deleteAll :: IO ()
deleteAll = do
  putStrLn "Clearing all data"
  runQueryDB $ queryP_ "MATCH (n)\nDETACH DELETE n" Map.empty

-- |
-- для детерменистической генерации значений
seededGen :: Word64 -> Gen a -> a
seededGen s g = unGen g (QCGen $ mkSMGen s) 30

-- населите базу данных представителями (хотя бы по 20 образцов каждого вида).
-- Данные подразумеваются совершенно синтетические.
populateDatabase :: IO [Int]
populateDatabase = do
  putStrLn "filling DB with generated sample data"
  let reactions = seededGen 1 (replicateM 30 arbitrary)
  runQueryDB $ traverse putReaction reactions

runTasks :: IO ()
runTasks = do
  putStrLn "== Task 1: saving reactions to Db =="
  (rId1, rId2) <-
    populateDatabase <&> \case
      (x1 : x2 : _) -> (x1, x2)
      _ -> error "not enough reactions created"

  putStrLn "== Task 2: loading reactions from Db =="
  r1 <- runQueryDB $ fromMaybe (error "could not find reaction") <$> getReaction rId1
  r2 <- runQueryDB $ fromMaybe (error "could not find reaction") <$> getReaction rId2
  putStrLn "Got reactions:"
  pPrint (r1, r2)

  putStrLn "== Task 3: finding shortest path =="
  let mFrom = rcMolecule . head . reactants $ r1
  let mTo = rpMolecule . head . products $ r2
  putStrLn $ toString $ "Looking for shortest path between " <> iupacName mFrom <> " and " <> iupacName mTo
  shortestPath <- runQueryDB $ getShortestPath mFrom mTo
  case shortestPath of
    Nothing -> putStrLn "Could not find path between molecules"
    Just x -> putStrLn "Found shortest path:" >> pPrint x

experiment :: IO ()
experiment = deleteAll >> runTasks
