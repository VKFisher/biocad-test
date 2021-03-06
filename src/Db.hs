module Db where

import Control.Lens (view)
import Control.Monad.Extra (pureIf)
import qualified Data.Map as Map
import qualified Data.Text as T
import Database.Bolt (BoltActionT, Node, at, queryP)
import Database.Bolt.Extras (NodeLike (fromNode, toNode), URelationLike (toURelation))
import Database.Bolt.Extras.DSL as DSL
import Database.Bolt.Extras.Graph
import Database.Bolt.Extras.Template (makeNodeLikeWith, makeURelationLikeWith)
import qualified Domain
import Util (dropPrefix)

----------------------------------------
-- DB represenation types             --
----------------------------------------

-- компонент Molecule должен иметь поля id :: Int, smiles :: String, iupacName :: String;
data Molecule = Molecule {m_iupacName :: Text, m_smiles :: Text}

-- компонент Reaction должен иметь поля id :: Int, name :: String;
data Reaction = Reaction {r_name :: Text, r_rate :: Double}

-- компонент Catalyst должен иметь поля id :: Int, smiles :: String, name :: Maybe String;
data Catalyst = Catalyst
  { c_smiles :: Text,
    c_name :: Maybe Text
  }

-- компонент PRODUCT_FROM должен иметь поле amount :: Float;
data REACTANT_IN = REACTANT_IN {ri_amount :: Double, ri_concentration :: Double}

newtype PRODUCT_FROM = PRODUCT_FROM {pf_amount :: Double}

-- компонент ACCELERATE должен иметь поле temperature :: Float, pressure :: Float;
data ACCELERATE = ACCELERATE
  { a_amount :: Double,
    a_concentration :: Double,
    a_temperature :: Double,
    a_pressure :: Double
  }

makeNodeLikeWith ''Molecule dropPrefix
makeNodeLikeWith ''Reaction dropPrefix
makeNodeLikeWith ''Catalyst dropPrefix
makeURelationLikeWith ''REACTANT_IN dropPrefix
makeURelationLikeWith ''PRODUCT_FROM dropPrefix
makeURelationLikeWith ''ACCELERATE dropPrefix

moleculeToDbRepr :: Domain.Molecule -> Molecule
moleculeToDbRepr m =
  Molecule
    { m_iupacName = Domain.iupacName m,
      m_smiles = Domain.smiles m
    }

moleculeFromDbRepr :: Molecule -> Domain.Molecule
moleculeFromDbRepr m =
  Domain.Molecule
    { Domain.iupacName = m_iupacName m,
      Domain.smiles = m_smiles m
    }

----------------------------------------
-- Requests                           --
----------------------------------------

-- напишите функцию, которая умеет принимать реацию на вход и загружать её в базу;
putReaction :: MonadIO m => Domain.Reaction -> BoltActionT m Int
putReaction reaction = do
  mResult <- viaNonEmpty head <$> graphRequest
  let mId = mResult <&> view vertices >>= Map.lookup reactionNodeName
  case mId of
    Just x -> pure x
    Nothing -> error "Failed to find resulting id"
  where
    graphRequest =
      emptyGraph
        & addNode
          reactionNodeName
          ( MergeN . toNode $
              Reaction
                { r_name = Domain.name reaction,
                  r_rate = Domain.rate reaction
                }
          )
        & addCatalyst
        & addReactants
        & addProducts
        & makeRequest @PutRequest []

    reactionNodeName = "reaction"

    addReactants =
      Domain.reactants reaction
        & toList
        & zip @Int [1 ..]
        <&> ( \(n, x) ->
                let nodeName = "reactant" <> show n
                    putN = MergeN . toNode . moleculeToDbRepr . Domain.rcMolecule $ x
                    putR =
                      MergeR . toURelation $
                        REACTANT_IN
                          { ri_amount = Domain.rcAmount x,
                            ri_concentration = Domain.rcConcentration x
                          }
                 in addRelation nodeName reactionNodeName putR . addNode nodeName putN
            )
        & foldr (.) id

    addProducts =
      Domain.products reaction
        & toList
        & zip @Int [1 ..]
        <&> ( \(n, x) ->
                let nodeName = "product" <> show n
                    putN = MergeN . toNode . moleculeToDbRepr . Domain.rpMolecule $ x
                    putR = MergeR . toURelation $ PRODUCT_FROM {pf_amount = Domain.rpAmount x}
                 in addRelation reactionNodeName nodeName putR . addNode nodeName putN
            )
        & foldr (.) id

    addCatalyst =
      let reactionConditions = Domain.conditions reaction
          catalyst = Domain.rcMolecule $ Domain.catalyst reactionConditions
          nodeName = "catalyst"
          putN =
            MergeN . toNode $
              Catalyst
                { c_smiles = Domain.smiles catalyst,
                  c_name = Just $ Domain.iupacName catalyst
                }
          putR =
            MergeR . toURelation $
              ACCELERATE
                { a_temperature = Domain.temperature reactionConditions,
                  a_pressure = Domain.pressure reactionConditions,
                  a_amount = Domain.rcAmount . Domain.catalyst $ reactionConditions,
                  a_concentration = Domain.rcConcentration . Domain.catalyst $ reactionConditions
                }
       in addRelation nodeName reactionNodeName putR . addNode nodeName putN

-- напишите функцию, которая по номеру реакции в базе будет возвращать её в Haskell-объект;
getReaction :: (MonadIO m) => Int -> BoltActionT m (Maybe Domain.Reaction)
getReaction reactionId = do
  result <- makeRequest @GetRequest [] graphRequest
  let result' = mergeGraphs <$> pureIf (not . null $ result) result
  pure $ toDomain <$> result'
  where
    graphRequest =
      emptyGraph
        & addNode "reaction" (getNode ''Reaction & withBoltId reactionId)
        & addNode "reactant" (getNode ''Molecule)
        & addRelation "reactant" "reaction" (getRel ''REACTANT_IN)
        & addNode "product" (getNode ''Molecule)
        & addRelation "reaction" "product" (getRel ''PRODUCT_FROM)
        & addNode "catalyst" (getNode ''Catalyst)
        & addRelation "catalyst" "reaction" (getRel ''ACCELERATE)

    getNode n =
      defaultNodeReturn
        & withLabelQ n
        & withReturn allProps

    getRel n =
      defaultRelReturn
        & withLabelQ n
        & withReturn allProps

    reactionNodeName = "reaction" <> show reactionId

    toDomain :: Graph NodeName NodeResult RelResult -> Domain.Reaction
    toDomain g =
      let rNode = extractNode reactionNodeName g
       in Domain.Reaction
            { Domain.name = r_name rNode,
              Domain.rate = r_rate rNode,
              Domain.products = fromList $ products g,
              Domain.reactants = fromList $ reactants g,
              Domain.conditions = catalyst g
            }

    products :: Graph NodeName NodeResult RelResult -> [Domain.ReactionProduct]
    products g =
      g
        & nodeNamesMatching (T.isPrefixOf "product")
        <&> (\n -> (extractNode n g, extractRelation reactionNodeName n g))
        <&> ( \(m, PRODUCT_FROM {pf_amount}) ->
                Domain.ReactionProduct
                  { Domain.rpMolecule = moleculeFromDbRepr m,
                    Domain.rpAmount = pf_amount
                  }
            )

    reactants :: Graph NodeName NodeResult RelResult -> [Domain.ReactionComponent]
    reactants g =
      g
        & nodeNamesMatching (T.isPrefixOf "reactant")
        <&> (\n -> (extractNode n g, extractRelation n reactionNodeName g))
        <&> ( \(m, REACTANT_IN {ri_amount, ri_concentration}) ->
                Domain.ReactionComponent
                  { Domain.rcMolecule = moleculeFromDbRepr m,
                    Domain.rcAmount = ri_amount,
                    Domain.rcConcentration = ri_concentration
                  }
            )

    catalyst :: Graph NodeName NodeResult RelResult -> Domain.ReactionConditions
    catalyst g =
      let catalystNodeName =
            g & nodeNamesMatching (T.isPrefixOf "catalyst")
              & fromMaybe (error "no catalyst node") . viaNonEmpty head
       in g
            & extractNode catalystNodeName &&& extractRelation catalystNodeName reactionNodeName
            & ( \(cNode, aRel) ->
                  Domain.ReactionConditions
                    { Domain.catalyst =
                        Domain.ReactionComponent
                          { Domain.rcMolecule =
                              Domain.Molecule
                                { Domain.smiles = c_smiles cNode,
                                  Domain.iupacName = fromMaybe (error "no catalyst name") $ c_name cNode
                                },
                            Domain.rcAmount = a_amount aRel,
                            Domain.rcConcentration = a_concentration aRel
                          },
                      Domain.temperature = a_temperature aRel,
                      Domain.pressure = a_pressure aRel
                    }
              )

    nodeNamesMatching :: (NodeName -> Bool) -> Graph NodeName a b -> [NodeName]
    nodeNamesMatching f g =
      g
        & view vertices
        & Map.keys
        & filter f

-- напишите функцию, которая по двум заданным молекулам ищет путь через реакции и молекулы с наименьшей длиной.
getShortestPath :: (MonadIO m) => Domain.Molecule -> Domain.Molecule -> BoltActionT m (Maybe Domain.ReactionPath)
getShortestPath mFrom mTo = do
  mResult <- viaNonEmpty head <$> queryP cypher Map.empty
  pathToDomain <<$>> maybe (pure Nothing) (`at` "pN") mResult
  where
    cypher = DSL.formQuery $ do
      matchF [mFromSelector, mToSelector]
      textF $
        "MATCH p = shortestPath((MFrom)-[r:REACTANT_IN|:PRODUCT_FROM*.." <> show maxPathLength <> "]->(MTo))"
          <> "WHERE ALL(n in nodes(p) WHERE n:Molecule OR n:Reaction)"
      returnF ["nodes(p) AS pN"]

    maxPathLength :: Int
    maxPathLength = 20

    pathToDomain :: [Node] -> Domain.ReactionPath
    pathToDomain (m : r : xs) =
      Domain.ReactionPathStep
        (moleculeFromDbRepr . fromNode $ m)
        (r_name . fromNode $ r)
        (pathToDomain xs)
    pathToDomain [m] = Domain.ReactionPathFinal . moleculeFromDbRepr . fromNode $ m
    pathToDomain [] = error "invalid length of reaction path"

    mFromSelector =
      mFrom
        & toNodeSelector . toNode . moleculeToDbRepr
        & DSL.withIdentifier "MFrom"
        & PS . P

    mToSelector =
      mTo
        & toNodeSelector . toNode . moleculeToDbRepr
        & DSL.withIdentifier "MTo"
        & PS . P
