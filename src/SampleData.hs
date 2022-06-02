{-# OPTIONS_GHC -fno-warn-orphans #-}

module SampleData where

import Domain
import Test.QuickCheck

-- |
-- ensure no whole values
-- see https://github.com/biocad/hasbolt-extras/issues/48
isWhole :: RealFrac b => b -> Bool
isWhole x = (round (10 ^ (7 :: Integer) * (x - fromIntegral @Integer (round x))) :: Integer) == 0

charToMolecule :: Char -> Molecule
charToMolecule c =
  let c' = toText [c]
   in Molecule
        { iupacName = "molecule " <> c',
          smiles = "smiles " <> c'
        }

instance Arbitrary Molecule where
  arbitrary = elements ['A' .. 'Z'] <&> charToMolecule

instance Arbitrary ReactionComponent where
  arbitrary = do
    molecule <- arbitrary

    amount <- (`suchThat` (not . isWhole @Double)) $ (/ 100) . fromIntegral <$> chooseInt (1, 1000)
    pure
      ReactionComponent
        { molecule = molecule,
          amount = amount
        }

instance Arbitrary ReactionConditions where
  arbitrary = do
    catalystComponent <- arbitrary
    temperature <- (`suchThat` (not . isWhole @Double)) $ (/ 100) . fromIntegral <$> chooseInt (1, 100000)
    pressure <- (`suchThat` (not . isWhole @Double)) $ (/ 100) . fromIntegral <$> chooseInt (1, 100000)
    pure
      ReactionConditions
        { catalyst = catalystComponent,
          temperature = realToFrac temperature,
          pressure = realToFrac pressure
        }

componentWithMolecule :: Molecule -> Gen ReactionComponent
componentWithMolecule m = arbitrary <&> \rc -> rc {molecule = m}

conditionsWithMolecule :: Molecule -> Gen ReactionConditions
conditionsWithMolecule m = do
  catalystComponent <- componentWithMolecule m
  arbitrary <&> \rc -> rc {catalyst = catalystComponent}

instance Arbitrary Reaction where
  arbitrary = do
    name <- ("reaction " <>) . show <$> chooseInteger (1, 1000000)
    productCount <- chooseInt (1, 4)
    reagentCount <- chooseInt (1, 3)
    molecules <- take (1 + reagentCount + productCount) . fmap charToMolecule <$> shuffle ['A' .. 'Z']
    let (catalyst, (reagents, products)) = case molecules of
          (x : xs) -> (x, splitAt reagentCount xs)
          _ -> error "did not generate enough molecules"
    reagents' <- fromList <$> traverse componentWithMolecule reagents
    products' <- fromList <$> traverse componentWithMolecule products
    conditions <- conditionsWithMolecule catalyst
    pure $
      Reaction
        { reagents = reagents',
          products = products',
          name = name,
          conditions = conditions
        }
