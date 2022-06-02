{-# OPTIONS_GHC -fno-warn-orphans #-}

module SampleData where

import Domain
import Relude.Extra (bimapBoth)
import Test.QuickCheck

-- |
-- ensure no whole values
-- see https://github.com/biocad/hasbolt-extras/issues/48
isWhole :: RealFrac b => b -> Bool
isWhole x = (round (10 ^ (7 :: Integer) * (x - fromIntegral @Integer (round x))) :: Integer) == 0

arbitraryDouble :: (Int, Int) -> Gen Double
arbitraryDouble bounds =
  bounds
    & bimapBoth (* 1000)
    & chooseInt
    <&> fromIntegral
    <&> (/ 1000)
    & (`suchThat` (not . isWhole))

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

    amount <- arbitraryDouble (1, 20)
    concentration <- arbitraryDouble (1, 10)
    pure
      ReactionComponent
        { rcMolecule = molecule,
          rcAmount = amount,
          rcConcentration = concentration
        }

instance Arbitrary ReactionProduct where
  arbitrary = do
    molecule <- arbitrary
    amount <- arbitraryDouble (1, 20)
    pure
      ReactionProduct
        { rpMolecule = molecule,
          rpAmount = amount
        }

instance Arbitrary ReactionConditions where
  arbitrary = do
    catalystComponent <- arbitrary
    temperature <- arbitraryDouble (-100, 1000)
    pressure <- arbitraryDouble (1, 1000000)
    pure
      ReactionConditions
        { catalyst = catalystComponent,
          temperature = realToFrac temperature,
          pressure = realToFrac pressure
        }

componentWithMolecule :: Molecule -> Gen ReactionComponent
componentWithMolecule m = arbitrary <&> \rc -> rc {rcMolecule = m}

productWithMolecule :: Molecule -> Gen ReactionProduct
productWithMolecule m = arbitrary <&> \rc -> rc {rpMolecule = m}

conditionsWithMolecule :: Molecule -> Gen ReactionConditions
conditionsWithMolecule m = do
  catalystComponent <- componentWithMolecule m
  arbitrary <&> \rc -> rc {catalyst = catalystComponent}

instance Arbitrary Reaction where
  arbitrary = do
    name <- ("reaction " <>) . show <$> chooseInteger (1, 1000000)
    rate <- arbitraryDouble (1, 10)
    productCount <- chooseInt (1, 4)
    reactantCount <- chooseInt (1, 3)
    molecules <- take (1 + reactantCount + productCount) . fmap charToMolecule <$> shuffle ['A' .. 'Z']
    let (catalyst, (reactants, products)) = case molecules of
          (x : xs) -> (x, splitAt reactantCount xs)
          _ -> error "did not generate enough molecules"
    reactants' <- fromList <$> traverse componentWithMolecule reactants
    products' <- fromList <$> traverse productWithMolecule products
    conditions <- conditionsWithMolecule catalyst
    pure $
      Reaction
        { name = name,
          reactants = reactants',
          products = products',
          rate = rate,
          conditions = conditions
        }
