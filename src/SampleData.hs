{-# OPTIONS_GHC -fno-warn-orphans #-}

module SampleData where

import Domain
import Test.QuickCheck

-- |
-- ensure no whole values
-- see https://github.com/biocad/hasbolt-extras/issues/48
isWhole :: RealFrac b => b -> Bool
isWhole x = (round (10 ^ (7 :: Integer) * (x - fromIntegral @Integer (round x))) :: Integer) == 0

instance Arbitrary Molecule where
  arbitrary =
    elements ['A' .. 'Z'] <&> \x ->
      let x' = toText [x]
       in Molecule
            { iupacName = "molecule " <> x',
              smiles = "smiles " <> x'
            }

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
    molecule <- arbitrary
    temperature <- (`suchThat` (not . isWhole @Double)) $ (/ 100) . fromIntegral <$> chooseInt (1, 100000)
    pressure <- (`suchThat` (not . isWhole @Double)) $ (/ 100) . fromIntegral <$> chooseInt (1, 100000)
    pure
      ReactionConditions
        { catalyst = molecule,
          temperature = realToFrac temperature,
          pressure = realToFrac pressure
        }

instance Arbitrary Reaction where
  arbitrary = do
    name <- ("reaction " <>) . show <$> chooseInteger (1, 1000000)
    -- TODO : arbitrary catalyst not in products / reactants
    productCount <- chooseInt (1, 4)
    reactantCount <- chooseInt (1, 3)
    (products, reactants) <- splitAt productCount <$> replicateM (productCount + reactantCount) arbitrary
    conditions <- arbitrary
    pure $
      Reaction
        { products = fromList products,
          reactants = fromList reactants,
          name = name,
          conditions = conditions
        }
