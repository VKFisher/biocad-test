module Domain where

data Molecule = Molecule
  { iupacName :: Text,
    smiles :: Text
  }
  deriving (Show)

-- Reaction

data ReactionComponent = ReactionComponent
  { molecule :: Molecule,
    amount :: Double -- Mole (?)
  }
  deriving (Show)

data ReactionConditions = ReactionConditions
  { catalyst :: ReactionComponent,
    temperature :: Double, -- Kelvin
    pressure :: Double -- Pascal
  }
  deriving (Show)

data Reaction = Reaction
  { name :: Text,
    reagents :: NonEmpty ReactionComponent,
    products :: NonEmpty ReactionComponent,
    conditions :: ReactionConditions
  }
  deriving (Show)

data ReactionPath
  = ReactionPathStep Molecule Text ReactionPath -- from molecule, reaction name, next step
  | ReactionPathFinal Molecule -- final molecule
  deriving (Show)