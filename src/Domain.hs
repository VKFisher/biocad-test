module Domain where

type Mole = Double

type Concentration = Double -- Moles per liter

type ReactionRate = Double -- Moles per liter per time unit (rate of concentration increase)

type Pascal = Double

type Celsius = Double

type ReactionName = Text

data Molecule = Molecule
  { iupacName :: Text,
    smiles :: Text
  }
  deriving (Show)

data ReactionComponent = ReactionComponent
  { rcMolecule :: Molecule,
    rcAmount :: Mole,
    rcConcentration :: Concentration
  }
  deriving (Show)

data ReactionProduct = ReactionProduct
  { rpMolecule :: Molecule,
    rpAmount :: Mole
  }
  deriving (Show)

data ReactionConditions = ReactionConditions
  { catalyst :: ReactionComponent,
    temperature :: Celsius,
    pressure :: Pascal
  }
  deriving (Show)

data Reaction = Reaction
  { name :: ReactionName,
    rate :: ReactionRate,
    reactants :: NonEmpty ReactionComponent,
    products :: NonEmpty ReactionProduct,
    conditions :: ReactionConditions
  }
  deriving (Show)

data ReactionPath
  = ReactionPathStep Molecule ReactionName ReactionPath -- from molecule, reaction name, next step
  | ReactionPathFinal Molecule -- final molecule
  deriving (Show)