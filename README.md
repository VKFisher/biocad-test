# biocad-test

[Условие задачи](/docs/spec.md)

## Deploy database

```bash
docker-compose up --build -dV db
```

interface: <http://localhost:7474/>

credentials: `neo4j:password`

## Run experiment

```bash
stack run biocad-test-exe
```

## Project structure

* `Db.hs` : взаимодействие с базой данных
  * `putReaction` : принимает реацию на вход и загружает её в базу
  * `getReaction` : по номеру реакции в базе возвращает её в Haskell-объект
  * `getShortestPath` : по двум заданным молекулам ищет путь через реакции и молекулы с наименьшей длиной
* `Experiment.hs` : пример выполнения действий указанных в условии задания
* `Domain.hs` : типы данных предметной области
* `SampleData.hs` : генерация тестовых данных

## Additional questions

В процессе развития системы будут добавляться различные компоненты, например, механизм реакции. Предлагается ответить на следующие вопросы:

* какие абстракции вы бы предложили ввести в Haskell-реализацию?

```haskell
  data Intermediate 
    = IntermediateAtom Atom 
    | IntermediateIon Ion 
    | IntermediateMolecule Molecule 
    | ...

  data MechanismType 
    = Homolytic 
    | Heterolytic 
    | Pericyclic

  data Transitions
    = TransitionStep (NonEmpty Intermediate) ReactionRate TransitionPath
    | TransitionFinal (NonEmpty ReactionProduct) ReactionRate

  data ReactionMechanism = ReactionMechanism {
      type :: MechanismType,
      transitions :: Transitions
    }

  getOverallRate :: ReactionMechanism -> ReactionRate
  isMultiStep :: ReactionMechanism -> Bool
```

* какие абстракции или вспомогательные компоненты можно ввести на уровне базы данных, чтобы новые полученные знания ладно укладывались в систему?

  ![mechanism-graph.png](/docs/img/mechanism-graph.png)
