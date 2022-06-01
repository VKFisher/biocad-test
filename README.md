# biocad-test

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

* `Experiment.hs` : пример выполнения действий указанных в условии задания
* `Db.hs` : взаимодействие с базой данных
* `Domain.hs` : типы данных предметной области
* `SampleData.hs` : генерация тестовых данных
