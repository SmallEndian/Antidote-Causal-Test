
## Launching

```bash
docker-compose up
docker exec antidote1 /code/connect.erl
```

## Background

During experiments on Antidote by Ranadeep Biswas and Constantin Enea. During their experiments, they found occurences of breaks in causality.

## The operations

The causality errors only become appatent once the database has to execute at least 14 transactions, each transaction consisting of 14 operations on 5 variables.

Once the error occurs, it involves 2 variables and 3 transactions. (And on the history we were shown, only two replicas).
