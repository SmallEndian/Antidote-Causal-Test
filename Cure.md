# How Cure does things:

With Cure, transactions are not a suit of R / W opearations which each give back a result, but rather a list of the keys you want to read, the keys you want to update and the values you wish to give them all.

And Cure reads your own write, yes, but it always reads them in the past. It doesn't, can't read them during the transactions.

Accross a singe DC, all operations are fully ordered, which is why when a transaction is committed, it goes through a 2PC.

So, the file we got from P7 is no good!

## What a transactions sees:

The values of previously committed transactions. That's another instance of the whole _availability_ vs _freshness_.

IV. B. Session guarantees

## Some reading material:

[Here](https://antidotedb.gitbook.io/documentation/api/native-api#transactions)

And the [client](https://github.com/AntidoteDB/antidote-erlang-client/tree/74fc7e093796d3ef8c4c253a7504931f41099293) 
