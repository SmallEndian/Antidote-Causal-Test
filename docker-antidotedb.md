In one console,

```
docker-compose -f antidote.yml up
```

When the nodes are ready, in another console,

```
docker exec -it antidote1 /opt/antidote/bin/env eval $(cat connect.erl)
```