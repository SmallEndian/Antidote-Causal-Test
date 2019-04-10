{ok, TxId} = antidote:start_transaction(ignore, []).
BCounter = {"Key", antidote_crdt_counter_b, "Bucket"}.
antidote:read_objects([BCounter], TxId).
antidote:update_objects([{BCounter, increment, 1}],TxId).
