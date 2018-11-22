#!/usr/bin/env escript
%%! -smp enable -sname erlshell -setcookie antidote

names(Num) when Num > 0 -> [list_to_atom(lists:flatten(io_lib:format("antidote@antidote~w", [Num]))) | names(Num - 1)];
names(_) -> [].

main(_) ->
  NumDC = 3,
  DCs = names(NumDC),
  lists:foreach(fun(DC) -> rpc:call(DC, inter_dc_manager, start_bg_processes, [stable]) end, DCs),
  Descriptors = lists:map(fun(DC) -> {ok, Desc} = rpc:call(DC, inter_dc_manager, get_descriptor, []), Desc end, DCs),
  lists:foreach(fun(DC) -> rpc:call(DC, inter_dc_manager, observe_dcs_sync, [Descriptors]) end, DCs),
  io:fwrite("AntidoteDB: cluster setup for ~w datacenters!~n", [NumDC]).
