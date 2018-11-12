#!/usr/bin/env escript
%%! -smp enable -sname erlshell -setcookie antidote


main(Args) ->
	io:format("I was called with ~p ~n", [Args]),
	[Filename] = Args,
	Ops = file_to_ops(Filename),
	print_list(Ops),

	ok.

file_to_ops(Filename) ->
	{ok, Binary} = file:read_file(Filename),
	{ok, C2, _} = erl_scan:string(unicode:characters_to_list(Binary)),
	%% With a fun, because escript doesn't seem to like higer order functions
	transform(lists:filter(fun(E) -> find(E) end, C2))
	.

%% Removes unnnecessary symbols
find({':', _}) -> false;
find({'<', _}) -> false;
find({'>', _}) -> false;
find({',', _}) -> false;
find({'(', _}) -> false;
find({')', _}) -> false;
find(E) -> true.

%% Merges several operations into a single tuple
transform([]) -> [];
transform([ {var, _, Op},{integer, _, Var},{integer, _, Val}  |R] ) ->  [ {Op, Var, Val} | transform(R) ] ;
transform([E | R]) -> [E | transform(R)].

print_list([]) -> 
	io:format("~n");
print_list([ E | R ]) -> 
	io:format("~p", [E]),
	print_list(R).

old(_) ->

		rpc:call(antidote@antidote2, inter_dc_manager, start_bg_processes, [stable]),
		rpc:call(antidote@antidote3, inter_dc_manager, start_bg_processes, [stable]),
		{ok, Desc1} = rpc:call(antidote@antidote1, inter_dc_manager, get_descriptor, []),
		{ok, Desc2} = rpc:call(antidote@antidote2, inter_dc_manager, get_descriptor, []),
		{ok, Desc3} = rpc:call(antidote@antidote3, inter_dc_manager, get_descriptor, []),
		Descriptors = [Desc1, Desc2, Desc3],
		rpc:call(antidote@antidote1, inter_dc_manager, observe_dcs_sync, [Descriptors]),
		rpc:call(antidote@antidote2, inter_dc_manager, observe_dcs_sync, [Descriptors]),
		rpc:call(antidote@antidote3, inter_dc_manager, observe_dcs_sync, [Descriptors]),
		io:format("Connection setup!~n").
