#!/usr/bin/env escript
%%! -smp enable -sname erlshell -setcookie antidote


main(Args) ->
	io:format("I was called with ~p ~n", [Args]),
	[Filename] = Args,
	Ops = file_to_ops(Filename),
	Ops2 = split(Ops),
	print_list(Ops2),
	print_list(Ops),
	io:format("~p~n", [Ops]),
	io:format("~p~n", [Ops2]),
	Ops3 = client_list(Ops),
	io:format("~p~n", [Ops3]),

	ok.

file_to_ops(Filename) ->
	{ok, Binary} = file:read_file(Filename),
	{ok, C2, _} = erl_scan:string(unicode:characters_to_list(Binary)),
	%% With a fun, because escript doesn't seem to like higer order functions
	transform(lists:filter(fun(E) -> find(E) end, C2))
	.
% Split the sessions from the list.
split(L) -> split(L, [], []).
split([], Acc, Cur) -> Acc++Cur;
split([ {']', _} | R ], Acc, Cur) -> 
	case Cur of
		[] -> split(R, Acc, []);
		_ ->split(R, Acc++[Cur], [])
	end;
split([ {'[', _} | R ], Cur,  Acc) -> 
	case Cur of
		[] -> split(R, Acc, []);
		_ ->split(R, Acc++[Cur], [])
	end;
split([ E | R ], Acc, Cur) -> 
	split(R, Acc, Cur++[E])
	.

%% This is not generic, but it's very simple to implement:
%% Three levels of depth in the section: all, clients, sessions

client_list([], Acc) -> Acc;
client_list([ {']', _} | R], Acc) -> Acc;
client_list([ {'[', _} | R], Acc) ->
	{NewR, NewAcc} = session_list(R, []),
	client_list(NewR, Acc++[NewAcc]).
client_list(L) -> client_list(tl(L), []).

session_list( [], Acc) -> {[], Acc};
session_list([ {'[', _} | R], Acc) ->
	{NewR, NewAcc } = ops_list(R, []),
	session_list(NewR, Acc++[NewAcc] )
	;
session_list([ {']', _} | R], Acc) ->
	{R, Acc}.

ops_list([], Acc) ->
	{[], Acc};
ops_list([ {']', _} | R ], Acc) ->
	{R, Acc}
;
ops_list([ E | R ], Acc) ->
	ops_list(R, Acc++[E])
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
