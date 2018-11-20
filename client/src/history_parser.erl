-module(history_parser).
-author("Jonathan Sid-Otmane <Jonathan.SidOtmane@gmail.com>"). 

-export([main/1, print_list/1, parse/1]).



%%====================================================================
%% Test Functions
%%====================================================================

print_list([]) -> 
	io:format("~n");
print_list([ E | R ]) -> 
	io:format("~p", [E]),
	print_list(R).

client(Transactions) ->
	Count = length(Transactions),
	io:format(user, "I'a a client with ~p transactions ~n", [Count]),
	lists:map(fun(E) ->
			       Count = length(E),
			       io:format(user, "I'a a transaction with ~p operations ~n", [Count])
	       end, Transactions),
	ok.

main(Args) ->
	io:format("I was called with ~p ~n", [Args]),
	[Filename] = Args,
	Content = file_to_ops(Filename),
	%Operations = client_list(Content),
	lists:map(fun(E) -> 
		 spawn(fun() -> 
				       client(E) end) 
			  end, Content),

	%% TODO: Replace this _hack_ with a monitor
	timer:sleep(1000),
	io:format("~p~n", [Content]),
	ok.

parse(Filename) ->
		All_Ops = file_to_ops(Filename),
		First = first_transaction(All_Ops),
		lists:map(fun(Client_Session) -> [First]++Client_Session end,
				  All_Ops)
		.
%%====================================================================
%%  Parse Functions
%%====================================================================


%% This is not generic, but it was very simple to implement:
%% Three levels of depth in the section: all, clients, sessions
%% This'll create a list of lists of lists that we'll be able give or client so that they may reproduce 
%% our last run.
client_list([], Acc) -> Acc;
client_list([ {']', _} | _R], Acc) -> Acc;
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
%
%%% History Parsing
%%% Part of the transformation from history file to list.
%%% The input file should be in the format used by Ranadeep Biswas
%

file_to_ops(Filename) ->
	{ok, Binary} = file:read_file(Filename),
	{ok, C2, _} = erl_scan:string(unicode:characters_to_list(Binary)),
	%% With a fun, because escript doesn't seem to like higer order functions
	client_list(transform(lists:filter(fun(E) -> find(E) end, C2)))
	.

%% Removes unnnecessary symbols
find({':', _}) -> false;
find({'<', _}) -> false;
find({'>', _}) -> false;
find({',', _}) -> false;
find({'(', _}) -> false;
find({')', _}) -> false;
find(_E) -> true.

%% Merges several operations into a single tuple
transform([]) -> [];
transform([ {var, _, Op},{integer, _, Var},{integer, _, Val}  |R] ) ->  [ {Op, Var, Val} | transform(R) ] ;
transform([E | R]) -> [E | transform(R)].

%% In order to init everything at zero, extract all variables names from script
%% This is to be the universal first transaction
first_transaction(List) ->
		lists:map(fun(VarName) -> {'W', VarName, 0} end,
				  lists:usort(lists:map(fun({_, VarName, _}) -> VarName end,
										lists:flatten(List))))
		.
