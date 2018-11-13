%%%-------------------------------------------------------------------
%% @doc client public API
%% @end
%%%-------------------------------------------------------------------

-module(client_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).
-export([run/0]).

var_x() -> {"x", antidote_crdt_register_lww, "bucket"}.

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    io:format("Successfully started with args: ~p  ~n", [_StartArgs]),
    client_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

%% Write and read a value

test_1() ->
	Register = {<<"a">>, antidote_crdt_register_lww, <<"bucket">>},
	Counter_key = {"k", antidote_crdt_counter, "bucket"},
	{ok, Pid} = antidotec_pb_socket:start_link({127,0,0,1}, 8087),
	{ok, TxId} =  antidotec_pb:start_transaction(Pid, ignore, []),
	RValue1 = [{Register, assign, <<"watabanga">>}],
	{ok, Register_Value} = antidotec_pb:read_objects(Pid, [Register], TxId ),
	ok = antidotec_pb:update_objects(Pid, RValue1,TxId),
	{ok, Register_Value_2} = antidotec_pb:read_objects(Pid, [Register], TxId ),
	io:format("Current value: ~p ~n", [Register_Value_2]),
	{ok, TimeStamp} = antidotec_pb:commit_transaction(Pid, TxId),


	{ok, Tx2} =  antidotec_pb:start_transaction(Pid, TimeStamp, []),
	{ok, Register_Value_3} = antidotec_pb:read_objects(Pid, [Register], Tx2 ),
	io:format("Current value 2: ~p ~n", [Register_Value_3]),
	{ok, TimeStamp2} = antidotec_pb:commit_transaction(Pid, Tx2),


	{ok, Tx3} =  antidotec_pb:start_transaction(Pid, ignore, []),
	{ok, Counter_Value} = antidotec_pb:read_objects(Pid, [Counter_key], Tx3 ),
	io:format("Current value 2: ~p ~n", [Counter_Value]),
	{ok, TimeStamp2} = antidotec_pb:commit_transaction(Pid, Tx3),
	
	

	% no start transactions
%	{ok, TxId} = rpc:call(Pid, antidote, start_transaction, [ignore, []]),
	% {ok, TxId} =antidotec_pb_socket:start_transaction(Pid,ignore),
	%{ok, [Register]}  = rpc:call(Pid, antidote, read_objects, [[Register], TxId]),

	%{ok, [CounterVal]} = rpc:call(Node, antidote, read_objects, [[CounterObj], TxId]),
	%{assign, term()}.
	%ok = rpc:call(Node, antidote, update_objects, [[{CounterObj, increment, 1}], TxId]),
	%ok = rpc:call(Pid, antidote, update_objects, [[{Register, assign, 150}], TxId]),
	_Disconnected = antidotec_pb_socket:stop(Pid),	
	ok.

update_reg_test() ->
    Bucket = <<"bket">>,
    Key = <<"pb_client_SUITE_update_reg_test">>,
    Bound_object = {Key, antidote_crdt_register_lww, Bucket},

    {ok, Pid} = antidotec_pb_socket:start({127,0,0,1}, 8087),
    {ok, TxId} = antidotec_pb:start_transaction(Pid,
                                                ignore, []),
    ok = antidotec_pb:update_objects(Pid,
                                     [{Bound_object, assign, <<"10">>}],
                                     TxId),
    {ok, [Val_tmp]} = antidotec_pb:read_objects(Pid, [Bound_object], TxId),
    io:format("I got the value ~p ~n", [Val_tmp]),
    {ok, _} = antidotec_pb:commit_transaction(Pid, TxId),
    %% Read committed updated
    {ok, Tx2} = antidotec_pb:start_transaction(Pid, ignore, []),
    {ok, [Val]} = antidotec_pb:read_objects(Pid, [Bound_object], Tx2),
    {ok, _} = antidotec_pb:commit_transaction(Pid, Tx2),
    io:format("I got the value ~p ~n", [Val]),
    %?assertEqual(<<"10">>, antidotec_reg:value(Val)),
    _Disconnected = antidotec_pb_socket:stop(Pid).



%%====================================================================
%% External functions
%%====================================================================

run() ->
	io:format("Hello world~n"),
	test_1(),
	c:q().
