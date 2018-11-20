%%%-------------------------------------------------------------------
%% @doc client public API
%% @end
%%%-------------------------------------------------------------------

-module(client_app).
-author("Jonathan Sid-Otmane <Jonathan.SidOtmane@gmail.com>"). 

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).
-export([run/0, run/1, run/2]).

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


%% Si;ple example execution: Three clients execute two transactions of five operations each.
%% The operations here have been copy and pasted, so that won't be useful for a test of causality
example() -> [[[{'R',1,52},{'R',1,52},{'W',3,61},{'W',1,53},{'W',1,54},{'R',2,62},{'R',2,62}],
	       [{'W',4,60},{'W',2,63},{'R',4,60},{'W',2,64},{'R',1,54},{'W',1,55},{'R',1,55}]],
	      [[{'R',1,52},{'R',1,52},{'W',3,61},{'W',1,53},{'W',1,54},{'R',2,62},{'R',2,62}],
	       [{'W',4,60},{'W',2,63},{'R',4,60},{'W',2,64},{'R',1,54},{'W',1,55},{'R',1,55}]],
	      [[{'R',1,52},{'R',1,52},{'W',3,61},{'W',1,53},{'W',1,54},{'R',2,62},{'R',2,62}],
	       [{'W',4,60},{'W',2,63},{'R',4,60},{'W',2,64},{'R',1,54},{'W',1,55},{'R',1,55}]]].

%% We just hardcode the list of Antidote instances, for simplicity
ports() -> [{{127,0,0,1}, 8087},{{127,0,0,1}, 8088},{{127,0,0,1}, 8089}].

client_thread(Transactions, {Ip, Port}) ->
	Bucket = <<"bucket">>,
	{ok, DC_Socket} = antidotec_pb_socket:start_link(Ip, Port),

	{TimeStamp , History} = lists:foldl(fun(Transaction, {TimeStamp, Old_History}) -> 
				    {ok, TxId} = antidotec_pb:start_transaction(DC_Socket, TimeStamp, []),

				    History = lists:reverse( lists:foldl(fun(Op,History ) ->
								[execute_op(Op, Bucket, DC_Socket, TxId) | History]
						end, [], Transaction)),

				    {ok, New_TimeStamp} = antidotec_pb:commit_transaction(DC_Socket, TxId),
				    {New_TimeStamp, [History | Old_History]}
		    end, {ignore, []}, Transactions),


	_Disconnected = antidotec_pb_socket:stop(DC_Socket),	
	%io:format("Client of ~p  ~p ~n", [Port, History]),
	History.
execute_op(Write = {'W', Key, Value}, Bucket, DC_Socket, TxId) ->
	Register = {<<Key>>, antidote_crdt_register_lww, Bucket},
	Ops = [{Register, assign, integer_to_binary(Value)}],
	%io:format("My args: ~p : ~p  ~n", [Register, Ops]),
	ok = antidotec_pb:update_objects(DC_Socket, Ops, TxId),
	Write;
execute_op({'R', Key, _}, Bucket, DC_Socket, TxId) ->
	Register = {<<Key>>, antidote_crdt_register_lww, Bucket},
	{ok, [{_,Value}]} = antidotec_pb:read_objects(DC_Socket, [Register], TxId),
	%io:format("My args: ~p ~p ~n", [Register, Value]),
	case Value of
		<<>> -> % We consider that uninitialized keys contain zero
			{'R', Key, 0};
		_ -> {'R', Key, binary_to_integer(Value)}
	end
	.



%% Write and read a value, without having committed the transaction and afterwads.
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

test_timestamp_across_dcs()->
		Register = {<<"a">>, antidote_crdt_register_lww, <<"bucket">>},
		lists:foldl(fun({Ip, Port}, TimeStamp) ->
									io:format("[~p,~p, ~p]~n", [Ip, Port, TimeStamp]),

									{ok, Pid} = antidotec_pb_socket:start_link(Ip, Port),
									{ok, TxId} =  antidotec_pb:start_transaction(Pid, TimeStamp, []),
									RValue1 = [{Register, assign, <<Port>>}],
									{ok, Register_Value} = antidotec_pb:read_objects(Pid, [Register], TxId ),
									ok = antidotec_pb:update_objects(Pid, RValue1,TxId),
									{ok, Register_Value_2} = antidotec_pb:read_objects(Pid, [Register], TxId ),
									io:format("Values: ~p -> ~p ~n", [Register_Value, Register_Value_2]),
									{ok, NewTimeStamp} = antidotec_pb:commit_transaction(Pid, TxId),
									_Disconnected = antidotec_pb_socket:stop(Pid),	
									NewTimeStamp
					end, ignore, ports())
		.

% test function
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

run(Filename, OutputFile) ->
	{ok, File} = file:open(OutputFile, write),
	io:fwrite(File, "[", [] ),
	lists:foreach(fun(Session) ->
				  io:fwrite(File, "~n[", [] ),
				  lists:foreach(fun(Transaction) ->
										       io:fwrite(File, "~n[", []),
							    lists:foreach( fun({Op, Var, Val}) ->
										       io:fwrite(File, "<~p(~p):~p>,", [Op, Var, Val])
								       end, Transaction),
										       io:fwrite(File, "\b],", [])
					    end, Session),
				  io:fwrite(File, "\b~n],", [] )
		  end,
		  run(Filename)),
	io:fwrite(File, "\b~n]~n", [] ),
	file:close(File).

run(Filename) ->
	Operations = history_parser:parse(Filename),
	Parent = self(),

	Children = lists:map( fun({T, DC_Addr}) -> 
				   spawn( fun()-> 
							  Parent ! {finished, client_thread(T, DC_Addr)}
					  end)
		   end,
		   lists:zip(Operations, ports())),
	Results = lists:map(fun(_) ->

					     % We only count on receiving our result
					     receive {finished, E} -> E end
			     end, Children),
	io:format("~n"), % cleans the output
	lists:map(fun(Session) -> lists:reverse(Session) end, Results).
run() ->
	lists:map( fun({T, DC_Addr}) -> client_thread(T, DC_Addr) end,
		   lists:zip(example(), ports())).
