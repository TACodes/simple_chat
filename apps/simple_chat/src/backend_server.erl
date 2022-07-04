-module(backend_server).
-behaviour(gen_server).

%% Public API
-export([start_link/0,
         stop/0,
         get_user/1,
         list_users/0,
         get_all_messages/0]).

%% Callback API for the gen_server
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

-include_lib("simple_chat/include/backend.hrl").

-define(SERVER, ?MODULE).

-record(state, {account_table, sockets = []}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EXPORTED FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Start the backend server.
-spec start_link() -> {ok, pid()} | {error, Reason :: term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, no_args, []).

%% Stop the backend server.
-spec stop() -> ok.
stop() ->
    gen_server:call(?SERVER, stop).

%% Retrieve the user informations by the user name.
-spec get_user(backend_db:id()) -> #user{} | {error, Reason :: term()}.
get_user(Id) ->
    gen_server:call(?SERVER, {get_user, Id}).

%% List all users.
-spec list_users() -> [#user{}].
list_users() ->
    gen_server:call(?SERVER, {list_users}).

add_new_message(Id, Msg) ->
    gen_server:cast(?SERVER, {new_message, Id, Msg}).

%% Get all messages from the users.
-spec get_all_messages() -> ok | {error, Reason :: term()}.
get_all_messages() ->
    gen_server:call(?SERVER, {get_all_messages}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% CALLBACK API FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(no_args) ->
    process_flag(trap_exit, true),
    DbRef = backend_db:create_db(),
    {ok, ListenSocket} = gen_tcp:listen(8081, [binary,{packet,0},{reuseaddr,true},{active, true}]),
    persistent_term:put({?MODULE,sockets}, []),
    spawn(fun() -> paral_connect(ListenSocket) end),
    {ok, #state{account_table = DbRef}}.

handle_call({add_user, Id, Name, Password}, _From, #state{account_table=DbRef} = State) ->
    Result = backend_db:new_user(Id, Name, Password, DbRef),
    {reply, Result, State};

handle_call({delete_user, Id}, _From, #state{account_table=DbRef} = State) ->
    Result = backend_db:delete_user(Id, DbRef),
    {reply, Result, State};

handle_call({get_user, Id}, _From, #state{account_table=DbRef} = State) ->
    Result = case backend_db:lookup(Id, DbRef) of
                 {error, instance} ->
                     {error, "No users found with the name: " ++ Id};
                 User ->
                     User
             end,
    {reply, Result, State};

handle_call({list_users}, _From, #state{account_table=DbRef} = State) ->
    Result = backend_db:all_users(DbRef),
    {reply, Result, State};

handle_call({get_all_messages}, _From, #state{account_table=DbRef} = State) ->
    Users = backend_db:all_users(DbRef),
    Result = [U#user.messages || U <- Users],
    {reply, Result, State};

handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

handle_cast({new_message, Id, Message}, #state{account_table=DbRef} = State) ->
    backend_db:new_message(Id, Message, DbRef),
    {noreply, State};

handle_cast({delete_message, Name, Message}, #state{account_table=DbRef} = State) ->
    backend_db:delete_message(Name, Message, DbRef),
    %% update send all messages to clients
    {noreply, State};

handle_cast(Cast, State) ->
    {stop, {"Can not handle cast", Cast}, State}.

handle_info(Info, State) ->
    {stop, {"Can not handle info", Info}, State}.

terminate(_Reason, #state{account_table=DbRef} = _State) ->
    backend_db:close(DbRef).

code_change(_, State, _) ->
    {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% WEB COMMUNICATION - internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

paral_connect(Listen) ->
    {ok, Socket} = gen_tcp:accept(Listen),
    spawn(fun() -> paral_connect(Listen) end),
    wait(Socket).

wait(Socket) ->
     receive
         {tcp, Socket, Data} ->
             SocketList = persistent_term:get({?MODULE,sockets}),
             persistent_term:put({?MODULE,sockets}, [Socket | SocketList]),
             Key = get_key_from_data(string:lexemes(Data, [[$\r,$\n]])),
             Challenge = base64:encode(crypto:hash(sha,
 		            << Key/binary, "258EAFA5-E914-47DA-95CA-C5AB0DC85B11" >>)),
             Msg = prefix() ++
                  "WebSocket-Origin: http://localhost:8080\r\n" ++
                  "Sec-Websocket-Accept: " ++ binary_to_list(Challenge) ++ "\r\n" ++
                  "WebSocket-Location: ws://localhost:8081/\r\n\r\n",
             gen_tcp:send(Socket, Msg),
             loop(Socket);
         Any ->
             io:format("Received Any in wait:~p~n~n",[Any]),
             wait(Socket)
     end.

prefix() ->
    "HTTP/1.1 101 Web Socket Protocol Handshake\r\nUpgrade:
     WebSocket\r\nConnection: Upgrade\r\n".

loop(Socket) ->
    receive
        {tcp, Socket, Data} ->
            Payload = parse_data(Data),
            Info = parse_json_and_add_msg(Payload), 
            Frame = frame(Info),
            [gen_tcp:send(S, Frame)|| S <- persistent_term:get({?MODULE, sockets})],
            loop(Socket);
        Any ->
            io:format("Received Any in loop:~p~n~n",[Any]),
            loop(Socket)
    end.

get_key_from_data(DataList) ->
    [Key] =
    lists:filtermap(
        fun(X) ->
                case string:find(X, "Sec-WebSocket-Key") of
                    nomatch ->
                        false;
                    WSKey ->
                        [_, K] = string:lexemes(WSKey, ": "),
                        {true, K}
                end
        end, DataList),
    Key.

parse_data(<< _Fin:1, _Rsv:3/bits, 8:4, 1:1, _Len:7, MaskKey:32, Rest/bits >>) ->
    << MaskedCode:2/binary, Data/bits >> = Rest,
    unmask(MaskedCode, MaskKey),
    Left = 2 rem 4,
	Right = 4 - Left,
	MaskKey2 = (MaskKey bsl (Left * 8)) + (MaskKey bsr (Right * 8)),
	mask(Data, MaskKey2, <<>>);

parse_data(<< _Fin:1, _Rsv:3/bits, _Opcode:4, 1:1, _Len:7, MaskKey:32, Rest/bits >>) ->
    unmask(Rest, MaskKey);

parse_data(<< _Fin:1, _Rsv:3/bits, Opcode:4, 1:1, 126:7, Len:16, MaskKey:32, Rest/bits >>) when Len > 125, Opcode < 8 ->
	unmask(Rest, MaskKey);

parse_data(<< _Fin:1, _Rsv:3/bits, Opcode:4, 1:1, 127:7, 0:1, Len:63, MaskKey:32, Rest/bits >>) when Len > 125, Opcode < 8 ->
	unmask(Rest, MaskKey).

unmask(Data, MaskKey) ->
	mask(Data, MaskKey, <<>>).

mask(<<>>, _, Unmasked) ->
	Unmasked;
mask(<< O:32, Rest/bits >>, MaskKey, Acc) ->
	T = O bxor MaskKey,
	mask(Rest, MaskKey, << Acc/binary, T:32 >>);
mask(<< O:24 >>, MaskKey, Acc) ->
	<< MaskKey2:24, _:8 >> = << MaskKey:32 >>,
	T = O bxor MaskKey2,
	<< Acc/binary, T:24 >>;
mask(<< O:16 >>, MaskKey, Acc) ->
	<< MaskKey2:16, _:16 >> = << MaskKey:32 >>,
	T = O bxor MaskKey2,
	<< Acc/binary, T:16 >>;
mask(<< O:8 >>, MaskKey, Acc) ->
	<< MaskKey2:8, _:24 >> = << MaskKey:32 >>,
	T = O bxor MaskKey2,
	<< Acc/binary, T:8 >>.

payload_length(Payload) ->
	case iolist_size(Payload) of
		N when N =< 125 -> << N:7 >>;
		N when N =< 16#ffff -> << 126:7, N:16 >>;
		N when N =< 16#7fffffffffffffff -> << 127:7, N:64 >>
	end.

frame(Payload) ->
	Len = payload_length(Payload),
	[<< 1:1, 0:3, 1:4, 0:1, Len/bits >>, Payload].

parse_json_and_add_msg(<<>>) ->
    <<>>;

parse_json_and_add_msg(Payload) ->
    JsonObject = jsx:decode(Payload),
    IDName = maps:get(<<"id">>, JsonObject),
    Msg = maps:get(<<"text">>, JsonObject),
    [Id, Name] = IDName,
    {{_,_,_},{Hour,Min,_Sec}} = erlang:localtime(),
    Info = jsx:encode(#{<<"name">> => Name, <<"text">> => Msg, <<"time">> => [Hour, Min]}),
    add_new_message(binary_to_integer(Id), Info),
    Info.