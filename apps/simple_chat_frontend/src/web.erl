%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MODULE INFO                                                                %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(web).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% INCLUDES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-include_lib("kernel/include/inet.hrl").
-include_lib("simple_chat/include/backend.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EXPORTS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%
% Session Interface
%
-export([new_cookie_session/1,
         cookieval_to_opaque/1,
         delete_cookie_session/1
        ]).

%%%%%
% Management Interface
%
-export([start/0, start_link/1, stop/0]).

%%%%%
% gen_server callbacks
%
-export([init/1,
         handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%%%%%
% Webpage callbacks
%
-export([do/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DEFINES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-define(SERVER_ADDR, {127,0,0,1}).
-define(SERVER_PORT, 8080).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% RECORDS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%
%% @type session() = {user::string()}
%%

-record(state, {}).
-record(session, {id, user}).
-record(cookie, {key, value}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EXPORTED FUNCTIONS / Session Interface
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

new_cookie_session(Session) ->
    gen_server:call(?MODULE, {new_cookie_session, Session}).

delete_cookie_session(SessionId) ->
    gen_server:cast(?MODULE, {delete_cookie_session, SessionId}).

cookieval_to_opaque(SessionId) ->
    gen_server:call(?MODULE, {cookieval_to_opaque, SessionId}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EXPORTED FUNCTIONS / Management Interface
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start() -> gen_server:start({local, ?MODULE}, ?MODULE, no_args, []).

start_link(Host) -> gen_server:start_link({local, ?MODULE}, ?MODULE, [Host], []).

stop() -> gen_server:cast(?MODULE, stop).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EXPORTED FUNCTIONS / Message callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Subscribe the user.
-spec subscribe(backend_db:id(), backend_db:name(), backend_db:password()) -> ok | {error, Reason :: term()}.
subscribe(Id, User, Password) ->
    gen_server:call(backend_server, {add_user, Id, User, Password}).

%% Unsubscribe the user.
-spec unsubscribe(backend_db:id()) -> ok | {error, Reason :: term()}.
unsubscribe(Id) ->
	gen_server:call(backend_server, {delete_user, Id}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EXPORTED FUNCTIONS / gen_server callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([_Host]) ->
    session_couter = ets:new(session_couter, [named_table]),
    sessions = ets:new(sessions, [named_table, {keypos, #session.id}]),
    ets:insert(session_couter, {counter, 0}),
    PrivDir = code:priv_dir(simple_chat_frontend),
    DocumentRootDir = filename:join([PrivDir, "www"]),
    ServerConf =
        [{port, ?SERVER_PORT},
         {server_name, "simple_chat"},
         {bind_address, ?SERVER_ADDR},
         {server_root, PrivDir},
         {document_root, DocumentRootDir},
         {modules, [mod_alias,
                    mod_auth,
                    mod_esi,
                    mod_actions,
                    mod_get,
                    mod_head,
                    mod_log,
                    mod_trace]},
         {error_log, "logs/error_log.log"},
         {security_log, "logs/security_log.log"},
         {transfer_log, "logs/transfer_log.log"},
         {directory_index, ["index.html"]},
         {erl_script_alias, {"/simple_chat", [?MODULE]}},
         {mime_types,[{"html", "text/html"},
                        {"htm", "text/html"},
                        {"css", "text/css"},
                        {"svg", "image/svg+xml"},
                        {"png", "image/png"},
                        {"exe", "application/octet-stream"},
                        {"js", "application/javascript"}]}
        ],
    inets:start(httpd, ServerConf),
    {ok, #state{}}.

handle_call({new_cookie_session, Session}, _, State) ->
    Id = ets:update_counter(session_couter, counter, {2, 1}),
    ets:insert(sessions, Session#session{id = Id}),
    erlang:send_after(600000, self(), {delete_cookie_session, Id}),
    {reply, Id, State};
handle_call({cookieval_to_opaque, SessionId}, _, State) ->
    Reply =
        case ets:lookup(sessions, SessionId) of
            [Session] -> {ok, Session};
            [] -> {error, no_session}
        end,
    {reply, Reply, State};
handle_call(Call, _, State) ->
    io:format("Unexpected call in ~p:~p~n", [?MODULE, Call]),
    {noreply, State}.

handle_cast({delete_cookie_session, SessionId}, State) ->
    ets:delete(sessions, SessionId),
    true = unsubscribe(SessionId),
    {noreply, State};
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(Cast, State) ->
    io:format("Unexpected cast in ~p:~p~n", [?MODULE, Cast]),
    {noreply, State}.

handle_info({delete_cookie_session, SessionId}, State) ->
    handle_cast({delete_cookie_session, SessionId}, State);
handle_info(Info, State) ->
    io:format("Unexpected info in ~p:~p~n", [?MODULE, Info]),
    {noreply, State}.

terminate(_, _) ->
    inets:stop(httpd, {?SERVER_ADDR, ?SERVER_PORT}),
    ok.

code_change(_, State, _) -> {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EXPORTED FUNCTIONS/WEBPAGE CALLBACKS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

do(SSID, Headers, Args) ->
    Tokens = string:lexemes(Args, ",{}"),
    SplitArgs = [string:lexemes(X, ":") || X <- Tokens],
    Vals = method_values(SSID, proplists:get_value(request_method, Headers), SplitArgs, Headers),
    mod_esi:deliver(SSID, Vals).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% INTERNAL FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

headers(SessId, Data) ->
    Headers = [[Key ++ ": " ++ Val ++ "\r\n" || {Key, Val} <- Data], ["\r\n"]],
    mod_esi:deliver(SessId, lists:flatten(Headers)).

method_values(SSID, "POST", [_, [_, U], [_, Pass], [_, ConfirmPass]], _Headers) ->
    case string:equal(Pass, ConfirmPass) of
        true ->
            [User] = string:lexemes(U, "\""),
            Username = string:lowercase(User),
            Session = #session{user = Username},
            Cookie = ?MODULE:new_cookie_session(Session),
            ok = subscribe(Cookie, Username, Pass),
            headers(SSID, [{"Set-Cookie", "ssid = " ++ integer_to_list(Cookie) ++
                            "; SameSite=None; Secure; Max-Age=600"},
                           {"Content-Type", "application/javascript"}]),
            Vals = "{\"login\": [\"" ++ integer_to_list(Cookie) ++ "\", \"" ++ Username ++ "\"]}",
            Vals;
        false -> "{\"login\":\"" ++ atom_to_list(false) ++ "\"}"
    end;
method_values(SSID, _Method, _Args, Headers) ->
    case check_cookie(proplists:get_value(http_cookie, Headers)) of
        {ok, Session, Cookie} ->
            Messages = backend_server:get_all_messages(),
            Vals = jsx:encode(#{<<"login">> => [list_to_binary(Cookie), list_to_binary(Session#session.user)],
                                <<"messages">> => Messages}),
            headers(SSID, [{"Content-Type", "application/javascript"}]),
            Vals;
        _ ->
            "{\"login\":\"" ++ atom_to_list(false) ++ "\"}"
    end.

%% @spec () -> 
%
%% this function extracts the session from the cookie
check_cookie(undefined) ->
    {error, nocookie};
check_cookie(Cookies) ->
    AttrValues = string:lexemes(Cookies, "; "),
    CookiesTuples =
    lists:map(
        fun(AttrValue) ->
            [Name, Value] = string:lexemes(AttrValue, "="),
            #cookie{key = Name, value = Value}
        end,
        AttrValues),
    case lists:keyfind("ssid", #cookie.key, CookiesTuples) of
        #cookie{value = Val} ->
            case ?MODULE:cookieval_to_opaque(list_to_integer(Val)) of
                {ok, Session} -> {ok, Session, Val};
                Else -> Else
            end;
        false ->
            {error, nocookie}
    end.