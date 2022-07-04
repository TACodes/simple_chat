-module(backend_db).

-export([create_db/0,
         new_user/4,
         delete_user/2,
         lookup/2,
         new_message/3,
         delete_message/3,
         close/1,
         all_users/1
        ]).

-type dbref() :: ets:tab().
-type id() :: integer().
-type password() :: string().
-type name() :: string() | binary().
-type message() :: {calendar:datetime(), string()}.
-type messages() :: list(message()).

-include_lib("simple_chat/include/backend.hrl").

-spec create_db() -> dbref().
create_db() ->
    ets:new(users, [set, named_table, public, {keypos, #user.id}, {write_concurrency, true},
            {read_concurrency, true}, {decentralized_counters, true}]).

%% Look up an user record by the user id.
-spec lookup(id(), dbref()) -> #user{} | {error, instance}.
lookup(Id, DbRef) ->
    case ets:lookup(DbRef, Id) of
        [] ->
            {error, instance};
        [User] ->
            User
    end.

%% Adds a new user to the DB.
-spec new_user(id(), name(), password(), dbref()) -> ok | {error, exists}.
new_user(Id, Name, Password, DbRef) ->
    case lookup(Id, DbRef) of
        {error, instance} ->
            ets:insert(DbRef, #user{id = Id, name = Name, password = Password}),
            ok;
        _User ->
            {error, exists}
    end.


%% Delete user from the DB.
-spec delete_user(id(), dbref()) -> ok | {error, not_exists}.
delete_user(Id, DbRef) ->
    case lookup(Id, DbRef) of
        {error, instance} ->
            {error, not_exists};
        _User ->
            ets:delete(DbRef, Id)
    end.

%% Add message to corresponding user
-spec new_message(id(), message(), dbref()) -> ok | {error, instance} | {error, string()}.
new_message(Id, Message, DbRef) ->
    case lookup(Id, DbRef) of
        {error, instance} ->
            {error, instance};
        User = #user{id = Id, messages = OldMessages} ->
            NewUser = User#user{messages = [Message | OldMessages]},
            ets:insert(DbRef, NewUser),
            ok
    end.

%% Delete message from corresponding user
-spec delete_message(id(), message(), dbref()) -> ok | {error, instance} | {error, string()}.
delete_message(Id, Message, DbRef) ->
    case lookup(Id, DbRef) of
        {error, instance} ->
            {error, instance};
        User = #user{id = Id, messages = OldMessages} ->
            NewUser = User#user{messages = lists:keydelete(Message, 2, OldMessages)},
            ets:insert(DbRef, NewUser),
            ok
    end.

%% Deletes the DB.
-spec close(dbref()) -> ok.
close(DbRef) ->
    ets:delete(DbRef),
    ok.

%% List all users
-spec all_users(dbref()) -> [#user{}].
all_users(DbRef) ->
    ets:tab2list(DbRef).