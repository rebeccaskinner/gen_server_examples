%%%-------------------------------------------------------------------
%%% @author Rebecca Skinner <rebecca@rebeccaskinner.net>
%%% @copyright (C) 2015, Rebecca Skinner
%%% @doc
%%%
%%% @end
%%% Created :  9 Nov 2015 by Rebecca Skinner <rebecca@rebeccaskinner.net>
%%%-------------------------------------------------------------------
-module(echoserver).

-behaviour(gen_server).

%% API
-export([start_link/0,
         listen_on/2,
         stop_listen/2,
         show_servers/1
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(echoserver, {server_socket :: gen_tcp:socket(),
                     client_sockets :: [gen_tcp:socket()],
                     name :: anonymous | string()}).
-record(state, {servers :: [#echoserver{}], name_map::map()}).

-type serverResponse() :: {ok, term()} | {error, string()}.

-define(ACTION_MAP, #{listen_on => fun listen_on_internal/2,
                      stop_listen => fun stop_listen_internal/2,
                      show_servers => fun show_servers_internal/2
                     }).

%%%===================================================================
%%% API
%%%===================================================================

-spec listen_on(pid(), integer()) -> serverResponse().
listen_on(Pid, PortNumber) -> gen_server:call(Pid, {listen_on,PortNumber}).

-spec stop_listen(pid(), integer() | atom()) -> serverResponse().
stop_listen(Pid, ServerID) -> gen_server:call(Pid, {stop_listen, ServerID}).

-spec show_servers(pid()) -> [integer()].
show_servers(Pid) -> gen_server:call(Pid, show_servers).

-spec lookup_action(atom() | {atom(), term()}) -> fun((term(), #state{}) -> {ok,
  #state{}} | {error, string()}).
lookup_action({Action, Param}) ->
  case map:lookup(Action, ?ACTION_MAP) of
    {
  

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
  case gen_server:start_link({local, ?SERVER}, ?MODULE, [], []) of
    {ok, Pid} -> Pid;
    {_,Reason} -> {error, Reason}
  end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
  {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(Action, _From, State) ->
  {F, Params} = lookup_action(Action),
  {Response, NewState} = 
    case F(Params, State) of
      {ok, NewState} -> {ok, NewState};
      {error, Reason} -> {{error, Reason}, State}
    end,
  {reply, Response, NewState}.
        

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
