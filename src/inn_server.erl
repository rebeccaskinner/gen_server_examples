%%%-------------------------------------------------------------------
%%% @author Rebecca Skinner <rebecca@rebeccaskinner.net>
%%% @copyright (C) 2015, Rebecca Skinner
%%% @doc
%%%
%%% @end
%%% Created :  9 Nov 2015 by Rebecca Skinner <rebecca@rebeccaskinner.net>
%%%-------------------------------------------------------------------
-module(inn_server).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, reserve_room/2, vacate_room/2,
         show_reserved/1]).

-define(SERVER, ?MODULE).

-define(CASTABLE_ACTIONS, [vacate]).

-record(hotelState, {rooms :: map()}).
-type reservation_action() :: {reserve | vacate, integer()} | reservations.

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
  {ok, Pid} = gen_server:start_link({local, ?SERVER}, ?MODULE, [], []),
  Pid.

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
  {ok, #hotelState{rooms = #{}}}.

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
handle_call(ReservationAction, _From, State) ->
  {Reply, NewState} =
    case callable_action(ReservationAction) of
      true -> dispatch_reservation(ReservationAction, State);
      false -> {error, State}
    end,
  {reply, Reply, NewState}.

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
handle_cast(ReservationAction, State) ->
  {_, NewState} =
    case castable_action(ReservationAction) of
      true -> dispatch_reservation(ReservationAction, State);
      false -> {error, State}
    end,
  {noreply, NewState}.

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
%%% API Helpers
%%%===================================================================

-spec reserve_room(pid(), integer()) -> ok | error.
reserve_room(Pid, RoomNumber) ->
  gen_server:call(Pid, {reserve, RoomNumber}).

-spec vacate_room(pid(), integer()) -> ok | error.
vacate_room(Pid, RoomNumber) ->
  gen_server:call(Pid, {vacate, RoomNumber}).

-spec show_reserved(pid()) -> string().
show_reserved(Pid) ->
  gen_server:call(Pid, reservations).

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec castable_action(reservation_action()) -> boolean().
castable_action({Action, _}) -> lists:member(Action, ?CASTABLE_ACTIONS).

-spec callable_action(reservation_action()) -> boolean().
callable_action(_) -> true.

-spec dispatch_reservation(reservation_action(), #hotelState{}) -> #hotelState{}.
dispatch_reservation({reserve, RoomNumber}, State) ->
  reserve_room_internal(RoomNumber, State);
dispatch_reservation({vacate, RoomNumber}, State) ->
  free_room(RoomNumber, State);
dispatch_reservation(reservations, State = #hotelState{rooms = Rooms}) ->
  {print_map(Rooms), State};
dispatch_reservation(_Action, _State) ->
  {error, "Invalid Request"}.

-spec print_map(map()) -> string().
print_map(M) -> 
  F = fun(K, V, Acc) ->
          Acc ++ io_lib:format("~p => ~p~n", [K, V])
      end,
  maps:fold(F, "", M).

-spec reserve_room_internal(integer(), #hotelState{}) -> #hotelState{}.
reserve_room_internal(RoomNumber, St = #hotelState{rooms = Rooms}) ->
  case maps:is_key(RoomNumber, Rooms) of
    true -> {error, St};
    false -> {ok, #hotelState{rooms = maps:put(RoomNumber, true, Rooms)}}
  end.

-spec free_room(integer(), #hotelState{}) -> #hotelState{}.
free_room(RoomNumber, #hotelState{rooms = Rooms}) ->
  Rooms1 = case maps:remove(RoomNumber, Rooms) of
             {badmap, _} -> Rooms;
             NewMap -> NewMap
           end,
  {ok, #hotelState{rooms = Rooms1}}.
