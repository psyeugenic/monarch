%%
%% Copyright (C) 2014 Björn-Egil Dahlberg
%%
%% File:    monarch_disk_service.erl
%% Author:  Björn-Egil Dahlberg
%% Created: 2014-09-14
%%

-module(monarch_disk_service).

-behaviour(gen_server).

%% API
-export([
	start_link/0
    ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(CMD_REFRESH, refresh).

-record(state, {
	ratio = 0.4,
	interval = 5000,
	timer_ref
    }).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, set_timer(#state{})}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(?CMD_REFRESH, S0) ->
    Ds = monarch_lib:disks(),
    S1 = check_full_disks(Ds,S0),
    S2 = update_list(Ds,S1),
    io:format("~p~n", [Ds]),
    {noreply, set_timer(S2)};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

set_timer(#state{ interval=T } = S) ->
    S#state{
	timer_ref=erlang:send_after(T,self(),?CMD_REFRESH)
    }.

check_full_disks([M|Ms],S0) ->
    S1 = check_full_disk(M,S0),
    check_full_disks(Ms,S1);
check_full_disks([],S0) ->
    S0.

check_full_disk(#{ blocks := 0 }, S) -> S;
check_full_disk(#{ bfree := Free, blocks := Total, mountpoint := _Name },
		#state{ ratio=R }= S) when Free/Total < R ->
    S;
check_full_disk(_,S) -> S.

update_list(_,S1) ->
    S1.
