%%
%% Copyright (C) 2014 Björn-Egil Dahlberg
%%
%% File:    monos_app.erl
%% Author:  Björn-Egil Dahlberg
%% Created: 2014-09-11
%%
-module(monos_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    monos_sup:start_link().

stop(_State) ->
    ok.
