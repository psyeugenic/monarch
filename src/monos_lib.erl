%%
%% Copyright (C) 2014 Björn-Egil Dahlberg
%%
%% File:    monos_lib.erl
%% Author:  Björn-Egil Dahlberg
%% Created: 2014-09-11
%%

-module(monos_lib).
-export([machine/0,
	 memory/0,
	 disks/0,
	 loadavg/0,
	 processes/0]).

-on_load(load_nif/0).

%% API

machine() -> erlang:nif_error(undef).
memory() -> erlang:nif_error(undef).
loadavg() -> erlang:nif_error(undef).
processes() -> erlang:nif_error(undef).
disks() -> erlang:nif_error(undef).


%% NIF handler

load_nif() ->
    SoName = case code:priv_dir(monos) of
        {error, bad_name} -> filename:join("priv", "libmonos");
        Dir -> filename:join(Dir, "libmonos")
    end,
    erlang:load_nif(SoName, 0).
