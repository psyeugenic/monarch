%%
%% Copyright (C) 2014 Björn-Egil Dahlberg
%%
%% File:    monarch_lib.erl
%% Author:  Björn-Egil Dahlberg
%% Created: 2014-09-11
%%

-module(monarch_lib).
-export([machine/0,
	 memory/0,
	 disks/0,
	 loadavg/0,
	 util/0,
	 processes/0]).

-on_load(load_nif/0).

%% API

machine()   -> erlang:nif_error(undef).
memory()    -> erlang:nif_error(undef).
loadavg()   -> erlang:nif_error(undef).
processes() -> erlang:nif_error(undef).
disks()     -> erlang:nif_error(undef).
cpu_util()  -> erlang:nif_error(undef).

util() ->
    %#{cpu_id => 7,idle => 13461918,kernel => 214612,nice_user => 0,user => 357013}
    [begin
         Tot = Idle + Kernel + Nice + User,
         U#{idle := Idle/Tot,
            kernel := Kernel/Tot,
            nice_user := Nice/Tot,
            user := User/Tot}
     end || #{idle := Idle, kernel := Kernel, nice_user := Nice, user := User} = U <- cpu_util()].

%% NIF handler

load_nif() ->
    SoName = case code:priv_dir(monarch) of
        {error, bad_name} -> filename:join("priv", "libmonarch");
        Dir -> filename:join(Dir, "libmonarch")
    end,
    erlang:load_nif(SoName, 0).
