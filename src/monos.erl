%%
%% Copyright (C) 2014 Björn-Egil Dahlberg
%%
%% File:    monos.erl
%% Author:  Björn-Egil Dahlberg
%% Created: 2014-09-11
%%

-module(monos).
-export([nprocs/0, avg1/0, avg5/0, avg15/0, util/0, util/1]).
-export([go/0]).

-define(SERVICE, monos_service).

go() ->
    [monos_lib:machine(),
     monos_lib:memory(),
     monos_lib:disks(),
     monos_lib:loadavg()].

nprocs()   -> call(nprocs).
avg1()     -> call(avg1).
avg5()     -> call(avg5).
avg15()    -> call(avg15).
util()     -> call(util).
util(Args) -> call({util, Args}).

call(Req) -> call(Req,infinity).
call(Req,Tmo) -> gen_server:call(?SERVICE, Req, Tmo).
