-module(monarch_lib_SUITE).
-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([t_machine/1, t_memory/1,
	  t_loadavg/1, t_processes/1,
	  t_disks/1,
	  t_cpu_util/1]).

all() -> [t_machine, t_memory,
	  t_loadavg, t_processes,
	  t_disks,
	  t_cpu_util].

init_per_suite(Config) ->
    Config.

end_per_suite(Config) ->
    Config.


t_machine(Config) when is_list(Config) ->
    M0 = monarch_lib:machine(),

    %% check keys

    #{boottime := Boottime,
	  hostname := Hostname,
      kernel_version := KernelVersion,
      machine := Machine,
      memsize := Memsize,
      model := Model,
      ncpu := Ncpu,
      osreldate := OsRelDate,
      osrelease := OsRelease,
      osrev := OsRev,
      ostype := OsType,
      pagesize := PageSize} = M0,
    
    %% check types

    true = is_integer(Boottime),
    true = is_binary(Hostname),
    true = is_binary(KernelVersion),
    true = is_binary(Machine),
    true = is_integer(Memsize),
    true = is_binary(Model),
    true = is_integer(Ncpu),
    true = is_integer(OsRelDate),
    %true = is_binary(OsRelDate),
    true = is_binary(OsRelease),
    true = is_integer(OsRev),
    true = is_binary(OsType),
    true = is_integer(PageSize),

    %% check repeated call is static

    M1 = monarch_lib:machine(),

    #{boottime := Boottime,
	  hostname := Hostname,
      kernel_version := KernelVersion,
      machine := Machine,
      memsize := Memsize,
      model := Model,
      ncpu := Ncpu,
      osreldate := OsRelDate,
      osrelease := OsRelease,
      osrev := OsRev,
      ostype := OsType,
      pagesize := PageSize} = M1,

    lists:foreach(fun(_) ->
                M1 = monarch_lib:machine()
        end, lists:seq(1,100)),
    ok.

t_memory(Config) when is_list(Config) ->
    ok.

t_loadavg(Config) when is_list(Config) ->
    M0 = monarch_lib:loadavg(),

    #{ 1 := L1,
       5 := L5,
      15 := L15 } = M0,

    true = is_float(L1),
    true = is_float(L5),
    true = is_float(L15),

    M1 = monarch_lib:loadavg(),

    #{ 1 := L1,
       5 := L5,
      15 := L15 } = M1,
    ok.

t_processes(Config) when is_list(Config) ->
    ok.

t_disks(Config) when is_list(Config) ->
    ok.

t_cpu_util(Config) when is_list(Config) ->
    ok.
