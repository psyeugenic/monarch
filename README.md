monarch
=======

An `os_mon` replacement.

### Static Data from Boot time ###

* `monarch:machine()`

```erlang
    #{'machine'  => :: binary(),     % ex. <<"x86_64">>
      'memsize'  => :: integer(),    % ex. 17179869184
      'model'    => :: binary(),     % ex. <<"iMac12,2">>
      'ncpu'     => :: integer(),    % ex. 4
      'pagesize' => :: integer()}    % ex. 4096
```

### Fetching Memory Data ###

* `monarch:memory()`

```erlang
    #{'active'   => integer(),    % ex. 1436594176,
      'free'     => integer(),    % ex. 14308564992,
      'inactive' => integer(),    % ex. 227401728,
      'total'    => integer(),    % ex. 17177673728,
      'wired'    => integer()}    % ex. 1205112832
```

### Fetching CPU Data ###

* `monarch:cpu_util()`

```erlang
    #{'soft_irq'  => float(),    % ex.  0.018104462
      'hard_irq'  => float(),    % ex.  0.0
      'kernel'    => float(),    % ex.  0.226
      'nice_user' => float(),    % ex.  0.00901
      'steal'     => float(),    % ex.  0.0
      'user'      => float(),    % ex.  0.869
      'steal'     => float(),    % ex.  0.0
      'wait'      => float(),    % ex. 98.08
      'idle'      => float()}    % ex.  0.78
```



### Fetching Disk Data ###

* `monarch:disks()`

```erlang
    [#{'bfree'      => integer(),  % ex. 45058684
       'blocks'     => integer(),  % ex. 243980744
       'mountpoint' => binary()}]  % ex. <<"/">>
```

### Fetching OS Process Data ###

* `monarch:processes()`

```erlang
    [#{'name'      => binary(),    % ex. <<"beam.smp">>
       'pid'       => integer(),   % ex. 54426
       'pgid'      => integer(),   % ex. 54426
       'ppid'      => integer(),   % ex. 18665
       'starttime' => integer(),   % ex. 1427997920
       'state'     => integer(),   % ex. 2
       'uid'       => integer(),   % ex. 501
       'user'      => binary()}]   % ex. <<"egil">>
```

How old `OS_Mon` did it
-----------------------

`os_mon` feature areas:

 * `cpu_sup` -  CPU load and utilization supervision (Unix)
 * `memsup`  -  Memory supervision (Unix, Windows, VxWorks)
 * `disksup` -  Disk supervision(Unix, Windows)
 * `os_sup`  -  Interface to OS system messages (Solaris, Windows)

### `cpu_sup` ###

Previous `OS_Mon` Interface:

 * `nprocs() -> integer() | {error, Reason :: term()}`

 * `avg1()  -> SystemLoad :: float() | {error, Reason :: term()}`
 * `avg5()  -> SystemLoad :: float() | {error, Reason :: term()}`
 * `avg15() -> SystemLoad :: float() | {error, Reason :: term()}`

 * `util() -> CpuUtil | {error, Reason}`
 * `util(Opts) -> UtilSpec | {error, Reason}`

### `memsup` ###

Previous `OS_Mon` Interface:

 * `get_memory_data() -> {Total :: integer(), Allocated :: integer(), Worst :: {pid(), integer()}`
 * `get_system_memory_data() -> [{ 'total_memory' | 'free_memory' .. , integer()}`

 * `get_wordsize() -> 32 | 64 | unsupported_os`

 * `get_check_interval() -> MS :: integer()`
 * `set_check_interval(Minutes :: non_neg_integer()) -> ok`

 * `get_procmem_high_watermark() -> integer()`
 * `set_procmem_high_watermark(Float :: float()) -> ok`

 * `get_helper_timeout() -> Seconds :: integer()`
 * `set_helper_timeout(Seconds :: integer()) -> ok`

### `disksup` ###

Previous `OS_Mon` Interface:

 * `get_disk_data() -> [{Id :: string(), KByte :: integer(), Capacity :: integer()}]`
 * `get_check_interval() -> MS :: non_neg_integer()`
 * `get_almost_full_threshold() -> Percent :: 0..100`
 * `set_almost_full_threshold(Float :: float()) -> ok`


Netflix Vector
--------------


#### CPU

 * Load Average
 * Runnable
 * CPU Utilization
 * Per-CPU Utilization
 * Context Switches

#### Memory

 * Memory Utilization
 * Page Faults

#### Disk

 * Disk IOPS
 * Disk Throughput
 * Disk Utilization
 * Disk Latency

#### Network

 * Network Drops
 * TCP Retransmits
 * TCP Connections
 * Network Throughput
 * Network Packets
