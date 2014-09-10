/* 
 * Copyright (C) 2014 Björn-Egil Dahlberg
 *
 * File:    libmonos.c
 * Author:  Björn-Egil Dahlberg
 * Created: 2014-09-11
 */

#include <stdio.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/sysctl.h>
#include <mach/host_info.h>
#include <mach/mach_host.h>
#include <mach/task_info.h>
#include <mach/task.h>

/* getpwuid */
#include <pwd.h>
#include <uuid/uuid.h>

/* getfsstat */
#include <sys/param.h>
#include <sys/ucred.h>
#include <sys/mount.h>

#include "erl_nif.h"

#define MIB_ENTRIES (12)

#define MIB_STRING          (0)
#define MIB_INTEGER         (1)
#define MIB_TIMEVAL         (2)
#define MIB_TYPE_SZ         (2)  /* #bits required for types */
#define MIB_CLASS_SZ        (15) /* #bits required for MIB TIER 1 */
/* mask val before oring .. */
#define MIB_ENC(C,M,TYPE)   (((((M) << MIB_CLASS_SZ) | (C)) << MIB_TYPE_SZ) | (TYPE))
#define MIB_DEC_CLASS(C)    (((C) >> (MIB_TYPE_SZ)) & ((1 << (MIB_CLASS_SZ)) - 1))
#define MIB_DEC_CODE(C)     ((C) >> (MIB_CLASS_SZ + MIB_TYPE_SZ))
#define MIB_DEC_TYPE(C)     ((C) & ((1 << MIB_TYPE_SZ) - 1))


/* useful atoms */
static ERL_NIF_TERM am_error;
static ERL_NIF_TERM am_ok;
/* getfsstat */
static ERL_NIF_TERM am_mountpoint;
static ERL_NIF_TERM am_blocks;
static ERL_NIF_TERM am_bfree;
/* vm_stat */
static ERL_NIF_TERM am_total;
static ERL_NIF_TERM am_wired;
static ERL_NIF_TERM am_active;
static ERL_NIF_TERM am_inactive;
static ERL_NIF_TERM am_free;
/* sysctl */
static ERL_NIF_TERM mib_atom[MIB_ENTRIES];
static unsigned int mib_code[MIB_ENTRIES];

/* machine
 * return:
 *   #{ 'machine'  => binary(),
 *      'model'    => binary(),
 *      'ncpu'     => integer(),
 *      'memsize'  => integer(),
 *      'pagesize' => integer() }.
 *
 * The idea is to only return *static* data known at boot time.
 *
 * From CTL_HW:
 *
 *   HW_MACHINE                 string        no
 *   HW_MODEL                   string        no
 *   HW_NCPU                    integer       no (deprecated)
 *   HW_BYTEORDER               integer       no
 *   HW_MEMSIZE                 integer       no
 *   HW_PHYSMEM                 integer       no (deprecated)
 *   HW_PAGESIZE                integer       no
 *
 *   hw.physicalcpu
 *   hw.physicalcpu_max
 *   hw.logicalcpu
 *   hw.logicalcpu_max
 *
 * From CTL_KERN
 *
 *   KERN_BOOTTIME              struct timeval         no
 *   KERN_HOSTNAME              string                 yes
 *   KERN_OSRELDATE             integer                no
 *   KERN_OSRELEASE             string                 no
 *   KERN_OSREV                 integer                no
 *   KERN_OSTYPE                string                 no
 *   KERN_VERSION               string                 no
 *
 *   KERN_ARGMAX                integer                no
 *   KERN_BOOTFILE              string                 yes
 *   KERN_CLOCKRATE             struct clockinfo       no
 *   KERN_FILE                  struct file            no
 *   KERN_HOSTID                integer                yes
 *   KERN_JOB_CONTROL           integer                no
 *   KERN_MAXFILES              integer                yes
 *   KERN_MAXFILESPERPROC       integer                yes
 *   KERN_MAXPROC               integer                no
 *   KERN_MAXPROCPERUID         integer                yes
 *   KERN_MAXVNODES             integer                yes
 *   KERN_NGROUPS               integer                no
 *   KERN_NISDOMAINNAME         string                 yes
 *   KERN_POSIX1                integer                no
 *   KERN_PROC                  struct kinfo_proc      no
 *   KERN_PROF                  node                   not applicable
 *   KERN_QUANTUM               integer                yes
 *   KERN_SAVED_IDS             integer                no
 *   KERN_SECURELVL             integer                raise only
 *   KERN_UPDATEINTERVAL        integer                no
 *   KERN_VNODE                 struct vnode           no
 *
 */

static ERL_NIF_TERM monos_machine(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    int ix,mib[2];
    long val;
    size_t sz;
    ERL_NIF_TERM map = enif_make_new_map(env);

    for (ix=0; ix < MIB_ENTRIES; ix++) {
	mib[0] = MIB_DEC_CLASS(mib_code[ix]);
	mib[1] = MIB_DEC_CODE(mib_code[ix]);
	switch MIB_DEC_TYPE(mib_code[ix]) {
	    case MIB_INTEGER:
		sz = sizeof(val);
		val = 0;
		sysctl(mib, 2, &val, &sz, NULL, 0);
		enif_make_map_put(env, map, mib_atom[ix], enif_make_long(env,val), &map);
	    break;
	    case MIB_STRING: {
		/* string .. null terminated ..*/
		ErlNifBinary obin;
		char tmp[256];
		sysctl(mib, 2, NULL, &sz, NULL, 0);
		if (sz < 2 || sz > 255) {
		    return enif_make_badarg(env);
		}
		sysctl(mib, 2, tmp, &sz, NULL, 0);
		sz--; /* don't include \0 */
		if (!enif_alloc_binary(sz,&obin)) {
		    return enif_make_badarg(env);
		}
		memcpy(obin.data,tmp,sz);
		enif_make_map_put(env, map, mib_atom[ix], enif_make_binary(env,&obin), &map);
	    }
	    break;
	    case MIB_TIMEVAL: {
	        struct timeval tv;
		sz = sizeof(tv);
		sysctl(mib, 2, &tv, &sz, NULL, 0);
		enif_make_map_put(env, map, mib_atom[ix], enif_make_long(env,tv.tv_sec), &map);
	    }
	    break;
	}
    }
    return map;
}

/* monos_memory
 * return:
 *   #{ 'total'    => Bytes :: integer(),
 *      'wired'    => Bytes :: integer(),
 *      'active'   => Bytes :: integer(),
 *      'inactive' => Bytes :: integer(),
 *      'free'     => Bytes :: integer()  }
 */

static ERL_NIF_TERM monos_memory(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    ERL_NIF_TERM map;
    mach_msg_type_number_t count = HOST_VM_INFO_COUNT;
    vm_statistics_data_t vmstat;
    int pagesize = 0;
    size_t sz;
    int mib[] = { CTL_HW, HW_PAGESIZE };
    unsigned long total,wired,active,inactive,free;

    if (host_statistics(mach_host_self(), HOST_VM_INFO, (host_info_t) &vmstat, &count) != KERN_SUCCESS) {
	return enif_make_badarg(env);
    }
    sz = sizeof(pagesize);
    sysctl(mib, 2, &pagesize, &sz, NULL, 0);

    map = enif_make_new_map(env);

    /* natural_t is machine depedent unsigned int */

    total = vmstat.wire_count + vmstat.active_count + vmstat.inactive_count + vmstat.free_count;
    wired = vmstat.wire_count;
    active = vmstat.active_count;
    inactive = vmstat.inactive_count;
    free = vmstat.free_count;

    enif_make_map_put(env, map, am_total, enif_make_ulong(env,total*pagesize), &map);
    enif_make_map_put(env, map, am_wired, enif_make_ulong(env,wired*pagesize), &map);
    enif_make_map_put(env, map, am_active, enif_make_ulong(env,active*pagesize), &map);
    enif_make_map_put(env, map, am_inactive, enif_make_ulong(env,inactive*pagesize), &map);
    enif_make_map_put(env, map, am_free, enif_make_ulong(env,free*pagesize), &map);

    return map;
}

static ERL_NIF_TERM monos_loadavg(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    ERL_NIF_TERM map;
    struct loadavg loadinfo;
    size_t sz;
    int n, i, mib[] = {CTL_VM, VM_LOADAVG};
    sz = sizeof(loadinfo);

    if (sysctl(mib, 2, &loadinfo, &sz, NULL, 0) < 0)
	return enif_make_badarg(env);

    map = enif_make_new_map(env);
    n = sizeof(loadinfo.ldavg) / sizeof(fixpt_t);
    for (i = 0; i < n; i++) {
	enif_make_map_put(env, map, enif_make_int(env,i),
	    enif_make_double(env,(double) loadinfo.ldavg[i] / loadinfo.fscale), &map);
    }

    return map;
}
static ERL_NIF_TERM monos_disks(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {

    int i,fsn;
    size_t sz;
    struct statfs buf[125];
    ERL_NIF_TERM res = enif_make_list(env, 0); /* NIL */
    ERL_NIF_TERM map;
    ErlNifBinary obin;

    fsn = getfsstat(NULL, 0, MNT_NOWAIT);
    getfsstat(buf, fsn * sizeof(struct statfs), MNT_NOWAIT);
    for (i = 0; i < fsn; ++i) {
	map = enif_make_new_map(env);
	sz = strlen(buf[i].f_mntonname);
	if (!enif_alloc_binary(sz,&obin)) {
	    return enif_make_badarg(env);
	}
	memcpy(obin.data,buf[i].f_mntonname,sz);
	enif_make_map_put(env, map, am_mountpoint,
		enif_make_binary(env,&obin), &map);
	enif_make_map_put(env, map, am_blocks,
		enif_make_ulong(env,buf[i].f_blocks), &map);
	enif_make_map_put(env, map, am_bfree,
		enif_make_ulong(env,buf[i].f_bfree), &map);
	res = enif_make_list_cell(env, map, res);
    }
    return res;
}

static ERL_NIF_TERM monos_processes(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    /*
    struct kinfo_proc *proc_list = NULL;
    size_t sz = 0;
    int i, n, mib[] = { CTL_KERN, KERN_PROC, KERN_PROC_ALL };
    struct passwd *user = NULL;

    sysctl(mib, 3, NULL, &sz, NULL, 0);

    proc_list = malloc(sz);
    sysctl(mib, 3, proc_list, &sz, NULL, 0);

    n = sz / sizeof(struct kinfo_proc);

    for (i = 0; i < n; i++) {
        uid_t uid = proc_list[i].kp_eproc.e_ucred.cr_uid;
        user = getpwuid(uid);
        char *username = user ? user->pw_name : "user name not found";

        fprintf(stderr,"pid=%d, uid=%d, username=%s\r\n",
                proc_list[i].kp_proc.p_pid,
                uid,
                username);
    }

    free(proc_list);
    */
    return am_ok;
}

/* boilerplate */
#define init_mib_code(Class,Code,Type,Ix,Name)     \
    do {                                           \
	mib_atom[Ix] = enif_make_atom(env,Name);   \
	mib_code[Ix] = MIB_ENC(Class,Code,Type);   \
    } while(0)

static void init(ErlNifEnv *env) {
    am_error = enif_make_atom(env,"error");
    am_ok    = enif_make_atom(env,"ok");

    /* vm_stat */
    am_total    = enif_make_atom(env,"total");
    am_wired    = enif_make_atom(env,"wired");
    am_active   = enif_make_atom(env,"active");
    am_inactive = enif_make_atom(env,"inactive");
    am_free     = enif_make_atom(env,"free");

    /* vm_stat */
    am_mountpoint = enif_make_atom(env,"mountpoint");
    am_blocks     = enif_make_atom(env,"blocks");
    am_bfree      = enif_make_atom(env,"bfree");

    /* sysctl */
    init_mib_code(CTL_HW, HW_MACHINE,  MIB_STRING,  0, "machine");
    init_mib_code(CTL_HW, HW_MODEL,    MIB_STRING,  1, "model");
    init_mib_code(CTL_HW, HW_NCPU,     MIB_INTEGER, 2, "ncpu");
    init_mib_code(CTL_HW, HW_MEMSIZE,  MIB_INTEGER, 3, "memsize");
    init_mib_code(CTL_HW, HW_PAGESIZE, MIB_INTEGER, 4, "pagesize");

    init_mib_code(CTL_KERN, KERN_HOSTNAME,  MIB_STRING,    5, "hostname");
    init_mib_code(CTL_KERN, KERN_OSRELDATE, MIB_INTEGER,   6, "osreldate");
    init_mib_code(CTL_KERN, KERN_OSREV,     MIB_INTEGER,   7, "osrev");
    init_mib_code(CTL_KERN, KERN_OSRELEASE, MIB_STRING,    8, "osrelease");
    init_mib_code(CTL_KERN, KERN_OSTYPE,    MIB_STRING,    9, "ostype");
    init_mib_code(CTL_KERN, KERN_VERSION,   MIB_STRING,   10, "kernel_version");
    init_mib_code(CTL_KERN, KERN_BOOTTIME,  MIB_TIMEVAL,  11, "boottime");
}
#undef init_mib_code

static ErlNifFunc nif_functions[] =
{
    {"loadavg", 0, monos_loadavg},
    {"memory",  0, monos_memory},
    {"processes",  0, monos_processes},
    {"disks",  0, monos_disks},
    {"machine", 0, monos_machine}
};

static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info) {
    init(env);
    *priv_data = NULL;
    return 0;
}

ERL_NIF_INIT(monos_lib, nif_functions, load, NULL, NULL, NULL)
