/* 
 * Copyright (C) 2014 Björn-Egil Dahlberg
 *
 * File:    libmonarch.c
 * Author:  Björn-Egil Dahlberg
 * Created: 2014-09-11
 */

#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#ifdef __linux__
#include <sys/sysinfo.h>
#endif
#include <sys/time.h>
#include <sys/sysctl.h>
#ifdef __darwin__
#include <mach/host_info.h>
#include <mach/mach_host.h>
#include <mach/task_info.h>
#include <mach/task.h>
#endif

/* getpwuid */
#include <pwd.h>
#ifdef __darwin__
#include <uuid/uuid.h>
#endif

/* getfsstat */
#ifdef __darwin__
#include <sys/param.h>
#include <sys/mount.h>
#include <sys/ucred.h>
#endif
#ifdef __linux__
#include <mntent.h>
#include <sys/statvfs.h>
#endif


#include "erl_nif.h"

#ifdef __darwin__
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
#endif

#define monarch_alloc(Sz)   (malloc((Sz)))
#define monarch_free(Ptr)   (free((Ptr)))

/* useful atoms */
static ERL_NIF_TERM am_error;
static ERL_NIF_TERM am_ok;
static ERL_NIF_TERM am_undefined;
/* getfsstat */
static ERL_NIF_TERM am_mountpoint;
static ERL_NIF_TERM am_blocks;
static ERL_NIF_TERM am_bfree;
static ERL_NIF_TERM am_bsize;
static ERL_NIF_TERM am_device;
/* vm_stat */
static ERL_NIF_TERM am_total;
static ERL_NIF_TERM am_wired;
static ERL_NIF_TERM am_active;
static ERL_NIF_TERM am_inactive;
static ERL_NIF_TERM am_free;
#ifdef __darwin__
/* sysctl */
static ERL_NIF_TERM mib_atom[MIB_ENTRIES];
static unsigned int mib_code[MIB_ENTRIES];
#endif
/* loadavg */
static ERL_NIF_TERM loadavg_key[3];
/* processes */
static ERL_NIF_TERM am_uid;
static ERL_NIF_TERM am_pid;
static ERL_NIF_TERM am_ppid;
static ERL_NIF_TERM am_pgid;
static ERL_NIF_TERM am_user;
static ERL_NIF_TERM am_name;
static ERL_NIF_TERM am_starttime;
static ERL_NIF_TERM am_load;
static ERL_NIF_TERM am_mem_res;
static ERL_NIF_TERM am_mem_map;

static ERL_NIF_TERM am_state;
static ERL_NIF_TERM process_state[8];

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

static ERL_NIF_TERM monarch_machine(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    ERL_NIF_TERM map = enif_make_new_map(env);
#ifdef __darwin__
    int ix,mib[2];
    long val;
    size_t sz;

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
#endif
    return map;
}

/* monarch_memory
 * return:
 *   #{ 'total'    => Bytes :: integer(),
 *      'wired'    => Bytes :: integer(),
 *      'active'   => Bytes :: integer(),
 *      'inactive' => Bytes :: integer(),
 *      'free'     => Bytes :: integer()  }
 */

static ERL_NIF_TERM monarch_memory(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    unsigned long total,wired,active,inactive,free;
    ERL_NIF_TERM map = enif_make_new_map(env);
#if defined(__darwin__)
    {
        mach_msg_type_number_t count = HOST_VM_INFO_COUNT;
        vm_statistics_data_t vmstat;
        int pagesize = 0;
        size_t sz;
        int mib[] = { CTL_HW, HW_PAGESIZE };

        if (host_statistics(mach_host_self(), HOST_VM_INFO, (host_info_t) &vmstat, &count) != KERN_SUCCESS) {
            return enif_make_badarg(env);
        }
        sz = sizeof(pagesize);
        sysctl(mib, 2, &pagesize, &sz, NULL, 0);

        /* natural_t is machine depedent unsigned int */

        total = vmstat.wire_count + vmstat.active_count + vmstat.inactive_count + vmstat.free_count;
        total = total * pagesize;
        wired = vmstat.wire_count * pagesize;
        active = vmstat.active_count * pagesize;
        inactive = vmstat.inactive_count * pagesize;
        free = vmstat.free_count * pagesize;
    }
#elif defined(__linux__)
    {
        struct sysinfo meminfo;

        // struct sysinfo {
        //     long uptime;             /* Seconds since boot */
        //     unsigned long loads[3];  /* 1, 5, and 15 minute load averages */
        //     unsigned long totalram;  /* Total usable main memory size */
        //     unsigned long freeram;   /* Available memory size */
        //     unsigned long sharedram; /* Amount of shared memory */
        //     unsigned long bufferram; /* Memory used by buffers */
        //     unsigned long totalswap; /* Total swap space size */
        //     unsigned long freeswap;  /* swap space still available */
        //     unsigned short procs;    /* Number of current processes */
        //     char _f[22];             /* Pads structure to 64 bytes */
        // };

        sysinfo(&meminfo);
        total = meminfo.totalram;
        free  = meminfo.freeram;
        active = 0;
        inactive = 0;
        wired = 0;
    }
#endif
    enif_make_map_put(env, map, am_total, enif_make_ulong(env,total), &map);
    enif_make_map_put(env, map, am_wired, enif_make_ulong(env,wired), &map);
    enif_make_map_put(env, map, am_active, enif_make_ulong(env,active), &map);
    enif_make_map_put(env, map, am_inactive, enif_make_ulong(env,inactive), &map);
    enif_make_map_put(env, map, am_free, enif_make_ulong(env,free), &map);
    return map;
}

/* monarch_loadavg
 * return:
 *   #{  1  => Load :: float(),
 *       5  => Load :: float(),
 *      15  => Load :: float()  }
 */

static ERL_NIF_TERM monarch_loadavg(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    ERL_NIF_TERM map = enif_make_new_map(env);
    int i, n;
#ifdef __darwin__
#define monarch_load_array(Ix) ((double) loadinfo.ldavg[(Ix)] / loadinfo.fscale)
    struct loadavg loadinfo;
    size_t sz;
    int mib[] = {CTL_VM, VM_LOADAVG};
    sz = sizeof(loadinfo);

    if (sysctl(mib, 2, &loadinfo, &sz, NULL, 0) < 0)
        return enif_make_badarg(env);

    n = sizeof(loadinfo.ldavg) / sizeof(fixpt_t);
#elif defined (__linux__)
#define monarch_load_array(Ix) ((double) (meminfo.loads[(Ix)] / (float)(1 <<  SI_LOAD_SHIFT)))
    struct sysinfo meminfo;
    sysinfo(&meminfo);
    n = sizeof(meminfo.loads) / sizeof(meminfo.loads[0]);
#endif
    for (i = 0; i < n; i++) {
        enif_make_map_put(env, map, loadavg_key[i],
                enif_make_double(env, monarch_load_array(i)), &map);
    }
    return map;
}

/* monarch_disks
 * return:
 * [#{bfree => Free :: integer(),
 *    blocks => Blocks :: integer(),
 *    mountpoint => Path :: binary() % ex. <<"/home">>
 *   }].
 */


static ERL_NIF_TERM monarch_disks(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    ERL_NIF_TERM res = enif_make_list(env, 0); /* NIL */
    ERL_NIF_TERM map;
#if defined (__darwin__)
    int i,fsn;
    size_t sz;
    struct statfs buf[125];
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
#elif defined (__linux__)
    struct mntent *ent;
    struct statvfs vfs;
    FILE *mounts = setmntent("/etc/mtab", "r");

    // struct mntent {
    //     char *mnt_fsname;   /* name of mounted filesystem */
    //     char *mnt_dir;      /* filesystem path prefix */
    //     char *mnt_type;     /* mount type (see mntent.h) */
    //     char *mnt_opts;     /* mount options (see mntent.h) */
    //     int   mnt_freq;     /* dump frequency in days */
    //     int   mnt_passno;   /* pass number on parallel fsck */
    // };
    size_t sz;
    ErlNifBinary obin;

    // struct statvfs {
    //     unsigned long  f_bsize;    /* filesystem block size */
    //     unsigned long  f_frsize;   /* fragment size */
    //     fsblkcnt_t     f_blocks;   /* size of fs in f_frsize units */
    //     fsblkcnt_t     f_bfree;    /* # free blocks */
    //     fsblkcnt_t     f_bavail;   /* # free blocks for unprivileged users */
    //     fsfilcnt_t     f_files;    /* # inodes */
    //     fsfilcnt_t     f_ffree;    /* # free inodes */
    //     fsfilcnt_t     f_favail;   /* # free inodes for unprivileged users */
    //     unsigned long  f_fsid;     /* filesystem ID */
    //     unsigned long  f_flag;     /* mount flags */
    //     unsigned long  f_namemax;  /* maximum filename length */
    // };

    while ((ent = getmntent(mounts)) != NULL) {
        statvfs(ent->mnt_dir, &vfs);

	map = enif_make_new_map(env);

	sz = strlen(ent->mnt_dir);
	if (!enif_alloc_binary(sz,&obin)) {
	    return enif_make_badarg(env);
	}
	memcpy(obin.data,ent->mnt_dir,sz);
	enif_make_map_put(env, map, am_mountpoint,
		enif_make_binary(env, &obin), &map);

	sz = strlen(ent->mnt_fsname);
	if (!enif_alloc_binary(sz,&obin)) {
	    return enif_make_badarg(env);
	}
	memcpy(obin.data,ent->mnt_fsname,sz);
	enif_make_map_put(env, map, am_device,
		enif_make_binary(env, &obin), &map);

	enif_make_map_put(env, map, am_blocks,
		enif_make_ulong(env,vfs.f_blocks), &map);
	enif_make_map_put(env, map, am_bfree,
		enif_make_ulong(env,vfs.f_bfree), &map);
	enif_make_map_put(env, map, am_bsize,
		enif_make_ulong(env,vfs.f_bsize), &map);
	res = enif_make_list_cell(env, map, res);
    }
    endmntent(mounts);

#endif
    return res;
}

/* monarch_processes
 *
 *  struct	eproc {
 *      struct	proc *e_paddr;		// address of proc
 *      struct	session *e_sess;	// session pointer
 *      struct	pcred e_pcred;		// process credentials
 *      struct	ucred e_ucred;		// current credentials
 *      struct	vmspace e_vm;		// address space
 *      pid_t	e_ppid;			// parent process id
 *      pid_t	e_pgid;			// process group id
 *      short	e_jobc;			// job control counter
 *      dev_t	e_tdev;			// controlling tty dev
 *      pid_t	e_tpgid;		// tty process group id
 *      struct	session *e_tsess;	// tty session pointer
 *  #define	WMESGLEN	7
 *      char	e_wmesg[WMESGLEN+1];	// wchan message
 *      segsz_t e_xsize;		// text size
 *      short	e_xrssize;		// text rss
 *      short	e_xccount;		// text references
 *      short	e_xswrss;
 *      long	e_flag;
 *  #define	EPROC_CTTY	0x01	// controlling tty vnode active
 *  #define	EPROC_SLEADER	0x02	// session leader
 *      char	e_login[MAXLOGNAME];	// setlogin() name
 *      long	e_spare[4];
 *  } kp_eproc;
 *
 *
 *   int procFlag = (int)(kp->kp_proc.p_flag);
 *   char procStat = (char)(kp->kp_proc.p_stat);
 *   pid_t procPid = (pid_t)(kp->kp_proc.p_pid);
 *   u_char procPriority = (u_char)(kp->kp_proc.p_priority);
 *   char procNice = (kp->kp_proc.p_nice);
 *   NSString *procName = nameForProcessWithPID( kp->kp_proc.p_pid );
 *   pid_t procParentPid = (pid_t)(kp->kp_eproc.e_ppid);
 *   time_t procStartTime = (kp->kp_proc.p_starttime.tv_sec);
 *   uid_t userId = (kp->kp_eproc.e_ucred.cr_uid);
 *   NSDate *theDate = [NSDate dateWithTimeIntervalSince1970:procStartTime];
 *   struct passwd *pw;
 */

#ifdef __darwin__
static ERL_NIF_TERM monarch_get_process_name(ErlNifEnv *env, pid_t pid) {
    int mib[4], maxarg = 0, numArgs = 0;
    size_t sz = 0;
    char *args = NULL, *name = NULL, *string = NULL;
    ERL_NIF_TERM res = am_undefined;
    ErlNifBinary obin;

    mib[0] = CTL_KERN;
    mib[1] = KERN_ARGMAX;

    sz = sizeof(maxarg);
    if (sysctl(mib, 2, &maxarg, &sz, NULL, 0) == -1) {
	return am_undefined;
    }

    if ((args = (char *)monarch_alloc(maxarg)) == NULL) {
	return am_undefined;
    }

    mib[0] = CTL_KERN;
    mib[1] = KERN_PROCARGS2;
    mib[2] = pid;

    sz = (size_t) maxarg;

    if (sysctl(mib, 3, args, &sz, NULL, 0) == -1) {
	monarch_free(args);
	return am_undefined;
    }

    memcpy(&numArgs, args, sizeof(numArgs));
    string = args + sizeof(numArgs);

    if ((name = strrchr(string, '/')) != NULL) {
	name++;
    } else {
	name = string;
    }
    sz = strlen(name);
    if (!enif_alloc_binary(sz,&obin)) {
	monarch_free(args);
	return am_undefined;
    }
    memcpy(obin.data,name,sz);
    res = enif_make_binary(env,&obin);
    monarch_free(args);
    return res;
}
#endif

#ifdef __darwin__
static ERL_NIF_TERM monarch_process(ErlNifEnv *env, struct kinfo_proc *proc) {
    uid_t uid = proc->kp_eproc.e_ucred.cr_uid;
    char *username = NULL;
    struct passwd *user = getpwuid(uid);
    ERL_NIF_TERM map;
    ErlNifBinary obin;
    size_t sz;

    username = user ? user->pw_name : "undefined";
    map = enif_make_new_map(env);
    sz  = strlen(username);

    /*  int procFlag = (int)(kp->kp_proc.p_flag);
     *  char procStat = (char)(kp->kp_proc.p_stat);
     *  u_char procPriority = (u_char)(kp->kp_proc.p_priority);
     *  char procNice = (kp->kp_proc.p_nice);
     */
 
    if (!enif_alloc_binary(sz,&obin)) {
        return enif_make_badarg(env);
    }

    memcpy(obin.data,username,sz);
    /* user name */
    enif_make_map_put(env, map, am_user,
            enif_make_binary(env,&obin), &map);
    /* user id */
    enif_make_map_put(env, map, am_uid,
            enif_make_ulong(env,(unsigned long)uid), &map);
    /* process name, if any otherwise 'undefined' */
    enif_make_map_put(env, map, am_name,
            monarch_get_process_name(env, proc->kp_proc.p_pid), &map);
    /* process id */
    enif_make_map_put(env, map, am_pid,
            enif_make_ulong(env,(unsigned long)proc->kp_proc.p_pid), &map);
    /* parent process id */
    enif_make_map_put(env, map, am_ppid,
            enif_make_ulong(env,(unsigned long)proc->kp_eproc.e_ppid), &map);
    /* process group id */
    enif_make_map_put(env, map, am_pgid,
            enif_make_ulong(env,(unsigned long)proc->kp_eproc.e_pgid), &map);
    /* process start time in seconds */
    enif_make_map_put(env, map, am_starttime,
            enif_make_long(env,(long)proc->kp_proc.p_starttime.tv_sec), &map);
    /* process state */
    enif_make_map_put(env, map, am_state,
            enif_make_int(env,(int)proc->kp_proc.p_stat), &map);
    /* process memory mapped */
    /* darn mac
       sz = (proc_list[i].kp_eproc.e_vm.vm_tsize +
       proc_list[i].kp_eproc.e_vm.vm_dsize +
       proc_list[i].kp_eproc.e_vm.vm_ssize);// * getpagesize();
       enif_make_map_put(env, map, am_mem_map,
       enif_make_ulong(env,(unsigned long)sz), &map);
       */
    /* process memory resident */
    /* darn mac
       sz = proc_list[i].kp_eproc.e_vm.vm_rssize; // * getpagesize();
       enif_make_map_put(env, map, am_mem_res,
       enif_make_ulong(env,(unsigned long)sz), &map);
       */
    /* process memory load */
    enif_make_map_put(env, map, am_load,
            enif_make_double(env,((double)proc->kp_proc.p_pctcpu / FSCALE)), &map);

    return map;
}
#endif

/* monarch_processes
 * [#{load => Load :: float(), ex 0.0,
 *    name => Name :: binary() | 'undefined',
 *    pgid => Pgid :: integer(),
 *    pid => Pid :: integer(),
 *    ppid => Ppid :: integer(),
 *    starttime => Secs :: integer(),  1428494470,
 *    state => 2,
 *    uid => 0,
 *    user => <<"root">>}].
 */

static ERL_NIF_TERM monarch_processes(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    ERL_NIF_TERM res = enif_make_list(env, 0); /* NIL */
#ifdef __darwin__
    ERL_NIF_TERM map = am_undefined;
    struct kinfo_proc *proc_list = NULL;
    size_t sz = 0;
    int i, n, mib[] = { CTL_KERN, KERN_PROC, KERN_PROC_ALL };

    /* XXX: Race condition? */
    sysctl(mib, 3, NULL, &sz, NULL, 0);
    proc_list = monarch_alloc(sz);
    sysctl(mib, 3, proc_list, &sz, NULL, 0);

    n = sz / sizeof(struct kinfo_proc);
    for (i = 0; i < n; i++) {
        map = monarch_process(env, &(proc_list[i]));
        res = enif_make_list_cell(env, map, res);
    }
    monarch_free(proc_list);
#endif
    return res;
}

static ERL_NIF_TERM monarch_cpu_util(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    return am_ok;
}
/* boilerplate */
#ifdef __darwin__
#define init_mib_code(Class,Code,Type,Ix,Name)     \
    do {                                           \
	mib_atom[Ix] = enif_make_atom(env,Name);   \
	mib_code[Ix] = MIB_ENC(Class,Code,Type);   \
    } while(0)
#endif

static void init(ErlNifEnv *env) {
    am_undefined = enif_make_atom(env,"undefined");
    am_error     = enif_make_atom(env,"error");
    am_ok        = enif_make_atom(env,"ok");

    /* vm_stat */
    am_total    = enif_make_atom(env,"total");
    am_wired    = enif_make_atom(env,"wired");
    am_active   = enif_make_atom(env,"active");
    am_inactive = enif_make_atom(env,"inactive");
    am_free     = enif_make_atom(env,"free");

    /* vm_stat and statvfs*/
    am_mountpoint = enif_make_atom(env,"mountpoint");
    am_blocks     = enif_make_atom(env,"blocks");
    am_bfree      = enif_make_atom(env,"bfree");
    am_bsize      = enif_make_atom(env,"bsize");
    am_device     = enif_make_atom(env,"device");

    /* loadavg */
    loadavg_key[0] = enif_make_int(env,1);
    loadavg_key[1] = enif_make_int(env,5);
    loadavg_key[2] = enif_make_int(env,15);

    /* processes */
    am_uid  = enif_make_atom(env,"uid");
    am_pid  = enif_make_atom(env,"pid");
    am_ppid = enif_make_atom(env,"ppid");
    am_pgid = enif_make_atom(env,"pgid");
    am_user = enif_make_atom(env,"user");
    am_name = enif_make_atom(env,"name");
    am_load = enif_make_atom(env,"load");
    am_mem_res = enif_make_atom(env,"mem_res");
    am_mem_map = enif_make_atom(env,"mem_map");
    am_starttime = enif_make_atom(env,"starttime");

    /* process states */
    //char p_stat[]="?iRSTZ";
    am_state = enif_make_atom(env,"state");

    process_state[0] = enif_make_atom(env,"zombie");
    process_state[1] = enif_make_atom(env,"running");
    process_state[2] = enif_make_atom(env,"stuck");
    process_state[3] = enif_make_atom(env,"sleeping");
    process_state[4] = enif_make_atom(env,"idle");
    process_state[5] = enif_make_atom(env,"stopped");
    process_state[6] = enif_make_atom(env,"halted");
    process_state[7] = enif_make_atom(env,"unknown");

    /* sysctl */
#ifdef __darwin__
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
#endif
}
#undef init_mib_code

static ErlNifFunc nif_functions[] =
{
    {"loadavg", 0, monarch_loadavg},
    {"memory",  0, monarch_memory},
    {"processes",  0, monarch_processes},
    {"disks",  0, monarch_disks},
    {"cpu_util",  0, monarch_cpu_util},
    {"machine", 0, monarch_machine}
};

static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info) {
    init(env);
    *priv_data = NULL;
    return 0;
}

ERL_NIF_INIT(monarch_lib, nif_functions, load, NULL, NULL, NULL)
