/*
 * Copyright (c) 1998-2001 National Center for Supercomputing Applications
 *                         All rights reserved.
 *
 * Programmer:  Robb Matzke <matzke@llnl.gov>
 *              Thursday, November 19, 1998
 *
 * Purpose:	Provides support functions for most of the hdf5 tests cases.
 *		
 */

#undef NDEBUG			/*override -DNDEBUG			*/
#include "h5test.h"
#include <sys/types.h>
#include <sys/stat.h>

#ifdef WIN32
#include <process.h>
#include <direct.h>
#include <winsock.h>
#endif  /* WIN32 */

/*
 * Define these environment variables or constants to influence functions in
 * this test support library.  The environment variable is used in preference
 * to the cpp constant.  If neither is defined then use some default value.
 *
 * HDF5_DRIVER:		This string describes what low level file driver to
 *			use for HDF5 file access.  The first word in the
 *			value is the name of the driver and subsequent data
 *			is interpreted according to the driver.  See
 *			h5_fileaccess() for details.
 *
 * HDF5_PREFIX:		A string to add to the beginning of all serial test
 *			file names.  This can be used to run tests in a
 *			different file system (e.g., "/tmp" or "/tmp/myname").
 *			The prefix will be separated from the base file name
 *			by a slash. See h5_fixname() for details.
 *
 * HDF5_PARAPREFIX:	A string to add to the beginning of all parallel test
 *			file names.  This can be used to tell MPIO what driver
 *			to use (e.g., "gfs:", "ufs:", or "nfs:") or to use a
 *			different file system (e.g., "/tmp" or "/tmp/myname").
 *			The prefix will be separated from the base file name
 *			by a slash. See h5_fixname() for details.
 *
 */
/*
 * In a parallel machine, the filesystem suitable for compiling is
 * unlikely a parallel file system that is suitable for parallel I/O.
 * There is no standard pathname for the parallel file system.  /tmp
 * is about the best guess.
 */
#ifndef HDF5_PARAPREFIX
#ifdef __PUMAGON__
/* For the PFS of TFLOPS */
#define HDF5_PARAPREFIX "pfs:/pfs_grande/multi/tmp_1"
#else
#define HDF5_PARAPREFIX "/tmp"
#endif
#endif
char	*paraprefix = NULL;	/* for command line option para-prefix */

/*
 * These are the letters that are appended to the file name when generating
 * names for the split and multi drivers. They are:
 *
 * 	m: All meta data when using the split driver.
 *	s: The userblock, superblock, and driver info block
 *	b: B-tree nodes
 *	r: Dataset raw data
 *	g: Global heap
 *	l: local heap (object names)
 *	o: object headers
 */
static const char *multi_letters = "msbrglo";


/*-------------------------------------------------------------------------
 * Function:	h5_errors
 *
 * Purpose:	Displays the error stack after printing "*FAILED*".
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Robb Matzke
 *		Wednesday, March  4, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
h5_errors(void UNUSED *client_data)
{
    H5_FAILED();
    H5Eprint (stdout);
    return 0;
}


/*-------------------------------------------------------------------------
 * Function:	h5_cleanup
 *
 * Purpose:	Cleanup temporary test files.
 *		base_name contains the list of test file names.
 *		The file access property list is also closed.
 *
 * Return:	Non-zero if cleanup actions were performed; zero otherwise.
 *
 * Programmer:	Albert Cheng
 *              May 28, 1998
 *
 * Modifications:
 *		Albert Cheng, 2000-09-09
 *		Added the explicite base_name argument to replace the
 *		global variable FILENAME.
 *
 *-------------------------------------------------------------------------
 */
int
h5_cleanup(const char *base_name[], hid_t fapl)
{
    char	filename[1024];
    char	temp[2048];
    int		i, j;
    int		retval=0;
    hid_t	driver;

    if (!HDgetenv("HDF5_NOCLEANUP")) {
	for (i = 0; base_name[i]; i++) {
	    if (h5_fixname(base_name[i], fapl, filename, sizeof(filename)) == NULL)
		continue;

	    driver = H5Pget_driver(fapl);

	    if (driver == H5FD_FAMILY) {
		for (j = 0; /*void*/; j++) {
		    HDsnprintf(temp, sizeof temp, filename, j);

		    if (HDaccess(temp, F_OK) < 0)
                        break;

		    HDremove(temp);
		}
	    } else if (driver == H5FD_CORE) {
		/*void*/
	    } else if (driver == H5FD_MULTI) {
		H5FD_mem_t mt;
		assert(strlen(multi_letters)==H5FD_MEM_NTYPES);

		for (mt = H5FD_MEM_DEFAULT; mt < H5FD_MEM_NTYPES; mt=mt+1) {
		    HDsnprintf(temp, sizeof temp, "%s-%c.h5",
			       filename, multi_letters[mt]);
		    HDremove(temp); /*don't care if it fails*/
		}
	    } else {
		HDremove(filename);
	    }
	}

	retval = 1;
    }

    H5Pclose(fapl);
    return retval;
}


/*-------------------------------------------------------------------------
 * Function:	h5_reset
 *
 * Purpose:	Reset the library by closing it.
 *
 * Return:	void
 *
 * Programmer:	Robb Matzke
 *              Friday, November 20, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void
h5_reset(void)
{
    char	filename[1024];
    
    HDfflush(stdout);
    HDfflush(stderr);
    H5close();
    H5Eset_auto (h5_errors, NULL);

    /*
     * Cause the library to emit some diagnostics early so they don't
     * interfere with other formatted output.
     */
    sprintf(filename, "/tmp/h5emit-%05d.h5", HDgetpid());
    H5E_BEGIN_TRY {
	hid_t file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT,
			       H5P_DEFAULT);
	hid_t grp = H5Gcreate(file, "emit", 0);
	H5Gclose(grp);
	H5Fclose(file);
	HDunlink(filename);
    } H5E_END_TRY;
}


/*-------------------------------------------------------------------------
 * Function:	h5_fixname
 *
 * Purpose:	Create a file name from a file base name like `test' and
 *		return it through the FULLNAME (at most SIZE characters
 *		counting the null terminator). The full name is created by
 *		prepending the contents of HDF5_PREFIX (separated from the
 *		base name by a slash) and appending a file extension based on
 *		the driver supplied, resulting in something like
 *		`ufs:/u/matzke/test.h5'.
 *
 * Return:	Success:	The FULLNAME pointer.
 *
 *		Failure:	NULL if BASENAME or FULLNAME is the null
 *				pointer or if FULLNAME isn't large enough for
 *				the result.
 *
 * Programmer:	Robb Matzke
 *              Thursday, November 19, 1998
 *
 * Modifications:
 *		Robb Matzke, 1999-08-03
 *		Modified to use the virtual file layer.
 *
 *		Albert Cheng, 2000-01-25
 *		Added prefix for parallel test files.
 *-------------------------------------------------------------------------
 */
char *
h5_fixname(const char *base_name, hid_t fapl, char *fullname, size_t size)
{
    const char     *prefix = NULL;
    const char     *suffix = ".h5"; /* suffix has default */
    char           *ptr, last = '\0';
    size_t          i, j;
    hid_t           driver;
    
    if (!base_name || !fullname || size < 1)
        return NULL;

    memset(fullname, 0, size);

    /* figure out the suffix */
    if (H5P_DEFAULT != fapl){
	if ((driver = H5Pget_driver(fapl)) < 0)
            return NULL;

	if (H5FD_FAMILY == driver)
	    suffix = "%05d.h5";
	else if (H5FD_CORE == driver || H5FD_MULTI == driver)
	    suffix = NULL;
    }
    
    /* Use different ones depending on parallel or serial driver used. */
    if (H5P_DEFAULT != fapl && H5FD_MPIO == driver){
	/* For parallel:
	 * First use command line option, then the environment variable,
	 * then try the constant
	 */
	prefix = (paraprefix ? paraprefix : getenv("HDF5_PARAPREFIX"));

#ifdef HDF5_PARAPREFIX
	if (!prefix)
            prefix = HDF5_PARAPREFIX;
#endif  /* HDF5_PARAPREFIX */
    }else{
	/* For serial:
	 * First use the environment variable, then try the constant
	 */
	prefix = HDgetenv("HDF5_PREFIX");

#ifdef HDF5_PREFIX
	if (!prefix)
            prefix = HDF5_PREFIX;
#endif  /* HDF5_PREFIX */
    }

    /* Prepend the prefix value to the base name */
    if (prefix && *prefix) {
        if (H5P_DEFAULT != fapl && H5FD_MPIO == driver) {
            /* This is a parallel system */
            char *subdir;

            if (!HDstrcmp(prefix, HDF5_PARAPREFIX)) {
                /* If the prefix specifies the HDF5_PARAPREFIX directory, then
                 * default to using the "/tmp/$USER" or "/tmp/$LOGIN"
                 * directory instead. */
                char *user, *login;

                user = HDgetenv("USER");
                login = HDgetenv("LOGIN");
                subdir = (user ? user : login);

                if (subdir) {
                    for (i = 0; i < size && prefix[i]; i++)
                        fullname[i] = prefix[i];

                    fullname[i++] = '/';

                    for (j = 0; i < size && subdir[j]; i++, j++)
                        fullname[i] = subdir[j];
                }
            }

            if (!fullname[0])
                /* We didn't append the prefix yet */
                HDstrncpy(fullname, prefix, MIN(strlen(prefix), size));

            if (HDstrlen(fullname) + HDstrlen(base_name) + 1 < size) {
                /* Append the base_name with a slash first. Multiple slashes are
                 * handled below. */
                h5_stat_t buf;

                if (HDstat(fullname, &buf) < 0)
                    /* The directory doesn't exist just yet */
                    if (HDmkdir(fullname, (mode_t)0755) < 0 && errno != EEXIST) {
                        /* We couldn't make the "/tmp/${USER,LOGIN}" subdirectory.
                         * Default to PREFIX's original prefix value. */
                        HDstrcpy(fullname, prefix);
                    }

                HDstrcat(fullname, "/");
                HDstrcat(fullname, base_name);
            } else {
                /* Buffer is too small */
                return NULL;
            }
        } else {
            if (HDsnprintf(fullname, size, "%s/%s", prefix, base_name) == (int)size)
                /* Buffer is too small */
                return NULL;
        }
    } else if (HDstrlen(base_name) >= size) {
	/* Buffer is too small */
	return NULL;
    } else {
	HDstrcpy(fullname, base_name);
   }

    /* Append a suffix */
    if (suffix) {
	if (HDstrlen(fullname) + HDstrlen(suffix) >= size)
            return NULL;

	HDstrcat(fullname, suffix);
    }

    /* Remove any double slashes in the filename */
    for (ptr = fullname, i = j = 0; ptr && i < size; i++, ptr++) {
        if (*ptr != '/' || last != '/')
            fullname[j++] = *ptr;

        last = *ptr;
    }

    return fullname;
}


/*-------------------------------------------------------------------------
 * Function:	h5_fileaccess
 *
 * Purpose:	Returns a file access template which is the default template
 *		but with a file driver set according to the constant or
 *		environment variable HDF5_DRIVER
 *
 * Return:	Success:	A file access property list
 *
 *		Failure:	-1
 *
 * Programmer:	Robb Matzke
 *              Thursday, November 19, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
hid_t
h5_fileaccess(void)
{
    const char	*val = NULL;
    const char	*name;
    char s[1024];
    hid_t fapl = -1;
    hsize_t fam_size = 100*1024*1024; /*100 MB*/
#ifdef H5_WANT_H5_V1_4_COMPAT
    long verbosity = 1;
#else /* H5_WANT_H5_V1_4_COMPAT */
    long log_flags = H5FD_LOG_LOC_IO;
#endif /* H5_WANT_H5_V1_4_COMPAT */
    H5FD_mem_t	mt;
    
    /* First use the environment variable, then the constant */
    val = HDgetenv("HDF5_DRIVER");
#ifdef HDF5_DRIVER
    if (!val) val = HDF5_DRIVER;
#endif

    if ((fapl=H5Pcreate(H5P_FILE_ACCESS))<0) return -1;
    if (!val || !*val) return fapl; /*use default*/
    
    HDstrncpy(s, val, sizeof s);
    s[sizeof(s)-1] = '\0';
    if (NULL==(name=HDstrtok(s, " \t\n\r"))) return fapl;

    if (!HDstrcmp(name, "sec2")) {
	/* Unix read() and write() system calls */
	if (H5Pset_fapl_sec2(fapl)<0) return -1;
    } else if (!HDstrcmp(name, "stdio")) {
	/* Standard C fread() and fwrite() system calls */
	if (H5Pset_fapl_stdio(fapl)<0) return -1;
    } else if (!HDstrcmp(name, "core")) {
	/* In-core temporary file with 1MB increment */
	if (H5Pset_fapl_core(fapl, 1024*1024, FALSE)<0) return -1;
    } else if (!HDstrcmp(name, "split")) {
	/* Split meta data and raw data each using default driver */
	if (H5Pset_fapl_split(fapl,
			      "-m.h5", H5P_DEFAULT,
			      "-r.h5", H5P_DEFAULT)<0)
	    return -1;
    } else if (!HDstrcmp(name, "multi")) {
	/* Multi-file driver, general case of the split driver */
	H5FD_mem_t memb_map[H5FD_MEM_NTYPES];
	hid_t memb_fapl[H5FD_MEM_NTYPES];
	const char *memb_name[H5FD_MEM_NTYPES];
	char sv[H5FD_MEM_NTYPES][1024];
	haddr_t memb_addr[H5FD_MEM_NTYPES];

	HDmemset(memb_map, 0, sizeof memb_map);
	HDmemset(memb_fapl, 0, sizeof memb_fapl);
	HDmemset((void*)(&memb_name[0]), 0, sizeof memb_name);
	HDmemset(memb_addr, 0, sizeof memb_addr);

	assert(HDstrlen(multi_letters)==H5FD_MEM_NTYPES);
	for (mt=H5FD_MEM_DEFAULT; mt<H5FD_MEM_NTYPES; mt=mt+1) {
	    memb_fapl[mt] = H5P_DEFAULT;
	    sprintf(sv[mt], "%%s-%c.h5", multi_letters[mt]);
	    memb_name[mt] = sv[mt];
	    memb_addr[mt] = MAX(mt-1,0)*(HADDR_MAX/10);
	}

	if (H5Pset_fapl_multi(fapl, memb_map, memb_fapl, memb_name,
			      memb_addr, FALSE)<0) {
	    return -1;
	}
    } else if (!HDstrcmp(name, "family")) {
	/* Family of files, each 1MB and using the default driver */
	if ((val=HDstrtok(NULL, " \t\n\r"))) {
	    fam_size = (hsize_t)(HDstrtod(val, NULL) * 1024*1024);
	}
	if (H5Pset_fapl_family(fapl, fam_size, H5P_DEFAULT)<0) return -1;
    } else if (!HDstrcmp(name, "log")) {
#ifdef H5_WANT_H5_V1_4_COMPAT
        /* Log file access */
        if ((val = strtok(NULL, " \t\n\r")))
            verbosity = strtol(val, NULL, 0);

        if (H5Pset_fapl_log(fapl, NULL, (int)verbosity) < 0)
	    return -1;
#else /* H5_WANT_H5_V1_4_COMPAT */
        /* Log file access */
        if ((val = HDstrtok(NULL, " \t\n\r")))
            log_flags = HDstrtol(val, NULL, 0);

        if (H5Pset_fapl_log(fapl, NULL, (unsigned)log_flags, 0) < 0)
	    return -1;
#endif /* H5_WANT_H5_V1_4_COMPAT */
    } else {
	/* Unknown driver */
	return -1;
    }

    return fapl;
}


/*-------------------------------------------------------------------------
 * Function:	h5_no_hwconv
 *
 * Purpose:	Turn off hardware data type conversions.
 *
 * Return:	void
 *
 * Programmer:	Robb Matzke
 *              Friday, November 20, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void
h5_no_hwconv(void)
{
    H5Tunregister(H5T_PERS_HARD, NULL, -1, -1, NULL);
}


/*-------------------------------------------------------------------------
 * Function:	h5_show_hostname
 *
 * Purpose:	Show hostname.  Show process ID if in MPI environment.
 *
 * Return:	void
 *
 * Programmer:	Albert Cheng
 *              2002/04/22
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void
h5_show_hostname(void)
{
    char	hostname[80];
#ifdef WIN32
     WSADATA wsaData;
     int err;
#endif

    /* try show the process or thread id in multiple processes cases*/
#ifdef H5_HAVE_PARALLEL
    {   
	int mpi_rank, mpi_initialized;

	MPI_Initialized(&mpi_initialized);
	if (mpi_initialized){
	    MPI_Comm_rank(MPI_COMM_WORLD,&mpi_rank);
	    printf("MPI-process %d.", mpi_rank);
	}else
	    printf("thread 0.");
    }
#elif defined(H5_HAVE_THREADSAFE)
    printf("thread %d.", (int)pthread_self());
#else
    printf("thread 0.");
#endif
#ifdef WIN32

   err = WSAStartup( MAKEWORD(2,2), &wsaData );
   if ( err != 0 ) {
    /* could not find a usable WinSock DLL */
    return;
   }
 
/* Confirm that the WinSock DLL supports 2.2.*/
/* Note that if the DLL supports versions greater    */
/* than 2.2 in addition to 2.2, it will still return */
/* 2.2 in wVersion since that is the version we      */
/* requested.                                        */
 
   if ( LOBYTE( wsaData.wVersion ) != 2 ||
        HIBYTE( wsaData.wVersion ) != 2 ) {
    /* could not find a usable WinSock DLL */
     WSACleanup( );
     return; 
   }

#endif

    if (gethostname(hostname, 80) < 0){
	printf(" gethostname failed\n");
    }
    else
	printf(" hostname=%s\n", hostname);
#ifdef WIN32
    WSACleanup();
#endif
}
