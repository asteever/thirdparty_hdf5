/*
 * Copyright � 1999 NCSA
 *                  All rights reserved.
 *
 * Programmer:  Robb Matzke <matzke@llnl.gov>
 *              Thursday, July 29, 1999
 *
 * Purpose:	This is the MPI-2 I/O driver.
 *
 * Limitations:
 *     	H5FD_mpio_read & H5FD_mpio_write
 *		Eventually these should choose collective or independent i/o
 *		based on a parameter that is passed down to it from H5Dwrite,
 *		rather than the access_parms (which are fixed at the open).
 *
 *	H5FD_mpio_read
 *		One implementation of MPI/MPI-IO causes MPI_Get_count
 *		to return (incorrectly) a negative count. I (who?) added code
 *		to detect this, and a kludge to pretend that the number of
 *		bytes read is always equal to the number requested.  This
 *		kluge is activated by #ifdef MPI_KLUGE0202.
 */
#include <assert.h>
#include <hdf5.h>
#include <stdlib.h>

/*
 * The driver identification number, initialized at runtime if HAVE_PARALLEL
 * is defined. This allows applications to still have the H5FD_MPIO
 * "constants" in their source code (it also makes this file strictly ANSI
 * compliant when HAVE_PARALLEL isn't defined)
 */
static hid_t H5FD_MPIO_g = 0;

#ifdef HAVE_PARALLEL

#define FALSE	0
#define TRUE	1

/*
 * The description of a file belonging to this driver.  If the ALLSAME
 * argument is set during a write operation then only p0 will do the actual
 * write (this assumes all procs would write the same data).  The EOF value
 * is only used just after the file is opened in order for the library to
 * determine whether the file is empty, truncated, or okay. The MPIO driver
 * doesn't bother to keep it updated since it's an expensive operation.
 */
typedef struct H5FD_mpio_t {
    H5FD_t	pub;		/*public stuff, must be first		*/
    MPI_File	f;		/*MPIO file handle			*/
    MPI_Comm	comm;		/*communicator				*/
    MPI_Info	info;		/*file information			*/
    hbool_t	allsame;	/*same data for all procs?		*/
    haddr_t	eof;		/*end-of-file marker			*/
    haddr_t	eoa;		/*end-of-address marker			*/
    MPI_Datatype btype;		/*buffer type for xfers			*/
    MPI_Datatype ftype;		/*file type for xfers			*/
    haddr_t	 disp;		/*displacement for set_view in xfers	*/
    int		 use_types;	/*if !0, use btype, ftype, disp.else do
				 * simple byteblk xfer		
				 */
    int		old_use_types; /*remember value of use_types		*/
} H5FD_mpio_t;

/* Prototypes */
static haddr_t MPIOff_to_haddr(MPI_Offset mpi_off);
static herr_t haddr_to_MPIOff(haddr_t addr, MPI_Offset *mpi_off/*out*/);

/* Callbacks */
static H5FD_t *H5FD_mpio_open(const char *name, unsigned flags, hid_t fapl_id,
			      haddr_t maxaddr);
static herr_t H5FD_mpio_close(H5FD_t *_file);
static haddr_t H5FD_mpio_get_eoa(H5FD_t *_file);
static herr_t H5FD_mpio_set_eoa(H5FD_t *_file, haddr_t addr);
static haddr_t H5FD_mpio_get_eof(H5FD_t *_file);
static herr_t H5FD_mpio_read(H5FD_t *_file, hid_t fapl_id, haddr_t addr,
			     hsize_t size, void *buf);
static herr_t H5FD_mpio_write(H5FD_t *_file, hid_t fapl_id, haddr_t addr,
			      hsize_t size, const void *buf);
static herr_t H5FD_mpio_flush(H5FD_t *_file);

/* MPIO-specific file access properties */
typedef struct H5FD_mpio_fapl_t {
    MPI_Comm		comm;		/*communicator			*/
    MPI_Info		info;		/*file information		*/
} H5FD_mpio_fapl_t;

/* The MPIO file driver information */
static const H5FD_class_t H5FD_mpio_g = {
    "mpio",					/*name			*/
    HADDR_MAX,					/*maxaddr		*/
    sizeof(H5FD_mpio_fapl_t),			/*fapl_size		*/
    NULL,					/*fapl_copy		*/
    NULL, 					/*fapl_free		*/
    sizeof(H5FD_mpio_dxpl_t),			/*dxpl_size		*/
    NULL,					/*dxpl_copy		*/
    NULL,					/*dxpl_free		*/
    H5FD_mpio_open,				/*open			*/
    H5FD_mpio_close,				/*close			*/
    NULL,					/*cmp			*/
    NULL,					/*alloc			*/
    NULL,					/*free			*/
    H5FD_mpio_get_eoa,				/*get_eoa		*/
    H5FD_mpio_set_eoa, 				/*set_eoa		*/
    H5FD_mpio_get_eof,				/*get_eof		*/
    H5FD_mpio_read,				/*read			*/
    H5FD_mpio_write,				/*write			*/
    H5FD_mpio_flush,				/*flush			*/
    H5FD_FLMAP_SINGLE,				/*fl_map		*/
};

#ifdef H5FDmpio_DEBUG
/* Flags to control debug actions in H5Fmpio.
 * Meant to be indexed by characters.
 *
 * 'c' show result of MPI_Get_count after read
 * 'r' show read offset and size
 * 't' trace function entry and exit
 * 'w' show write offset and size
 */
static int H5FD_mpio_Debug[256] =
        { 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
          0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0 };
#endif

/* Global var to allow elimination of redundant metadata writes
 * to be controlled by the value of an environment variable. */
/* Use the elimination by default unless this is the Intel Red machine */
#ifndef __PUMAGON__
hbool_t	H5_mpi_1_metawrite_g = TRUE;
#else
hbool_t	H5_mpi_1_metawrite_g = FALSE;
#endif


/*-------------------------------------------------------------------------
 * Function:	H5FD_mpio_init
 *
 * Purpose:	Initialize this driver by registering the driver with the
 *		library.
 *
 * Return:	Success:	The driver ID for the mpio driver.
 *
 *		Failure:	Negative.
 *
 * Programmer:	Robb Matzke
 *              Thursday, August 5, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5FD_mpio_init(void)
{
    if (!H5FD_MPIO_g) {
	H5FD_MPIO_g = H5FDregister(&H5FD_mpio_g);

#if 1
	/*
	 * To be removed after Albert proof reads this driver.
	 * --rpm 1999-08-06
	 */
	fprintf(stderr, "\
H5FD_MPIO: this driver is currently under construction and may\n\
    not work as advertised. Please use hdf5-1.3.? if you need a\n\
    more stable development version (or use the hdf5-1.2.x release\n\
    version).\n");
#endif
    }
    return H5FD_MPIO_g;
}


/*-------------------------------------------------------------------------
 * Function:	H5Pset_fapl_mpio
 *
 * Purpose:	Store the user supplied MPIO communicator COMM and INFO in
 *		the file access property list FAPL_ID which can then be used
 *		to create and/or open the file.  This function is available
 *		only in the parallel HDF5 library and is not a collective
 *		function.
 *
 *		COMM is the MPI communicator to be used for file open as
 *		defined in MPI_FILE_OPEN of MPI-2. This function does not
 *		make a duplicated communicator. Any modification to COMM
 *		after this function call returns may have undetermined effect
 *		on the access property list. Users should not modify the
 *		communicator while it is defined in a property list.
 *
 *		INFO is the MPI info object to be used for file open as
 *		defined in MPI_FILE_OPEN of MPI-2. This function does not
 *		make a duplicated info. Any modification to info after this
 *		function call returns may have undetermined effect on the
 *		access property list. Users should not modify the info while
 *		it is defined in a property list.
 *
 * Return:	Success:	Non-negative
 *
 * 		Failure:	Negative
 *
 * Programmer:	Albert Cheng
 *		Feb 3, 1998
 *
 * Modifications:
 *		Robb Matzke, 1998-02-18
 *		Check all arguments before the property list is updated so we
 *		don't leave the property list in a bad state if something
 *		goes wrong.  Also, the property list data type changed to
 *		allow more generality so all the mpi-related stuff is in the
 *		`u.mpi' member.  The `access_mode' will contain only
 * 		mpi-related flags defined in H5Fpublic.h.
 *
 *		Albert Cheng, 1998-04-16
 *		Removed the ACCESS_MODE argument.  The access mode is changed
 *		to be controlled by data transfer property list during data
 *		read/write calls.
 *
 * 		Robb Matzke, 1999-08-06
 *		Modified to work with the virtual file layer.
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_fapl_mpio(hid_t fapl_id, MPI_Comm comm, MPI_Info info)
{
    H5FD_mpio_fapl_t	fa;
    
    /*NO TRACE*/

    /* Check arguments */
    if (H5P_FILE_ACCESS!=H5Pget_class(fapl_id)) return -1;
#ifdef LATER
#warning "We need to verify that COMM and INFO contain sensible information."
#endif

    /* Initialize driver specific properties */
    fa.comm = comm;
    fa.info = info;
    return H5Pset_driver(fapl_id, H5FD_MPIO, &fa);
}


/*-------------------------------------------------------------------------
 * Function:	H5Pget_fapl_mpio
 *
 * Purpose:	If the file access property list is set to the H5FD_MPIO
 *		driver then this function returns the MPI communicator and
 *		information through the COMM and INFO pointers.
 *
 * Return:	Success:	Non-negative with the communicator and
 *				information returned through the COMM and
 *				INFO arguments if non-null. Neither piece of
 *				information is copied and they are therefore
 *				valid only until the file access property
 *				list is modified or closed.
 *
 * 		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		Thursday, February 26, 1998
 *
 * Modifications:
 *
 *	Albert Cheng, Apr 16, 1998
 *	Removed the access_mode argument.  The access_mode is changed
 *	to be controlled by data transfer property list during data
 *	read/write calls.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_fapl_mpio(hid_t fapl_id, MPI_Comm *comm/*out*/, MPI_Info *info/*out*/)
{
    H5FD_mpio_fapl_t	*fa;
    
    /*NO TRACE*/

    if (H5P_FILE_ACCESS!=H5Pget_class(fapl_id)) return -1;
    if (H5FD_MPIO!=H5Pget_driver(fapl_id)) return -1;
    if (NULL==(fa=H5Pget_driver_info(fapl_id))) return -1;

    if (comm) *comm = fa->comm;
    if (info) *info = fa->info;
    return 0;
}


/*-------------------------------------------------------------------------
 * Function:	H5Pset_dxpl_mpio
 *
 * Purpose:	Set the data transfer property list DXPL_ID to use transfer
 *		mode XFER_MODE. The property list can then be used to control
 *		the I/O transfer mode during data I/O operations. The valid
 *		transfer modes are:
 *
 * 		H5FD_MPIO_INDEPENDENT:
 *			Use independent I/O access (the default).
 *
 * 		H5FD_MPIO_COLLECTIVE:
 *			Use collective I/O access.
 *			
 * Return:	Success:	Non-negative
 *
 * 		Failure:	Negative
 *
 * Programmer:	Albert Cheng
 *		April 2, 1998
 *
 * Modifications:
 * 		Robb Matzke, 1999-08-06
 *		Modified to work with the virtual file layer.
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_dxpl_mpio(hid_t dxpl_id, H5FD_mpio_xfer_t xfer_mode)
{
    H5FD_mpio_dxpl_t	dx;

    /*NO TRACE*/
    
    /* Check arguments */
    if (H5P_DATA_XFER!=H5Pget_class(dxpl_id)) return -1;
    if (H5FD_MPIO_INDEPENDENT!=xfer_mode &&
	H5FD_MPIO_COLLECTIVE!=xfer_mode) return -1;

    /* Initialize driver-specific properties */
    dx.xfer_mode = xfer_mode;
    return H5Pset_driver(dxpl_id, H5FD_MPIO, &dx);
}


/*-------------------------------------------------------------------------
 * Function:	H5Pget_dxpl_mpio
 *
 * Purpose:	Queries the transfer mode current set in the data transfer
 *		property list DXPL_ID. This is not a collective function.
 *
 * Return:	Success:	Non-negative, with the transfer mode returned
 *				through the XFER_MODE argument if it is
 *				non-null.
 *
 * 		Failure:	Negative
 *
 * Programmer:	Albert Cheng
 *		April 2, 1998
 *
 * Modifications:
 * 		Robb Matzke, 1999-08-06
 *		Modified to work with the virtual file layer.
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_dxpl_mpio(hid_t dxpl_id, H5FD_mpio_xfer_t *xfer_mode/*out*/)
{
    H5FD_mpio_dxpl_t	*dx;

    /*NO TRACE*/
    
    if (H5P_DATA_XFER!=H5Pget_class(dxpl_id)) return -1;
    if (H5FD_MPIO!=H5Pget_driver(dxpl_id)) return -1;
    if (NULL==(dx=H5Pget_driver_info(dxpl_id))) return -1;

    if (xfer_mode) *xfer_mode = dx->xfer_mode;
    return 0;
}


/*-------------------------------------------------------------------------
 * Function:    H5FD_mpio_tas_allsame
 *
 * Purpose:     Test and set the allsame parameter.
 *
 * Return:      Success:        the old value of the allsame flag
 *
 *              Failure:        assert fails if access_parms is NULL.
 *
 * Programmer:  rky 980828
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
htri_t
H5FD_mpio_tas_allsame(H5FD_t *_file, hbool_t newval)
{
    H5FD_mpio_t	*file = (H5FD_mpio_t*)_file;
    hbool_t	oldval;

#ifdef H5FDmpio_DEBUG
    if (H5FD_mpio_Debug[(int)'t'])
    	fprintf(stdout, "Entering H5FD_mpio_tas_allsame, newval=%d\n", newval);
#endif

    assert(file);
    oldval = file->allsame;
    file->allsame = newval;

#ifdef H5FDmpio_DEBUG
    if (H5FD_mpio_Debug[(int)'t'])
    	fprintf(stdout, "Leaving H5FD_mpio_tas_allsame, oldval=%d\n", oldval);
#endif

    return oldval;
}


/*-------------------------------------------------------------------------
 * Function:	H5FD_mpio_communicator
 *
 * Purpose:	Returns the MPI communicator for the file.
 *
 * Return:	Success:	The communicator
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *              Monday, August  9, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
MPI_Comm
H5FD_mpio_communicator(H5FD_t *_file)
{
    H5FD_mpio_t	*file = (H5FD_mpio_t*)_file;
    return file->comm;
}


/*-------------------------------------------------------------------------
 * Function:	H5FD_mpio_setup
 *
 * Purpose:	Set the buffer type BTYPE, file type FTYPE, and absolute base
 *		address DISP (i.e., the file view displacement) for a data
 *		transfer. Also request a dataspace transfer or an elementary
 *		byteblock transfer depending on whether USE_TYPES is non-zero
 *		or zero, respectively.
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Robb Matzke
 *              Monday, August  9, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5FD_mpio_setup(H5FD_t *_file, MPI_Datatype btype, MPI_Datatype ftype,
		haddr_t disp, hbool_t use_types)
{
    H5FD_mpio_t	*file = (H5FD_mpio_t*)_file;

    file->btype = btype;
    file->ftype = ftype;
    file->disp = disp;
    file->use_types = use_types;
    return 0;
}


/*-------------------------------------------------------------------------
 * Function:	H5FD_mpio_wait_for_left_neighbor
 *
 * Purpose:	Blocks until (empty) msg is received from immediately
 *		lower-rank neighbor. In conjunction with
 *		H5FD_mpio_signal_right_neighbor, useful for enforcing
 *		1-process-at-at-time access to critical regions to avoid race
 *		conditions (though it is overkill to require that the
 *		processes be allowed to proceed strictly in order of their
 *		rank).
 *
 * Note:	This routine doesn't read or write any file, just performs
 *		interprocess coordination. It really should reside in a
 *		separate package of such routines.
 *
 * Return:	Success:	0
 *		Failure:	-1
 *
 * Programmer:	rky
 *              19981207
 *
 * Modifications:
 *		Robb Matzke, 1999-08-09
 *		Modified to work with the virtual file layer.
 *-------------------------------------------------------------------------
 */
herr_t
H5FD_mpio_wait_for_left_neighbor(H5FD_t *_file)
{
    H5FD_mpio_t	*file = (H5FD_mpio_t*)_file;
    MPI_Comm comm = file->comm;
    char msgbuf[1];
    int myid, mpi_err;
    MPI_Status rcvstat;

    mpi_err = MPI_Comm_rank(comm, &myid);
    if (MPI_SUCCESS!=mpi_err) return -1;

    /* p0 has no left neighbor; all other procs wait for msg */
    if (myid != 0) {
	mpi_err = MPI_Recv( &msgbuf, 1, MPI_CHAR, myid-1, MPI_ANY_TAG, comm,
			    &rcvstat );
	if (MPI_SUCCESS!=mpi_err) return -1;
    }
    
    return 0;
}


/*-------------------------------------------------------------------------
 * Function:	H5FD_mpio_signal_right_neighbor
 *
 * Purpose:	Blocks until (empty) msg is received from immediately
 *		lower-rank neighbor. In conjunction with
 *		H5FD_mpio_wait_for_left_neighbor, useful for enforcing
 *		1-process-at-at-time access to critical regions to avoid race
 *		conditions (though it is overkill to require that the
 *		processes be allowed to proceed strictly in order of their
 *		rank).
 *
 * Note: 	This routine doesn't read or write any file, just performs
 *		interprocess coordination. It really should reside in a
 *		separate package of such routines.
 *
 * Return:	Success:	0
 *		Failure:	-1
 *
 * Programmer:	rky
 *              19981207
 *
 * Modifications:
 *		Robb Matzke, 1999-08-09
 *		Modified to work with the virtual file layer.
 *-------------------------------------------------------------------------
 */
herr_t
H5FD_mpio_signal_right_neighbor(H5FD_t *_file)
{
    H5FD_mpio_t	*file = (H5FD_mpio_t*)_file;
    MPI_Comm comm = file->comm;
    char msgbuf[1];
    int myid, numprocs, mpi_err;

    mpi_err = MPI_Comm_size( comm, &numprocs );
    if (MPI_SUCCESS!=mpi_err) return -1;
    mpi_err = MPI_Comm_rank( comm, &myid );
    if (MPI_SUCCESS!=mpi_err) return -1;
    if (myid != (numprocs-1)) {
	mpi_err = MPI_Send(&msgbuf, 0/*empty msg*/, MPI_CHAR, myid+1, 0, comm);
	if (MPI_SUCCESS!=mpi_err) return -1;
    }
    return 0;
}


/*-------------------------------------------------------------------------
 * Function:    H5FD_mpio_open
 *
 * Purpose:     Opens a file with name NAME.  The FLAGS are a bit field with
 *		purpose similar to the second argument of open(2) and which
 *		are defined in H5Fpublic.h. The file access property list
 *		FAPL_ID contains the properties driver properties and MAXADDR
 *		is the largest address which this file will be expected to
 *		access.
 *
 * Return:      Success:        A new file pointer.
 *
 *              Failure:        NULL
 *
 * Programmer:  
 *              January 30, 1998
 *
 * Modifications:
 * 		Robb Matzke, 1998-02-18
 *		Added the ACCESS_PARMS argument.  Moved some error checking
 *		here from elsewhere.
 *
 *      	rky, 1998-01-11
 *      	Added H5FD_mpio_Debug debug flags controlled by MPI_Info.
 *
 *      	rky, 1998-08-28
 *		Init flag controlling redundant metadata writes to disk.
 *
 *      	rky, 1998-12-07
 *		Added barrier after MPI_File_set_size to prevent race
 *		condition -- subsequent writes were being truncated, causing
 *		holes in file.
 *		
 * 		Robb Matzke, 1999-08-06
 *		Modified to work with the virtual file layer.
 *-------------------------------------------------------------------------
 */
static H5FD_t *
H5FD_mpio_open(const char *name, unsigned flags, hid_t fapl_id,
	       haddr_t maxaddr/*unused*/)
{
    H5FD_mpio_t			*file=NULL;
    MPI_File			fh;
    int				mpi_amode;
    int				mpierr;
    MPI_Offset			size;
    const H5FD_mpio_fapl_t	*fa=NULL;
    H5FD_mpio_fapl_t		_fa;

#ifdef H5FDmpio_DEBUG
    if (H5FD_mpio_Debug[(int)'t']) {
    	fprintf(stdout, "Entering H5FD_mpio_open(name=\"%s\", flags=0x%x, "
		"fapl_id=%lu, maxaddr=%lu)\n", name, flags, fapl_id, maxaddr);
    }
#endif

    /* Obtain a pointer to mpio-specific file access properties */
    if (H5P_DEFAULT==fapl_id || H5FD_MPIO!=H5Pget_driver(fapl_id)) {
	_fa.comm = MPI_COMM_SELF; /*default*/
	_fa.info = MPI_INFO_NULL; /*default*/
	fa = &_fa;
    } else {
	fa = H5Pget_driver_info(fapl_id);
	assert(fa);
    }

    /* convert HDF5 flags to MPI-IO flags */
    /* some combinations are illegal; let MPI-IO figure it out */
    mpi_amode  = (flags&H5F_ACC_RDWR) ? MPI_MODE_RDWR : MPI_MODE_RDONLY;
    if (flags&H5F_ACC_CREAT)	mpi_amode |= MPI_MODE_CREATE;
    if (flags&H5F_ACC_EXCL)	mpi_amode |= MPI_MODE_EXCL;

#ifdef H5FDmpio_DEBUG
    {
	/* set debug mask */
	/* Should this be done in H5F global initialization instead of here? */
        const char *s = HDgetenv ("H5FD_mpio_Debug");
        if (s) {
	    while (*s){
		H5FD_mpio_Debug[(int)*s]++;
		s++;
	    }
        }
    }
    
    /* Check for debug commands in the info parameter */
    {
	char debug_str[128];
        int infoerr, flag, i;
        if (fa->info) {
            infoerr = MPI_Info_get(fa->info, H5FD_MPIO_DEBUG_KEY, 127,
				   debug_str, &flag);
            if (flag) {
                fprintf(stdout, "H5FD_mpio debug flags=%s\n", debug_str );
                for (i=0;
                     debug_str[i]/*end of string*/ && i<128/*just in case*/;
                     ++i) {
                    H5FD_mpio_Debug[(int)debug_str[i]] = 1;
                }
            }
        }
    }
#endif

    /*OKAY: CAST DISCARDS CONST*/
    mpierr = MPI_File_open(fa->comm, (char*)name, mpi_amode, fa->info, &fh);
    if (MPI_SUCCESS != mpierr) return NULL;

    /* truncate the file, if requested */
    if (flags & H5F_ACC_TRUNC) {
	mpierr = MPI_File_set_size(fh, (MPI_Offset)0);
	if (MPI_SUCCESS != mpierr) {
	    MPI_File_close(&fh);
	    return NULL;
	}

	/* Don't let any proc return until all have truncated the file. */
	mpierr = MPI_Barrier(fa->comm);
	if (MPI_SUCCESS!=mpierr) {
	    MPI_File_close(&fh);
	    return NULL;
	}
    }

    /* Build the return value and initialize it */
    if (NULL==(file=calloc(1, sizeof(H5FD_mpio_t)))) return NULL;
    file->f = fh;
    file->comm = fa->comm;
    file->info = fa->info;
    file->btype = MPI_DATATYPE_NULL;
    file->ftype = MPI_DATATYPE_NULL;

    /* Get current file size */
    mpierr = MPI_File_get_size(fh, &size);
    if (MPI_SUCCESS != mpierr) {
	free(file);
	MPI_File_close(&fh);
	return NULL;
    }
    file->eof = MPIOff_to_haddr(size);

#ifdef H5FDmpio_DEBUG
    if (H5FD_mpio_Debug[(int)'t']) {
	fprintf(stdout, "Leaving H5FD_mpio_open\n" );
    }
#endif

    return (H5FD_t*)file;
}


/*-------------------------------------------------------------------------
 * Function:    H5FD_mpio_close
 *
 * Purpose:     Closes a file.
 *
 * Return:      Success:	Non-negative
 *
 * 		Failure:	Negative
 *
 * Programmer:  Unknown
 *              January 30, 1998
 *
 * Modifications:
 * 		Robb Matzke, 1998-02-18
 *		Added the ACCESS_PARMS argument.
 *
 * 		Robb Matzke, 1999-08-06
 *		Modified to work with the virtual file layer.
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_mpio_close(H5FD_t *_file)
{
    H5FD_mpio_t	*file = (H5FD_mpio_t*)_file;
    int         mpierr;

#ifdef H5FDmpio_DEBUG
    if (H5FD_mpio_Debug[(int)'t'])
    	fprintf(stdout, "Entering H5FD_mpio_close\n");
#endif

    /* MPI_File_close sets argument to MPI_FILE_NULL */
    mpierr = MPI_File_close(&(file->f)/*in,out*/);
    if (MPI_SUCCESS != mpierr) return -1;

#ifdef H5FDmpio_DEBUG
    if (H5FD_mpio_Debug[(int)'t'])
    	fprintf(stdout, "Leaving H5FD_mpio_close\n");
#endif

    return 0;
}


/*-------------------------------------------------------------------------
 * Function:	H5FD_mpio_get_eoa
 *
 * Purpose:	Gets the end-of-address marker for the file. The EOA marker
 *		is the first address past the last byte allocated in the
 *		format address space.
 *
 * Return:	Success:	The end-of-address marker.
 *
 *		Failure:	HADDR_UNDEF
 *
 * Programmer:	Robb Matzke
 *              Friday, August  6, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static haddr_t
H5FD_mpio_get_eoa(H5FD_t *_file)
{
    H5FD_mpio_t	*file = (H5FD_mpio_t*)_file;
    return file->eoa;
}


/*-------------------------------------------------------------------------
 * Function:	H5FD_mpio_set_eoa
 *
 * Purpose:	Set the end-of-address marker for the file. This function is
 *		called shortly after an existing HDF5 file is opened in order
 *		to tell the driver where the end of the HDF5 data is located.
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Robb Matzke
 *              Friday, August 6, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_mpio_set_eoa(H5FD_t *_file, haddr_t addr)
{
    H5FD_mpio_t	*file = (H5FD_mpio_t*)_file;
    file->eoa = addr;
    return 0;
}


/*-------------------------------------------------------------------------
 * Function:	H5FD_mpio_get_eof
 *
 * Purpose:	Gets the end-of-file marker for the file. The EOF marker
 *		is the real size of the file.
 *
 *		The MPIO driver doesn't bother keeping this field updated
 *		since that's a relatively expensive operation. Fortunately
 *		the library only needs the EOF just after the file is opened
 *		in order to determine whether the file is empty, truncated,
 *		or okay.  Therefore, any MPIO I/O function will set its value
 *		to HADDR_UNDEF which is the error return value of this
 *		function.
 *
 * Return:	Success:	The end-of-address marker.
 *
 *		Failure:	HADDR_UNDEF
 *
 * Programmer:	Robb Matzke
 *              Friday, August  6, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static haddr_t
H5FD_mpio_get_eof(H5FD_t *_file)
{
    H5FD_mpio_t	*file = (H5FD_mpio_t*)_file;
    return file->eof;
}


/*-------------------------------------------------------------------------
 * Function:	H5FD_mpio_read
 *
 * Purpose:	Reads SIZE bytes of data from FILE beginning at address ADDR
 *		into buffer BUF according to data transfer properties in
 *		DXPL_ID using potentially complex file and buffer types to
 *		effect the transfer.
 *
 *		Reading past the end of the MPI file returns zeros instead of
 *		failing.  MPI is able to coalesce requests from different
 *		processes (collective or independent).
 *
 * Return:	Success:	Zero. Result is stored in caller-supplied
 *				buffer BUF.
 *
 *		Failure:	-1, Contents of buffer BUF are undefined.
 *
 * Programmer:	rky, 1998-01-30
 *
 * Modifications:
 * 		Robb Matzke, 1998-02-18
 *		Added the ACCESS_PARMS argument.
 *
 * 		rky, 1998-04-10
 *		Call independent or collective MPI read, based on
 *		ACCESS_PARMS.
 *
 * 		Albert Cheng, 1998-06-01
 *		Added XFER_MODE to control independent or collective MPI
 *		read.
 *
 * 		rky, 1998-08-16
 *		Use BTYPE, FTYPE, and DISP from access parms. The guts of
 *		H5FD_mpio_read and H5FD_mpio_write should be replaced by a
 *		single dual-purpose routine.
 *
 * 		Robb Matzke, 1999-04-21
 *		Changed XFER_MODE to XFER_PARMS for all H5F_*_read()
 *		callbacks.
 *
 * 		Robb Matzke, 1999-07-28
 *		The ADDR argument is passed by value.
 *		
 * 		Robb Matzke, 1999-08-06
 *		Modified to work with the virtual file layer.
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_mpio_read(H5FD_t *_file, hid_t dxpl_id, haddr_t addr, hsize_t size,
	       void *buf/*out*/)
{
    H5FD_mpio_t			*file = (H5FD_mpio_t*)_file;
    const H5FD_mpio_dxpl_t	*dx=NULL;
    H5FD_mpio_dxpl_t		_dx;
    MPI_Offset			mpi_off, mpi_disp;
    MPI_Status  		mpi_stat;
    MPI_Datatype		buf_type, file_type;
    int         		mpierr, size_i, bytes_read, n;
    int				use_types_this_time, used_types_last_time;

#ifdef H5FDmpio_DEBUG
    if (H5FD_mpio_Debug[(int)'t'])
    	fprintf(stdout, "Entering H5FD_mpio_read\n" );
#endif

    /* some numeric conversions */
    if (haddr_to_MPIOff(addr, &mpi_off/*out*/)<0) return -1;
    size_i = (int)size;
    if ((size_t)size_i != size) return -1;

#ifdef H5FDmpio_DEBUG
    if (H5FD_mpio_Debug[(int)'r'])
        fprintf(stdout, "in H5FD_mpio_read  mpi_off=%ld  size_i=%d\n",
		(long)mpi_off, size_i );
#endif

    /* Obtain the data transfer properties */
    if (H5P_DEFAULT==dxpl_id || H5FD_MPIO!=H5Pget_driver(dxpl_id)) {
	_dx.xfer_mode = H5FD_MPIO_INDEPENDENT; /*the default*/
	dx = &_dx;
    } else {
	dx = H5Pget_driver_info(dxpl_id);
	assert(dx);
    }
    
    /*
     * Set up for a fancy xfer using complex types, or single byte block. We
     * wouldn't need to rely on the use_types field if MPI semantics allowed
     * us to test that btype=ftype=MPI_BYTE (or even MPI_TYPE_NULL, which
     * could mean "use MPI_BYTE" by convention).
     */
    use_types_this_time = file->use_types;
    if (use_types_this_time) {
	/* prepare for a full-blown xfer using btype, ftype, and disp */
	buf_type = file->btype;
	file_type = file->ftype;
	if (haddr_to_MPIOff(file->disp, &mpi_disp)<0) return -1;
    } else {
	/*
	 * Prepare for a simple xfer of a contiguous block of bytes. The
	 * btype, ftype, and disp fields are not used.
	 */
	buf_type = MPI_BYTE;
	file_type = MPI_BYTE;
	mpi_disp = 0;		/* mpi_off is sufficient */
    }

    /*
     * Don't bother to reset the view if we're not using the types this time,
     * and did we didn't use them last time either.
     */
    used_types_last_time = file->old_use_types;
    if (used_types_last_time || /* change to new ftype or MPI_BYTE */
	use_types_this_time) { 	/* almost certainly a different ftype */
	/*OKAY: CAST DISCARDS CONST QUALIFIER*/
	mpierr = MPI_File_set_view(file->f, mpi_disp, MPI_BYTE, file_type,
				   (char*)"native",  file->info);
	if (MPI_SUCCESS != mpierr) return -1;
    }
    
    /*
     * We always set the use_types flag to 0 because the default is not to
     * use types next time, unless someone explicitly requests it by setting
     * this flag to !=0.
     */
    file->old_use_types = use_types_this_time;
    file->use_types = 0;

    /* Read the data. */
    assert(H5FD_MPIO_INDEPENDENT==dx->xfer_mode ||
	   H5FD_MPIO_COLLECTIVE==dx->xfer_mode);
    if (H5FD_MPIO_INDEPENDENT==dx->xfer_mode) {
	mpierr = MPI_File_read_at(file->f, mpi_off, buf, size_i, buf_type,
				  &mpi_stat);
	if (MPI_SUCCESS!=mpierr) return -1;
    } else {
#ifdef H5FDmpio_DEBUG
	if (H5FD_mpio_Debug[(int)'t'])
	    fprintf(stdout, "H5FD_mpio_read: using MPIO collective mode\n");
#endif
	mpierr = MPI_File_read_at_all(file->f, mpi_off, buf, size_i, buf_type,
				      &mpi_stat );
	if (MPI_SUCCESS!=mpierr) return -1;
    }

    /* How many bytes were actually read? */
    mpierr = MPI_Get_count(&mpi_stat, MPI_BYTE, &bytes_read);
    if (MPI_SUCCESS != mpierr) return -1;
#ifdef H5FDmpio_DEBUG
    if (H5FD_mpio_Debug[(int)'c'])
    	fprintf(stdout,
	    "In H5FD_mpio_read after Get_count size_i=%d bytes_read=%d\n",
	    size_i, bytes_read );
#endif
#if 1
    /*
     * KLUGE rky 1998-02-02
     * MPI_Get_count incorrectly returns negative count; fake a complete
     * read.
     */
    bytes_read = size_i;
#endif
    if (bytes_read<0 || bytes_read>size_i) return -1;

    /*
     * This gives us zeroes beyond end of physical MPI file.  What about
     * reading past logical end of HDF5 file???
     */
    if ((n=(size_i-bytes_read)) > 0) {
	if (use_types_this_time) {
	    /*
	     * INCOMPLETE rky 1998-09-18
	     * Haven't implemented reading zeros beyond EOF. What to do???
	     */
	    return -1;
	} else {
	    memset((char*)buf+bytes_read, 0, (size_t)n);
	}
    }

    /* Forget the EOF value (see H5FD_mpio_get_eof()) --rpm 1999-08-06 */
    file->eof = HADDR_UNDEF;

#ifdef H5FDmpio_DEBUG
    if (H5FD_mpio_Debug[(int)'t'])
    	fprintf(stdout, "Leaving H5FD_mpio_read\n" );
#endif

    return 0;
}


/*-------------------------------------------------------------------------
 * Function:	H5FD_mpio_write
 *
 * Purpose:	Writes SIZE bytes of data to FILE beginning at address ADDR
 *		from buffer BUF according to data transfer properties in
 *		DXPL_ID using potentially complex file and buffer types to
 *		effect the transfer.
 *
 *		MPI is able to coalesce requests from different processes
 *		(collective and independent).
 *
 * Return:	Success:	Zero. USE_TYPES and OLD_USE_TYPES in the
 *				access params are altered.
 *
 *		Failure:	-1, USE_TYPES and OLD_USE_TYPES in the
 *				access params may be altered.
 *
 * Programmer:	Unknown
 *              January 30, 1998
 *
 * Modifications:
 *		rky, 1998-08-28
 *		If the file->allsame flag is set, we assume that all the
 *		procs in the relevant MPI communicator will write identical
 *		data at identical offsets in the file, so only proc 0 will
 *		write, and all other procs will wait for p0 to finish. This
 *		is useful for writing metadata, for example. Note that we
 *		don't _check_ that the data is identical. Also, the mechanism
 *		we use to eliminate the redundant writes is by requiring a
 *		call to H5FD_mpio_tas_allsame before the write, which is
 *		rather klugey. Would it be better to pass a parameter to
 *		low-level writes like H5F_block_write and H5F_low_write,
 *		instead?  Or...??? Also, when I created this mechanism I
 *		wanted to minimize the difference in behavior between the old
 *		way of doing things (i.e., all procs write) and the new way,
 *		so the writes are eliminated at the very lowest level, here
 *		in H5FD_mpio_write. It may be better to rethink that, and
 *		short-circuit the writes at a higher level (e.g., at the
 *		points in the code where H5FD_mpio_tas_allsame is called).
 *
 *
 * 		Robb Matzke, 1998-02-18
 *		Added the ACCESS_PARMS argument.
 *
 * 		rky, 1998-04-10
 *		Call independent or collective MPI write, based on
 *		ACCESS_PARMS.
 *
 * 		rky, 1998-04-24
 *		Removed redundant write from H5FD_mpio_write.
 *
 * 		Albert Cheng, 1998-06-01
 *		Added XFER_MODE to control independent or collective MPI
 *		write.
 *
 * 		rky, 1998-08-16
 *		Use BTYPE, FTYPE, and DISP from access parms. The guts of
 *		H5FD_mpio_read and H5FD_mpio_write should be replaced by a
 *		single dual-purpose routine.
 *
 * 		rky, 1998-08-28
 *		Added ALLSAME parameter to make all but proc 0 skip the
 *		actual write.
 *
 * 		Robb Matzke, 1999-04-21
 *		Changed XFER_MODE to XFER_PARMS for all H5FD_*_write()
 *		callbacks.
 *
 * 		Robb Matzke, 1999-07-28
 *		The ADDR argument is passed by value.
 *
 * 		Robb Matzke, 1999-08-06
 *		Modified to work with the virtual file layer.
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_mpio_write(H5FD_t *_file, hid_t dxpl_id/*unused*/, haddr_t addr,
		hsize_t size, const void *buf)
{
    H5FD_mpio_t			*file = (H5FD_mpio_t*)_file;
    const H5FD_mpio_dxpl_t	*dx=NULL;
    H5FD_mpio_dxpl_t		_dx;
    MPI_Offset 		 	mpi_off, mpi_disp;
    MPI_Status			mpi_stat;
    MPI_Datatype		buf_type, file_type;
    int         		mpierr, size_i, bytes_written;
    int				mpi_rank;
    int				use_types_this_time, used_types_last_time;
    hbool_t     		allsame;

#ifdef H5FDmpio_DEBUG
    if (H5FD_mpio_Debug[(int)'t'])
    	fprintf(stdout, "Entering H5FD_mpio_write\n" );
#endif

    /* some numeric conversions */
    if (haddr_to_MPIOff(addr, &mpi_off)<0) return -1;
    if (haddr_to_MPIOff(file->disp, &mpi_disp)<0) return -1;
    size_i = (int)size;
    if ((size_t)size_i != size) return -1;

#ifdef H5FDmpio_DEBUG
    if (H5FD_mpio_Debug[(int)'w'])
        fprintf(stdout, "in H5FD_mpio_write  mpi_off=%ld  size_i=%d\n",
                (long)mpi_off, size_i);
#endif

    /* Only p0 will do the actual write if all procs in comm write same data */
    allsame = H5FD_mpio_tas_allsame(_file, FALSE);
    if (allsame && H5_mpi_1_metawrite_g) {
	mpierr = MPI_Comm_rank(file->comm, &mpi_rank);
	if (mpierr != MPI_SUCCESS) return -1;
	if (mpi_rank != 0) {
#ifdef H5FDmpio_DEBUG
	    if (H5FD_mpio_Debug[(int)'w']) {
		fprintf(stdout, "  in H5FD_mpio_write (write omitted)\n" );
	    }
#endif
	    goto done;			/* skip the actual write */
	}
    }

    /* Obtain the data transfer properties */
    if (H5P_DEFAULT==dxpl_id || H5FD_MPIO!=H5Pget_driver(dxpl_id)) {
	_dx.xfer_mode = H5FD_MPIO_INDEPENDENT; /*the default*/
	dx = &_dx;
    } else {
	dx = H5Pget_driver_info(dxpl_id);
	assert(dx);
    }
    
    /*
     * Set up for a fancy xfer using complex types, or single byte block. We
     * wouldn't need to rely on the use_types field if MPI semantics allowed
     * us to test that btype=ftype=MPI_BYTE (or even MPI_TYPE_NULL, which
     * could mean "use MPI_BYTE" by convention).
     */
    use_types_this_time = file->use_types;
    if (use_types_this_time) {
	/* prepare for a full-blown xfer using btype, ftype, and disp */
	buf_type = file->btype;
	file_type = file->ftype;
	if (haddr_to_MPIOff(file->disp, &mpi_disp)<0) return -1;
    } else {
	/*
	 * Prepare for a simple xfer of a contiguous block of bytes.
	 * The btype, ftype, and disp fields are not used.
	 */
	buf_type = MPI_BYTE;
	file_type = MPI_BYTE;
	mpi_disp = 0;		/* mpi_off is sufficient */
    }

    /*
     * Don't bother to reset the view if we're not using the types this time,
     * and did we didn't use them last time either.
     */
    used_types_last_time = file->old_use_types;
    if (used_types_last_time ||	/* change to new ftype or MPI_BYTE */
	use_types_this_time) {	/* almost certainly a different ftype */
	/*OKAY: CAST DISCARDS CONST QUALIFIER*/
	mpierr = MPI_File_set_view(file->f, mpi_disp, MPI_BYTE, file_type,
				   (char*)"native", file->info);
	if (MPI_SUCCESS != mpierr) return -1;
    }
    
    /*
     * We always set the use_types flag to 0 because the default is not to
     * use types next time, unless someone explicitly requests it by setting
     * this flag to !=0.
     */
    file->old_use_types = use_types_this_time;
    file->use_types = 0;

    /* Write the data. */
    assert(H5FD_MPIO_INDEPENDENT==dx->xfer_mode ||
	   H5FD_MPIO_COLLECTIVE==dx->xfer_mode);
    if (H5FD_MPIO_INDEPENDENT==dx->xfer_mode) {
	/*OKAY: CAST DISCARDS CONST QUALIFIER*/
	mpierr = MPI_File_write_at(file->f, mpi_off, (void*)buf, size_i,
				   buf_type, &mpi_stat);
    } else {
#ifdef H5FDmpio_DEBUG
	if (H5FD_mpio_Debug[(int)'t'])
	    fprintf(stdout, "H5FD_mpio_write: using MPIO collective mode\n");
#endif
	/*OKAY: CAST DISCARDS CONST QUALIFIER*/
	mpierr = MPI_File_write_at_all(file->f, mpi_off, (void*)buf, size_i,
				       buf_type, &mpi_stat);
    }
    if (MPI_SUCCESS != mpierr) return -1;

    /* How many bytes were actually written? */
    mpierr = MPI_Get_count(&mpi_stat, MPI_BYTE, &bytes_written);
    if (MPI_SUCCESS!=mpierr) return -1;
#ifdef H5FDmpio_DEBUG
    if (H5FD_mpio_Debug[(int)'c'])
    	fprintf(stdout,
	    "In H5FD_mpio_write after Get_count size_i=%d bytes_written=%d\n",
	    size_i, bytes_written );
#endif
#if 1
    /*
     * KLUGE rky, 1998-02-02
     * MPI_Get_count incorrectly returns negative count; fake a complete
     * write.
     */
    bytes_written = size_i;
#endif
    if (bytes_written<0 || bytes_written>size_i) return -1;

    /* Forget the EOF value (see H5FD_mpio_get_eof()) --rpm 1999-08-06 */
    file->eof = HADDR_UNDEF;
    
 done:
#ifdef H5FDmpio_DEBUG
    if (H5FD_mpio_Debug[(int)'t'])
    	fprintf(stdout, "Leaving H5FD_mpio_write\n" );
#endif
    return 0;
}


/*-------------------------------------------------------------------------
 * Function:    H5FD_mpio_flush
 *
 * Purpose:     Makes sure that all data is on disk.
 *
 * Return:      Success:	Non-negative
 *
 * 		Failure:	Negative
 *
 * Programmer:  Unknown
 *              January 30, 1998
 *
 * Modifications:
 * 		Robb Matzke, 1998-02-18
 *		Added the ACCESS_PARMS argument.
 *
 * 		Robb Matzke, 1999-08-06
 *		Modified to work with the virtual file layer.
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_mpio_flush(H5FD_t *_file)
{
    H5FD_mpio_t		*file = (H5FD_mpio_t*)_file;
    int                 mpierr;

#ifdef H5FDmpio_DEBUG
    if (H5FD_mpio_Debug[(int)'t'])
    	fprintf(stdout, "Entering H5FD_mpio_flush\n" );
#endif

    mpierr = MPI_File_sync(file->f);
    if (MPI_SUCCESS != mpierr) return -1;

#ifdef H5FDmpio_DEBUG
    if (H5FD_mpio_Debug[(int)'t'])
    	fprintf(stdout, "Leaving H5FD_mpio_flush\n" );
#endif

    return 0;
}


/*-------------------------------------------------------------------------
 * Function:    MPIOff_to_haddr
 *
 * Purpose:     Convert an MPI_Offset value to haddr_t.
 *
 * Return:      Success:	The haddr_t equivalent of the MPI_OFF
 *				argument.
 *				
 *              Failure:	HADDR_UNDEF
 *
 * Programmer:  Unknown
 *              January 30, 1998
 *
 * Modifications:
 * 		Robb Matzke, 1999-04-23
 *		An error is reported for address overflows. The ADDR output
 *		argument is optional.
 *
 * 		Robb Matzke, 1999-08-06
 *		Modified to work with the virtual file layer.
 *------------------------------------------------------------------------- 
 */
static haddr_t
MPIOff_to_haddr(MPI_Offset mpi_off)
{
    if (mpi_off != (MPI_Offset)(haddr_t)mpi_off) return HADDR_UNDEF;
    return (haddr_t)mpi_off;
}


/*-------------------------------------------------------------------------
 * Function:    haddr_to_MPIOff
 *
 * Purpose:     Convert an haddr_t value to MPI_Offset.
 *
 * Return:      Success:	Non-negative, the MPI_OFF argument contains
 *				the converted value.
 *
 * 		Failure:	Negative, MPI_OFF is undefined.
 *
 * Programmer:  Unknown
 *              January 30, 1998
 *
 * Modifications:
 * 		Robb Matzke, 1999-04-23
 *		An error is reported for address overflows. The ADDR output
 *		argument is optional.
 *
 * 		Robb Matzke, 1999-07-28
 *		The ADDR argument is passed by value.
 *
 * 		Robb Matzke, 1999-08-06
 *		Modified to work with the virtual file layer.
 *-------------------------------------------------------------------------
 */
static herr_t
haddr_to_MPIOff(haddr_t addr, MPI_Offset *mpi_off/*out*/)
{
    if (mpi_off) *mpi_off = (MPI_Offset)addr;
    if (addr != (haddr_t)(MPI_Offset)addr) return -1;
    return 0;
}
#endif /*HAVE_PARALLEL*/
