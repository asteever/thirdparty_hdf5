/*
 * Copyright (c) 1999-2002 NCSA
 *                       All rights reserved.
 *
 * Programmer:  Robb Matzke <matzke@llnl.gov>
 *              Thursday, July 29, 1999
 *
 * Purpose:	This is the MPI-2 I/O driver.
 *
 * Limitations:
 *	H5FD_mpio_read
 *		One implementation of MPI/MPI-IO causes MPI_Get_count
 *		to return (incorrectly) a negative count. I (who?) added code
 *		to detect this, and a kludge to pretend that the number of
 *		bytes read is always equal to the number requested.  This
 *		kluge is activated by #ifdef MPI_KLUGE0202.
 */
#include "H5private.h"		/*library functions			*/
#include "H5ACprivate.h"        /* Metadata cache */
#include "H5Eprivate.h"		/*error handling			*/
#include "H5Fprivate.h"		/*files					*/
#include "H5FDprivate.h"	/*file driver				  */
#include "H5FDmpio.h"           /* MPI I/O file driver */
#include "H5Iprivate.h"		/*object IDs				  */
#include "H5MMprivate.h"        /* Memory allocation */
#include "H5Pprivate.h"		/*property lists			*/

#ifdef H5_HAVE_PARALLEL

/*
 * The driver identification number, initialized at runtime if H5_HAVE_PARALLEL
 * is defined. This allows applications to still have the H5FD_MPIO
 * "constants" in their source code (it also makes this file strictly ANSI
 * compliant when H5_HAVE_PARALLEL isn't defined)
 */
static hid_t H5FD_MPIO_g = 0;

/*
 * The description of a file belonging to this driver.
 * The EOF value is only used just after the file is opened in order for the
 * library to determine whether the file is empty, truncated, or okay. The MPIO
 * driver doesn't bother to keep it updated since it's an expensive operation.
 */
typedef struct H5FD_mpio_t {
    H5FD_t	pub;		/*public stuff, must be first		*/
    MPI_File	f;		/*MPIO file handle			*/
    MPI_Comm	comm;		/*communicator				*/
    MPI_Info	info;		/*file information			*/
    int         mpi_rank;       /* This process's rank                  */
    int         mpi_size;       /* Total number of processes            */
    int         mpi_round;      /* Current round robin process (for metadata I/O) */
    haddr_t	eof;		/*end-of-file marker			*/
    haddr_t	eoa;		/*end-of-address marker			*/
    haddr_t	last_eoa;	/* Last known end-of-address marker	*/
} H5FD_mpio_t;

/* Prototypes */
static haddr_t H5FD_mpio_MPIOff_to_haddr(MPI_Offset mpi_off);
static herr_t H5FD_mpio_haddr_to_MPIOff(haddr_t addr, MPI_Offset *mpi_off/*out*/);

/* Callbacks */
static void *H5FD_mpio_fapl_get(H5FD_t *_file);
static H5FD_t *H5FD_mpio_open(const char *name, unsigned flags, hid_t fapl_id,
			      haddr_t maxaddr);
static herr_t H5FD_mpio_close(H5FD_t *_file);
static herr_t H5FD_mpio_query(const H5FD_t *_f1, unsigned long *flags);
static haddr_t H5FD_mpio_get_eoa(H5FD_t *_file);
static herr_t H5FD_mpio_set_eoa(H5FD_t *_file, haddr_t addr);
static haddr_t H5FD_mpio_get_eof(H5FD_t *_file);
static herr_t  H5FD_mpio_get_handle(H5FD_t *_file, hid_t fapl, void** file_handle);
static herr_t H5FD_mpio_read(H5FD_t *_file, H5FD_mem_t type, hid_t dxpl_id, haddr_t addr,
            size_t size, void *buf);
static herr_t H5FD_mpio_write(H5FD_t *_file, H5FD_mem_t type, hid_t dxpl_id, haddr_t addr,
            size_t size, const void *buf);
static herr_t H5FD_mpio_flush(H5FD_t *_file, unsigned closing);

/* MPIO-specific file access properties */
typedef struct H5FD_mpio_fapl_t {
    MPI_Comm		comm;		/*communicator			*/
    MPI_Info		info;		/*file information		*/
} H5FD_mpio_fapl_t;

/* The MPIO file driver information */
static const H5FD_class_t H5FD_mpio_g = {
    "mpio",					/*name			*/
    HADDR_MAX,					/*maxaddr		*/
    H5F_CLOSE_SEMI,				/* fc_degree		*/
    NULL,					/*sb_size		*/
    NULL,					/*sb_encode		*/
    NULL,					/*sb_decode		*/
    sizeof(H5FD_mpio_fapl_t),			/*fapl_size		*/
    H5FD_mpio_fapl_get,				/*fapl_get		*/
    NULL,					/*fapl_copy		*/
    NULL, 					/*fapl_free		*/
    sizeof(H5FD_mpio_dxpl_t),			/*dxpl_size		*/
    NULL,					/*dxpl_copy		*/
    NULL,					/*dxpl_free		*/
    H5FD_mpio_open,				/*open			*/
    H5FD_mpio_close,				/*close			*/
    NULL,					/*cmp			*/
    H5FD_mpio_query,		                /*query			*/
    NULL,					/*alloc			*/
    NULL,					/*free			*/
    H5FD_mpio_get_eoa,				/*get_eoa		*/
    H5FD_mpio_set_eoa, 				/*set_eoa		*/
    H5FD_mpio_get_eof,				/*get_eof		*/
    H5FD_mpio_get_handle,                       /*get_handle            */
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

/* Interface initialization */
#define PABLO_MASK	H5FD_mpio_mask
#define INTERFACE_INIT	H5FD_mpio_init
static int interface_initialize_g = 0;

/* ======== Temporary, Local data transfer properties ======== */
/* Definitions for memory MPI type property */
#define H5FD_MPIO_XFER_MEM_MPI_TYPE_NAME        "H5FD_mpio_mem_mpi_type"
#define H5FD_MPIO_XFER_MEM_MPI_TYPE_SIZE        sizeof(MPI_Datatype)
/* Definitions for file MPI type property */
#define H5FD_MPIO_XFER_FILE_MPI_TYPE_NAME       "H5FD_mpio_file_mpi_type"
#define H5FD_MPIO_XFER_FILE_MPI_TYPE_SIZE       sizeof(MPI_Datatype)
/* Definitions for whether to use MPI types property */
#define H5FD_MPIO_XFER_USE_VIEW_NAME            "H5FD_mpio_use_view"
#define H5FD_MPIO_XFER_USE_VIEW_SIZE            sizeof(unsigned)


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
    hid_t ret_value=H5FD_MPIO_g;        /* Return value */

    FUNC_ENTER_NOAPI(H5FD_mpio_init, FAIL);

    if (H5I_VFL!=H5Iget_type(H5FD_MPIO_g))
        H5FD_MPIO_g = H5FDregister(&H5FD_mpio_g);

    /* Set return value */
    ret_value=H5FD_MPIO_g;

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5Pset_fapl_mpio
 *
 * Purpose:	Store the user supplied MPIO communicator COMM and INFO in
 *		the file access property list FAPL_ID which can then be used
 *		to create and/or open the file.  This function is available
 *		only in the parallel HDF5 library and is not collective.
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
 *
 *		Raymond Lu 
 * 		Tuesday, Oct 23, 2001
 *		Changed the file access list to the new generic property 
 *		list.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_fapl_mpio(hid_t fapl_id, MPI_Comm comm, MPI_Info info)
{
    H5FD_mpio_fapl_t	fa;
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t ret_value;
    
    FUNC_ENTER_API(H5Pset_fapl_mpio, FAIL);
    H5TRACE3("e","iMcMi",fapl_id,comm,info);

    if(fapl_id==H5P_DEFAULT)
        HGOTO_ERROR(H5E_PLIST, H5E_BADVALUE, FAIL, "can't set values in default property list");

    /* Check arguments */
    if(NULL == (plist = H5P_object_verify(fapl_id,H5P_FILE_ACCESS)))
        HGOTO_ERROR(H5E_PLIST, H5E_BADTYPE, FAIL, "not a file access list");

#ifdef LATER
#warning "We need to verify that COMM and INFO contain sensible information."
#endif

    /* Initialize driver specific properties */
    fa.comm = comm;
    fa.info = info;

    ret_value= H5P_set_driver(plist, H5FD_MPIO, &fa);

done:
    FUNC_LEAVE_API(ret_value);
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
 *	        Albert Cheng, Apr 16, 1998
 *	        Removed the access_mode argument.  The access_mode is changed
 *	        to be controlled by data transfer property list during data
 *	        read/write calls.
 *
 *		Raymond Lu 
 * 		Tuesday, Oct 23, 2001
 *		Changed the file access list to the new generic property 
 *		list.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_fapl_mpio(hid_t fapl_id, MPI_Comm *comm/*out*/, MPI_Info *info/*out*/)
{
    H5FD_mpio_fapl_t	*fa;
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t      ret_value=SUCCEED;       /* Return value */
    
    FUNC_ENTER_API(H5Pget_fapl_mpio, FAIL);
    H5TRACE3("e","ixx",fapl_id,comm,info);

    if(NULL == (plist = H5P_object_verify(fapl_id,H5P_FILE_ACCESS)))
        HGOTO_ERROR(H5E_PLIST, H5E_BADTYPE, FAIL, "not a file access list");
    if (H5FD_MPIO!=H5P_get_driver(plist))
        HGOTO_ERROR(H5E_PLIST, H5E_BADVALUE, FAIL, "incorrect VFL driver");
    if (NULL==(fa=H5P_get_driver_info(plist)))
        HGOTO_ERROR(H5E_PLIST, H5E_BADVALUE, FAIL, "bad VFL driver info");

    if (comm)
        *comm = fa->comm;
    if (info)
        *info = fa->info;

done:
    FUNC_LEAVE_API(ret_value);
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
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t ret_value;

    FUNC_ENTER_API(H5Pset_dxpl_mpio, FAIL);
    H5TRACE2("e","iDt",dxpl_id,xfer_mode);
    
    if(dxpl_id==H5P_DEFAULT)
        HGOTO_ERROR(H5E_PLIST, H5E_BADVALUE, FAIL, "can't set values in default property list");

    /* Check arguments */
    if(NULL == (plist = H5P_object_verify(dxpl_id,H5P_DATASET_XFER)))
        HGOTO_ERROR(H5E_PLIST, H5E_BADTYPE, FAIL, "not a dxpl");
    if (H5FD_MPIO_INDEPENDENT!=xfer_mode && H5FD_MPIO_COLLECTIVE!=xfer_mode)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "incorrect xfer_mode");

    /* Initialize driver-specific properties */
    dx.xfer_mode = xfer_mode;

    ret_value= H5P_set_driver(plist, H5FD_MPIO, &dx);

done:
    FUNC_LEAVE_API(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5Pget_dxpl_mpio
 *
 * Purpose:	Queries the transfer mode current set in the data transfer
 *		property list DXPL_ID. This is not collective.
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
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_API(H5Pget_dxpl_mpio, FAIL);
    H5TRACE2("e","ix",dxpl_id,xfer_mode);

    if(NULL == (plist = H5P_object_verify(dxpl_id,H5P_DATASET_XFER)))
        HGOTO_ERROR(H5E_PLIST, H5E_BADTYPE, FAIL, "not a dxpl");
    if (H5FD_MPIO!=H5P_get_driver(plist))
        HGOTO_ERROR(H5E_PLIST, H5E_BADVALUE, FAIL, "incorrect VFL driver");
    if (NULL==(dx=H5P_get_driver_info(plist)))
        HGOTO_ERROR(H5E_PLIST, H5E_BADVALUE, FAIL, "bad VFL driver info");

    if (xfer_mode)
        *xfer_mode = dx->xfer_mode;

done:
    FUNC_LEAVE_API(ret_value);
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
    MPI_Comm ret_value;         /* Return value */

    FUNC_ENTER_NOAPI(H5FD_mpio_communicator, MPI_COMM_NULL);

    assert(file);
    assert(H5FD_MPIO==file->pub.driver_id);

    /* Set return value */
    ret_value=file->comm;

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5FD_mpio_mpi_rank
 *
 * Purpose:	Returns the MPI rank for a process
 *
 * Return:	Success: non-negative
 *		Failure: negative
 *
 * Programmer:	Quincey Koziol
 *              Thursday, May 16, 2002
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
H5FD_mpio_mpi_rank(H5FD_t *_file)
{
    H5FD_mpio_t	*file = (H5FD_mpio_t*)_file;
    int ret_value;              /* Return value */

    FUNC_ENTER_NOAPI(H5FD_mpio_mpi_rank, FAIL);

    assert(file);
    assert(H5FD_MPIO==file->pub.driver_id);

    /* Set return value */
    ret_value=file->mpi_rank;

done:
    FUNC_LEAVE_NOAPI(ret_value);
} /* end H5FD_mpio_mpi_rank() */


/*-------------------------------------------------------------------------
 * Function:	H5FD_mpio_mpi_size
 *
 * Purpose:	Returns the number of MPI processes
 *
 * Return:	Success: non-negative
 *		Failure: negative
 *
 * Programmer:	Quincey Koziol
 *              Thursday, May 16, 2002
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
H5FD_mpio_mpi_size(H5FD_t *_file)
{
    H5FD_mpio_t	*file = (H5FD_mpio_t*)_file;
    int ret_value;              /* Return value */

    FUNC_ENTER_NOAPI(H5FD_mpio_mpi_rank, FAIL);

    assert(file);
    assert(H5FD_MPIO==file->pub.driver_id);

    /* Set return value */
    ret_value=file->mpi_size;

done:
    FUNC_LEAVE_NOAPI(ret_value);
} /* end H5FD_mpio_mpi_size() */


/*-------------------------------------------------------------------------
 * Function:	H5FD_mpio_setup
 *
 * Purpose:	Set the buffer type BTYPE, file type FTYPE for a data
 *		transfer. Also request a MPI type transfer.
 *
 * Return:	Success:	0
 *		Failure:	-1
 *
 * Programmer:	Robb Matzke
 *              Monday, August  9, 1999
 *
 * Modifications:
 *
 *              Quincey Koziol - 2002/06/17
 *              Removed 'disp' parameter, read & write routines will use
 *              the address of the dataset in MPI_File_set_view() calls, as
 *              necessary.
 *
 *              Quincey Koziol - 2002/06/17
 *              Changed to set temporary properties in a dxpl, instead of
 *              flags in the file struct, which will make this more threadsafe.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5FD_mpio_setup(hid_t dxpl_id, MPI_Datatype btype, MPI_Datatype ftype, unsigned use_view)
{
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5FD_mpio_setup, FAIL);

    /* Check arguments */
    if(NULL == (plist = H5P_object_verify(dxpl_id,H5P_DATASET_XFER)))
        HGOTO_ERROR(H5E_PLIST, H5E_BADTYPE, FAIL, "not a dataset transfer list");

    /* Set buffer MPI type */
    if(H5P_insert(plist,H5FD_MPIO_XFER_MEM_MPI_TYPE_NAME,H5FD_MPIO_XFER_MEM_MPI_TYPE_SIZE,&btype,NULL,NULL,NULL,NULL,NULL)<0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't insert MPI-I/O property");

    /* Set file MPI type */
    if(H5P_insert(plist,H5FD_MPIO_XFER_FILE_MPI_TYPE_NAME,H5FD_MPIO_XFER_FILE_MPI_TYPE_SIZE,&ftype,NULL,NULL,NULL,NULL,NULL)<0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't insert MPI-I/O property");

    /* Set 'use view' property */
    if(H5P_insert(plist,H5FD_MPIO_XFER_USE_VIEW_NAME,H5FD_MPIO_XFER_USE_VIEW_SIZE,&use_view,NULL,NULL,NULL,NULL,NULL)<0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't insert MPI-I/O property");

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5FD_mpio_teardown
 *
 * Purpose:	Remove the temporary MPI-I/O properties from dxpl.
 *
 * Return:	Success:        Non-negative
 *		Failure:	Negative
 *
 * Programmer:	Quincey Koziol
 *              Monday, June 17, 2002
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5FD_mpio_teardown(hid_t dxpl_id)
{
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5FD_mpio_teardown, FAIL);

    /* Check arguments */
    if(NULL == (plist = H5P_object_verify(dxpl_id,H5P_DATASET_XFER)))
        HGOTO_ERROR(H5E_PLIST, H5E_BADTYPE, FAIL, "not a dataset transfer list");

    /* Remove buffer MPI type */
    if(H5P_remove(dxpl_id,plist,H5FD_MPIO_XFER_MEM_MPI_TYPE_NAME)<0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTDELETE, FAIL, "can't remove MPI-I/O property");

    /* Remove file MPI type */
    if(H5P_remove(dxpl_id,plist,H5FD_MPIO_XFER_FILE_MPI_TYPE_NAME)<0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTDELETE, FAIL, "can't remove MPI-I/O property");

    /* Remove 'use view' property */
    if(H5P_remove(dxpl_id,plist,H5FD_MPIO_XFER_USE_VIEW_NAME)<0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTDELETE, FAIL, "can't remove MPI-I/O property");

done:
    FUNC_LEAVE_NOAPI(ret_value);
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
    char msgbuf[1];
    MPI_Status rcvstat;
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5FD_mpio_wait_for_left_neighbor, FAIL);

    assert(file);
    assert(H5FD_MPIO==file->pub.driver_id);

    /* Portably initialize MPI status variable */
    HDmemset(&rcvstat,0,sizeof(MPI_Status));

    /* p0 has no left neighbor; all other procs wait for msg */
    if (file->mpi_rank != 0) {
        if (MPI_SUCCESS!= MPI_Recv( &msgbuf, 1, MPI_CHAR, file->mpi_rank-1, MPI_ANY_TAG, file->comm, &rcvstat ))
            HGOTO_ERROR(H5E_INTERNAL, H5E_MPI, FAIL, "MPI_Recv failed");
    }
    
done:
    FUNC_LEAVE_NOAPI(ret_value);
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
    char msgbuf[1];
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5FD_mpio_signal_right_neighbor, FAIL);

    assert(file);
    assert(H5FD_MPIO==file->pub.driver_id);

    if (file->mpi_rank != (file->mpi_size-1)) {
        if (MPI_SUCCESS!= MPI_Send(&msgbuf, 0/*empty msg*/, MPI_CHAR, file->mpi_rank+1, 0, file->comm))
            HGOTO_ERROR(H5E_INTERNAL, H5E_MPI, FAIL, "MPI_Send failed");
    }

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5FD_mpio_fapl_get
 *
 * Purpose:	Returns a file access property list which could be used to
 *		create another file the same as this one.
 *
 * Return:	Success:	Ptr to new file access property list with all
 *				fields copied from the file pointer.
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *              Friday, August 13, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void *
H5FD_mpio_fapl_get(H5FD_t *_file)
{
    H5FD_mpio_t		*file = (H5FD_mpio_t*)_file;
    H5FD_mpio_fapl_t	*fa = NULL;
    void      *ret_value;       /* Return value */

    FUNC_ENTER_NOAPI(H5FD_mpio_fapl_get, NULL);

    assert(file);
    assert(H5FD_MPIO==file->pub.driver_id);

    if (NULL==(fa=H5MM_calloc(sizeof(H5FD_mpio_fapl_t))))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed");

    /* These should both be copied. --rpm, 1999-08-13 */
    fa->comm = file->comm;
    fa->info = file->info;

    /* Set return value */
    ret_value=fa;

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:    H5FD_mpio_open
 *
 * Purpose:     Opens a file with name NAME.  The FLAGS are a bit field with
 *		purpose similar to the second argument of open(2) and which
 *		are defined in H5Fpublic.h. The file access property list
 *		FAPL_ID contains the properties driver properties and MAXADDR
 *		is the largest address which this file will be expected to
 *		access.  This is collective.
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
 *
 *      	rky & ppw, 1999-11-07
 *		Modified "H5FD_mpio_open" so that file-truncation is
 *              avoided for brand-new files (with zero filesize).
 *-------------------------------------------------------------------------
 */
static H5FD_t *
H5FD_mpio_open(const char *name, unsigned flags, hid_t fapl_id,
	       haddr_t UNUSED maxaddr)
{
    H5FD_mpio_t			*file=NULL;
    MPI_File			fh;
    unsigned                    file_opened=0;  /* Flag to indicate that the file was successfully opened */
    int				mpi_amode;
    int				mpi_rank;       /* MPI rank of this process */
    int				mpi_size;       /* Total number of MPI processes */
    int				mpi_code;	/* mpi return code */
    MPI_Offset			size;
    const H5FD_mpio_fapl_t	*fa=NULL;
    H5FD_mpio_fapl_t		_fa;
    H5P_genplist_t *plist;      /* Property list pointer */
    H5FD_t			*ret_value;     /* Return value */

    FUNC_ENTER_NOAPI(H5FD_mpio_open, NULL);

#ifdef H5FDmpio_DEBUG
    if (H5FD_mpio_Debug[(int)'t']) {
    	fprintf(stdout, "Entering H5FD_mpio_open(name=\"%s\", flags=0x%x, "
		"fapl_id=%d, maxaddr=%lu)\n", name, flags, (int)fapl_id, (unsigned long)maxaddr);
    }
#endif

    /* Obtain a pointer to mpio-specific file access properties */
    if(NULL == (plist = H5P_object_verify(fapl_id,H5P_FILE_ACCESS)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a file access property list");
    if (H5P_FILE_ACCESS_DEFAULT==fapl_id || H5FD_MPIO!=H5P_get_driver(plist)) {
	_fa.comm = MPI_COMM_SELF; /*default*/
	_fa.info = MPI_INFO_NULL; /*default*/
	fa = &_fa;
    } else {
	fa = H5P_get_driver_info(plist);
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
#if 0
    /* Temporary KLUGE rky 2000-06-29, because fa->info is invalid (-1)*/
    {
	char debug_str[128];
        int infoerr, flag, i;
        if (fa->info) {
            infoerr = MPI_Info_get(fa->info, H5F_MPIO_DEBUG_KEY, 127,
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
    /* END Temporary KLUGE rky 2000-06-29, because fa->info is invalid (-1) */
#endif
#endif

    /*OKAY: CAST DISCARDS CONST*/
    if (MPI_SUCCESS != (mpi_code=MPI_File_open(fa->comm, (char*)name,
            mpi_amode, fa->info, &fh)))
        HMPI_GOTO_ERROR(NULL, "MPI_File_open failed", mpi_code);
    file_opened=1;

    /* Get the MPI rank of this process and the total number of processes */
    if (MPI_SUCCESS != (mpi_code=MPI_Comm_rank (fa->comm, &mpi_rank)))
        HMPI_GOTO_ERROR(NULL, "MPI_Comm_rank failed", mpi_code);
    if (MPI_SUCCESS != (mpi_code=MPI_Comm_size (fa->comm, &mpi_size)))
        HMPI_GOTO_ERROR(NULL, "MPI_Comm_size failed", mpi_code);

/*  Following changes in handling file-truncation made be rkyates and ppweidhaas, sep 99  */

    /* Only processor p0 will get the filesize and broadcast it. */
    if (mpi_rank == 0) {
        /* Get current file size */
        if (MPI_SUCCESS != (mpi_code=MPI_File_get_size(fh, &size)))
            HMPI_GOTO_ERROR(NULL, "MPI_File_get_size failed", mpi_code);
    }

    /* Broadcast file-size */
    if (MPI_SUCCESS != (mpi_code=MPI_Bcast(&size, sizeof(MPI_Offset), MPI_BYTE, 0, fa->comm)))
        HMPI_GOTO_ERROR(NULL, "MPI_Bcast failed", mpi_code);

    /* Only if size > 0, truncate the file - if requested */
    if (size && (flags & H5F_ACC_TRUNC)) {
        if (MPI_SUCCESS != (mpi_code=MPI_File_set_size(fh, (MPI_Offset)0)))
            HMPI_GOTO_ERROR(NULL, "MPI_File_set_size failed", mpi_code);

	/* Don't let any proc return until all have truncated the file. */
        if (MPI_SUCCESS!= (mpi_code=MPI_Barrier(fa->comm)))
            HMPI_GOTO_ERROR(NULL, "MPI_Barrier failed", mpi_code);
        size = 0;
    }

    /* Build the return value and initialize it */
    if (NULL==(file=H5MM_calloc(sizeof(H5FD_mpio_t))))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed");

    file->f = fh;
    file->comm = fa->comm;
    file->info = fa->info;
    file->mpi_rank = mpi_rank;
    file->mpi_size = mpi_size;
    file->eof = H5FD_mpio_MPIOff_to_haddr(size);

    /* Set return value */
    ret_value=(H5FD_t*)file;

done:
    if(ret_value==NULL) {
        if(file_opened)
            MPI_File_close(&fh);
    } /* end if */

#ifdef H5FDmpio_DEBUG
    if (H5FD_mpio_Debug[(int)'t'])
        fprintf(stdout, "Leaving H5FD_mpio_open\n" );
#endif
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:    H5FD_mpio_close
 *
 * Purpose:     Closes a file.  This is collective.
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
    int				mpi_code;	/* mpi return code */
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5FD_mpio_close, FAIL);

#ifdef H5FDmpio_DEBUG
    if (H5FD_mpio_Debug[(int)'t'])
    	fprintf(stdout, "Entering H5FD_mpio_close\n");
#endif
    assert(file);
    assert(H5FD_MPIO==file->pub.driver_id);

    /* MPI_File_close sets argument to MPI_FILE_NULL */
    if (MPI_SUCCESS != (mpi_code=MPI_File_close(&(file->f)/*in,out*/)))
        HMPI_GOTO_ERROR(FAIL, "MPI_File_close failed", mpi_code);

    /* Clean up other stuff */
    H5MM_xfree(file);

done:
#ifdef H5FDmpio_DEBUG
    if (H5FD_mpio_Debug[(int)'t'])
    	fprintf(stdout, "Leaving H5FD_mpio_close\n");
#endif
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5FD_mpio_query
 *
 * Purpose:	Set the flags that this VFL driver is capable of supporting.
 *              (listed in H5FDpublic.h)
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Friday, August 25, 2000
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_mpio_query(const H5FD_t UNUSED *_file, unsigned long *flags /* out */)
{
    herr_t ret_value=SUCCEED;

    FUNC_ENTER_NOAPI(H5FD_mpio_query, FAIL);

    /* Set the VFL feature flags that this driver supports */
    if(flags) {
        *flags=0;
        *flags|=H5FD_FEAT_AGGREGATE_METADATA; /* OK to aggregate metadata allocations */

        /* Distinguish between updating the metadata accumulator on writes and
         * reads.  This is particularly (perhaps only, even) important for MPI-I/O
         * where we guarantee that writes are collective, but reads may not be.
         * If we were to allow the metadata accumulator to be written during a
         * read operation, the application would hang.
         */
        *flags|=H5FD_FEAT_ACCUMULATE_METADATA_WRITE; /* OK to accumulate metadata for faster writes */

        *flags|=H5FD_FEAT_AGGREGATE_SMALLDATA; /* OK to aggregate "small" raw data allocations */
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value);
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
    haddr_t ret_value;          /* Return value */

    FUNC_ENTER_NOAPI(H5FD_mpio_get_eoa, HADDR_UNDEF);

    assert(file);
    assert(H5FD_MPIO==file->pub.driver_id);

    /* Set return value */
    ret_value=file->eoa;

done:
    FUNC_LEAVE_NOAPI(ret_value);
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
    herr_t ret_value=SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI(H5FD_mpio_set_eoa, FAIL);

    assert(file);
    assert(H5FD_MPIO==file->pub.driver_id);

    file->eoa = addr;

done:
    FUNC_LEAVE_NOAPI(ret_value);
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
    haddr_t ret_value;          /* Return value */

    FUNC_ENTER_NOAPI(H5FD_mpio_get_eof, HADDR_UNDEF);

    assert(file);
    assert(H5FD_MPIO==file->pub.driver_id);

    /* Set return value */
    ret_value=file->eof;

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:       H5FD_mpio_get_handle
 * 
 * Purpose:        Returns the file handle of MPIO file driver.
 * 
 * Returns:        Non-negative if succeed or negative if fails.
 * 
 * Programmer:     Raymond Lu
 *                 Sept. 16, 2002
 * 
 * Modifications:
 *
 *-------------------------------------------------------------------------
*/
static herr_t  
H5FD_mpio_get_handle(H5FD_t *_file, hid_t UNUSED fapl, void** file_handle)
{   
    H5FD_mpio_t         *file = (H5FD_mpio_t *)_file;
    herr_t              ret_value = SUCCEED;
                            
    FUNC_ENTER_NOAPI(H5FD_mpio_get_handle, FAIL);
                                    
    if(!file_handle)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "file handle not valid");

    *file_handle = &(file->f);

done:
    FUNC_LEAVE_NOAPI(ret_value);
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
 *
 *		Quincey Koziol,  2002-05-14
 *		Only call MPI_Get_count if we can use MPI_BYTE for the MPI type
 *              for the I/O transfer.  Someday we might include code to decode
 *              the MPI type used for more complicated transfers and call
 *              MPI_Get_count all the time.
 *
 *              Quincey Koziol - 2002/06/17
 *              Removed 'disp' parameter from H5FD_mpio_setup routine and use
 *              the address of the dataset in MPI_File_set_view() calls, as
 *              necessary.
 *
 *              Quincey Koziol - 2002/06/24
 *              Removed "lazy" MPI_File_set_view() calls, since they would fail
 *              if the first I/O was a collective I/O using MPI derived types
 *              and the next I/O was an independent I/O.
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_mpio_read(H5FD_t *_file, H5FD_mem_t UNUSED type, hid_t dxpl_id, haddr_t addr, size_t size,
	       void *buf/*out*/)
{
    H5FD_mpio_t			*file = (H5FD_mpio_t*)_file;
    const H5FD_mpio_dxpl_t	*dx=NULL;
    H5FD_mpio_dxpl_t		_dx;
    MPI_Offset			mpi_off, mpi_disp;
    MPI_Status  		mpi_stat;
    int				mpi_code;	/* mpi return code */
    MPI_Datatype		buf_type, file_type;
    int         		size_i, bytes_read, n;
    unsigned			use_view_this_time=0;
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t              	ret_value=SUCCEED;

    FUNC_ENTER_NOAPI(H5FD_mpio_read, FAIL);

#ifdef H5FDmpio_DEBUG
    if (H5FD_mpio_Debug[(int)'t'])
    	fprintf(stdout, "Entering H5FD_mpio_read\n" );
#endif
    assert(file);
    assert(H5FD_MPIO==file->pub.driver_id);
    /* Make certain we have the correct type of property list */
    assert(H5I_GENPROP_LST==H5I_get_type(dxpl_id));
    assert(TRUE==H5P_isa_class(dxpl_id,H5P_DATASET_XFER));
    assert(buf);

    /* Portably initialize MPI status variable */
    HDmemset(&mpi_stat,0,sizeof(MPI_Status));

    /* some numeric conversions */
    if (H5FD_mpio_haddr_to_MPIOff(addr, &mpi_off/*out*/)<0)
        HGOTO_ERROR(H5E_INTERNAL, H5E_BADRANGE, FAIL, "can't convert from haddr to MPI off");
    size_i = (int)size;
    if ((hsize_t)size_i != size)
        HGOTO_ERROR(H5E_INTERNAL, H5E_BADRANGE, FAIL, "can't convert from size to size_i");

#ifdef H5FDmpio_DEBUG
    if (H5FD_mpio_Debug[(int)'r'])
        fprintf(stdout, "in H5FD_mpio_read  mpi_off=%ld  size_i=%d\n",
		(long)mpi_off, size_i );
#endif

    /* Obtain the data transfer properties */
    if(NULL == (plist = H5I_object(dxpl_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file access property list");
    if (H5FD_MPIO!=H5P_get_driver(plist)) {
        _dx.xfer_mode = H5FD_MPIO_INDEPENDENT; /*the default*/
        dx = &_dx;
    } else {
        dx = H5P_get_driver_info(plist);
        assert(dx);
    }
    
    /*
     * Set up for a fancy xfer using complex types, or single byte block. We
     * wouldn't need to rely on the use_view field if MPI semantics allowed
     * us to test that btype=ftype=MPI_BYTE (or even MPI_TYPE_NULL, which
     * could mean "use MPI_BYTE" by convention).
     */
    if(H5P_exist_plist(plist,H5FD_MPIO_XFER_USE_VIEW_NAME)>0)
        if(H5P_get(plist,H5FD_MPIO_XFER_USE_VIEW_NAME,&use_view_this_time)<0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get MPI-I/O type property");

    if (use_view_this_time) {
        /* prepare for a full-blown xfer using btype, ftype, and disp */
        if(H5P_get(plist,H5FD_MPIO_XFER_MEM_MPI_TYPE_NAME,&buf_type)<0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get MPI-I/O type property");
        if(H5P_get(plist,H5FD_MPIO_XFER_FILE_MPI_TYPE_NAME,&file_type)<0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get MPI-I/O type property");

        /* When using types, use the address as the displacement for
         * MPI_File_set_view and reset the address for the read to zero
         */
        mpi_disp=mpi_off;
        mpi_off=0;
    } /* end if */
    else {
        /*
         * Prepare for a simple xfer of a contiguous block of bytes. The
         * btype, ftype, and disp fields are not used.
         */
        buf_type = MPI_BYTE;
        file_type = MPI_BYTE;
        mpi_disp = 0;		/* mpi_off is alread set */
    } /* end else */

    /*
     * Set the file view when we are using MPI derived types
     */
    if (use_view_this_time) {
        /*OKAY: CAST DISCARDS CONST QUALIFIER*/
        if (MPI_SUCCESS != (mpi_code=MPI_File_set_view(file->f, mpi_disp, MPI_BYTE, file_type, (char*)"native",  file->info)))
            HMPI_GOTO_ERROR(FAIL, "MPI_File_set_view failed", mpi_code);
    } /* end if */
    
    /* Read the data. */
    assert(H5FD_MPIO_INDEPENDENT==dx->xfer_mode || H5FD_MPIO_COLLECTIVE==dx->xfer_mode);
    if (H5FD_MPIO_INDEPENDENT==dx->xfer_mode) {
        if (MPI_SUCCESS!= (mpi_code=MPI_File_read_at(file->f, mpi_off, buf, size_i, buf_type, &mpi_stat)))
            HMPI_GOTO_ERROR(FAIL, "MPI_File_read_at failed", mpi_code);
    } else {
#ifdef H5FDmpio_DEBUG
	if (H5FD_mpio_Debug[(int)'t'])
	    fprintf(stdout, "H5FD_mpio_read: using MPIO collective mode\n");
#endif
        if (MPI_SUCCESS!= (mpi_code=MPI_File_read_at_all(file->f, mpi_off, buf, size_i, buf_type, &mpi_stat )))
            HMPI_GOTO_ERROR(FAIL, "MPI_File_read_at_all failed", mpi_code);
    }

    /* KLUDGE, Robb Matzke, 2000-12-29
     * The LAM implementation of MPI_Get_count() says
     *    MPI_Get_count: invalid argument (rank 0, MPI_COMM_WORLD)
     * So I'm commenting this out until it can be investigated. The
     * returned `bytes_written' isn't used anyway because of Kim's
     * kludge to avoid bytes_written<0. Likewise in H5FD_mpio_write(). */

#ifdef H5_HAVE_MPI_GET_COUNT /* Bill and Albert's kludge*/
    /* Yet Another KLUDGE, Albert Cheng & Bill Wendling, 2001-05-11.
     * Many systems don't support MPI_Get_count so we need to do a
     * configure thingy to fix this. */

    /* Calling MPI_Get_count with "MPI_BYTE" is only valid when we actually
     * had the 'buf_type' set to MPI_BYTE -QAK
     */
    if(use_view_this_time) {
        /* Figure out the mapping from the MPI 'buf_type' to bytes, someday...
         * If this gets fixed (and MPI_Get_count() is reliable), the
         * kludge below where the 'bytes_read' value from MPI_Get_count() is
         * overwritten with the 'size_i' parameter can be removed. -QAK
         */
    } /* end if */
    else {
        /* How many bytes were actually read? */
        if (MPI_SUCCESS != (mpi_code=MPI_Get_count(&mpi_stat, MPI_BYTE, &bytes_read)))
            HMPI_GOTO_ERROR(FAIL, "MPI_Get_count failed", mpi_code);
    } /* end else */
#ifdef H5FDmpio_DEBUG
    if (H5FD_mpio_Debug[(int)'c'])
    	fprintf(stdout,
	    "In H5FD_mpio_read after Get_count size_i=%d bytes_read=%d\n",
	    size_i, bytes_read );
#endif
#endif /* H5_HAVE_MPI_GET_COUNT */

    /*
     * KLUGE rky 1998-02-02
     * MPI_Get_count incorrectly returns negative count; fake a complete
     * read.
     */
    bytes_read = size_i;

    /* Check for read failure */
    if (bytes_read<0 || bytes_read>size_i)
        HGOTO_ERROR(H5E_IO, H5E_READERROR, FAIL, "file read failed");

    /*
     * Reset the file view when we used MPI derived types
     */
    if (use_view_this_time) {
        /*OKAY: CAST DISCARDS CONST QUALIFIER*/
        if (MPI_SUCCESS != (mpi_code=MPI_File_set_view(file->f, 0, MPI_BYTE, MPI_BYTE, (char*)"native",  file->info)))
            HMPI_GOTO_ERROR(FAIL, "MPI_File_set_view failed", mpi_code);
    } /* end if */
    
    /*
     * This gives us zeroes beyond end of physical MPI file.  What about
     * reading past logical end of HDF5 file???
     */
    if ((n=(size_i-bytes_read)) > 0) {
        if (use_view_this_time) {
            /*
             * INCOMPLETE rky 1998-09-18
             * Haven't implemented reading zeros beyond EOF. What to do???
             */
            HGOTO_ERROR(H5E_IO, H5E_READERROR, FAIL, "eof file read failed");
        } else {
            memset((char*)buf+bytes_read, 0, (size_t)n);
        }
    }

done:
#ifdef H5FDmpio_DEBUG
    if (H5FD_mpio_Debug[(int)'t'])
    	fprintf(stdout, "Leaving H5FD_mpio_read\n" );
#endif

    FUNC_LEAVE_NOAPI(ret_value);
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
 *
 *		Albert Cheng, 1999-12-19
 *		When only-p0-write-allsame-data, p0 Bcasts the
 *		ret_value to other processes.  This prevents
 *		a racing condition (that other processes try to
 *		read the file before p0 finishes writing) and also
 *		allows all processes to report the same ret_value.
 *
 *		Kim Yates, Pat Weidhaas,  2000-09-26
 *		Move block of coding where only p0 writes after the
 *              MPI_File_set_view call. 
 *
 *		Quincey Koziol,  2002-05-10
 *		Instead of always writing metadata from process 0, spread the
 *              burden among all the processes by using a round-robin rotation
 *              scheme.
 *
 *		Quincey Koziol,  2002-05-10
 *		Removed allsame code, keying off the type parameter instead.
 *
 *		Quincey Koziol,  2002-05-14
 *		Only call MPI_Get_count if we can use MPI_BYTE for the MPI type
 *              for the I/O transfer.  Someday we might include code to decode
 *              the MPI type used for more complicated transfers and call
 *              MPI_Get_count all the time.
 *
 *              Quincey Koziol - 2002/06/17
 *              Removed 'disp' parameter from H5FD_mpio_setup routine and use
 *              the address of the dataset in MPI_File_set_view() calls, as
 *              necessary.
 *
 *              Quincey Koziol - 2002/06/24
 *              Removed "lazy" MPI_File_set_view() calls, since they would fail
 *              if the first I/O was a collective I/O using MPI derived types
 *              and the next I/O was an independent I/O.
 *
 *              Quincey Koziol - 2002/07/18
 *              Added "block_before_meta_write" dataset transfer flag, which
 *              is set during writes from a metadata cache flush and indicates
 *              that all the processes must sync up before (one of them)
 *              writing metadata.
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_mpio_write(H5FD_t *_file, H5FD_mem_t type, hid_t dxpl_id, haddr_t addr,
		size_t size, const void *buf)
{
    H5FD_mpio_t			*file = (H5FD_mpio_t*)_file;
    const H5FD_mpio_dxpl_t	*dx=NULL;
    H5FD_mpio_dxpl_t		_dx;
    MPI_Offset 		 	mpi_off, mpi_disp;
    MPI_Status			mpi_stat;
    MPI_Datatype		buf_type, file_type;
    int			        mpi_code;	/* MPI return code */
    int         		size_i, bytes_written;
    unsigned			use_view_this_time=0;
    unsigned		        block_before_meta_write=0;      /* Whether to block before a metadata write */
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t              	ret_value=SUCCEED;

    FUNC_ENTER_NOAPI(H5FD_mpio_write, FAIL);

#ifdef H5FDmpio_DEBUG
    if (H5FD_mpio_Debug[(int)'t'])
    	fprintf(stdout, "Entering H5FD_mpio_write\n" );
#endif
    assert(file);
    assert(H5FD_MPIO==file->pub.driver_id);
    /* Make certain we have the correct type of property list */
    assert(H5I_GENPROP_LST==H5I_get_type(dxpl_id));
    assert(TRUE==H5P_isa_class(dxpl_id,H5P_DATASET_XFER));
    assert(buf);

    /* Portably initialize MPI status variable */
    HDmemset(&mpi_stat,0,sizeof(MPI_Status));

    /* some numeric conversions */
    if (H5FD_mpio_haddr_to_MPIOff(addr, &mpi_off)<0)
        HGOTO_ERROR(H5E_INTERNAL, H5E_BADRANGE, FAIL, "can't convert from haddr to MPI off");
    size_i = (int)size;
    if ((hsize_t)size_i != size)
        HGOTO_ERROR(H5E_INTERNAL, H5E_BADRANGE, FAIL, "can't convert from size to size_i");

#ifdef H5FDmpio_DEBUG
    if (H5FD_mpio_Debug[(int)'w'])
        fprintf(stdout, "in H5FD_mpio_write  mpi_off=%ld  size_i=%d\n",
                (long)mpi_off, size_i);
#endif

    /* Obtain the data transfer properties */
    if(NULL == (plist = H5I_object(dxpl_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file access property list");
    if (H5FD_MPIO!=H5P_get_driver(plist)) {
        _dx.xfer_mode = H5FD_MPIO_INDEPENDENT; /*the default*/
        dx = &_dx;
    } else {
        dx = H5P_get_driver_info(plist);
        assert(dx);
    }
    
    /*
     * Set up for a fancy xfer using complex types, or single byte block. We
     * wouldn't need to rely on the use_view field if MPI semantics allowed
     * us to test that btype=ftype=MPI_BYTE (or even MPI_TYPE_NULL, which
     * could mean "use MPI_BYTE" by convention).
     */
    if(H5P_exist_plist(plist,H5FD_MPIO_XFER_USE_VIEW_NAME)>0)
        if(H5P_get(plist,H5FD_MPIO_XFER_USE_VIEW_NAME,&use_view_this_time)<0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get MPI-I/O type property");

    if (use_view_this_time) {
        /* prepare for a full-blown xfer using btype, ftype, and disp */
        if(H5P_get(plist,H5FD_MPIO_XFER_MEM_MPI_TYPE_NAME,&buf_type)<0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get MPI-I/O type property");
        if(H5P_get(plist,H5FD_MPIO_XFER_FILE_MPI_TYPE_NAME,&file_type)<0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get MPI-I/O type property");

        /* When using types, use the address as the displacement for
         * MPI_File_set_view and reset the address for the read to zero
         */
        mpi_disp=mpi_off;
        mpi_off=0;
    } /* end if */
    else {
        /*
         * Prepare for a simple xfer of a contiguous block of bytes.
         * The btype, ftype, and disp fields are not used.
         */
        buf_type = MPI_BYTE;
        file_type = MPI_BYTE;
        mpi_disp = 0;		/* mpi_off is already set */
    } /* end else */

    /*
     * Set the file view when we are using MPI derived types
     */
    if (use_view_this_time) {
        /*OKAY: CAST DISCARDS CONST QUALIFIER*/
        if (MPI_SUCCESS != (mpi_code=MPI_File_set_view(file->f, mpi_disp, MPI_BYTE, file_type, (char*)"native", file->info)))
            HMPI_GOTO_ERROR(FAIL, "MPI_File_set_view failed", mpi_code);
    } /* end if */
    
    /* Metadata specific actions */
    if(type!=H5FD_MEM_DRAW) {
        /* Check if we need to syncronize all processes before attempting metadata write
         * (Prevents race condition where the process writing the metadata goes ahead
         * and writes the metadata to the file before all the processes have
         * read the data, "transmitting" data from the "future" to the reading
         * process. -QAK )
         */
        if(H5P_exist_plist(plist,H5AC_BLOCK_BEFORE_META_WRITE_NAME)>0)
            if(H5P_get(plist,H5AC_BLOCK_BEFORE_META_WRITE_NAME,&block_before_meta_write)<0)
                HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get H5AC property");

        if(block_before_meta_write)
            if (MPI_SUCCESS!= (mpi_code=MPI_Barrier(file->comm)))
                HMPI_GOTO_ERROR(FAIL, "MPI_Barrier failed", mpi_code);

        /* Only p<round> will do the actual write if all procs in comm write same metadata */
        if (H5_mpi_1_metawrite_g) {
            if (file->mpi_rank != file->mpi_round) {
#ifdef H5FDmpio_DEBUG
                if (H5FD_mpio_Debug[(int)'w']) {
                    fprintf(stdout,
                        "  proc %d: in H5FD_mpio_write (write omitted)\n",
                        file->mpi_rank );
                }
#endif
                HGOTO_DONE(SUCCEED) /* skip the actual write */
            }
        }
    } /* end if */

    /* Write the data. */
    assert(H5FD_MPIO_INDEPENDENT==dx->xfer_mode || H5FD_MPIO_COLLECTIVE==dx->xfer_mode);
    if (H5FD_MPIO_INDEPENDENT==dx->xfer_mode) {
        /*OKAY: CAST DISCARDS CONST QUALIFIER*/
        if (MPI_SUCCESS != (mpi_code=MPI_File_write_at(file->f, mpi_off, (void*)buf, size_i, buf_type, &mpi_stat)))
            HMPI_GOTO_ERROR(FAIL, "MPI_File_write_at failed", mpi_code);
    } else {
#ifdef H5FDmpio_DEBUG
        if (H5FD_mpio_Debug[(int)'t'])
            fprintf(stdout, "H5FD_mpio_write: using MPIO collective mode\n");
#endif
        /*OKAY: CAST DISCARDS CONST QUALIFIER*/
        if (MPI_SUCCESS != (mpi_code=MPI_File_write_at_all(file->f, mpi_off, (void*)buf, size_i, buf_type, &mpi_stat)))
            HMPI_GOTO_ERROR(FAIL, "MPI_File_write_at_all failed", mpi_code);
    }

    /* KLUDGE, Robb Matzke, 2000-12-29
     * The LAM implementation of MPI_Get_count() says
     *    MPI_Get_count: invalid argument (rank 0, MPI_COMM_WORLD)
     * So I'm commenting this out until it can be investigated. The
     * returned `bytes_written' isn't used anyway because of Kim's
     * kludge to avoid bytes_written<0. Likewise in H5FD_mpio_read(). */

#ifdef H5_HAVE_MPI_GET_COUNT /* Bill and Albert's kludge*/
    /* Yet Another KLUDGE, Albert Cheng & Bill Wendling, 2001-05-11.
     * Many systems don't support MPI_Get_count so we need to do a
     * configure thingy to fix this. */

    /* Calling MPI_Get_count with "MPI_BYTE" is only valid when we actually
     * had the 'buf_type' set to MPI_BYTE -QAK
     */
    if(use_view_this_time) {
        /* Figure out the mapping from the MPI 'buf_type' to bytes, someday...
         * If this gets fixed (and MPI_Get_count() is reliable), the
         * kludge below where the 'bytes_written' value from MPI_Get_count() is
         * overwritten with the 'size_i' parameter can be removed. -QAK
         */
    } /* end if */
    else {
        /* How many bytes were actually written? */
        if (MPI_SUCCESS!= (mpi_code=MPI_Get_count(&mpi_stat, MPI_BYTE, &bytes_written)))
            HMPI_GOTO_ERROR(FAIL, "MPI_Get_count failed", mpi_code);
    } /* end else */
#ifdef H5FDmpio_DEBUG
    if (H5FD_mpio_Debug[(int)'c'])
    	fprintf(stdout,
	    "In H5FD_mpio_write after Get_count size_i=%d bytes_written=%d\n",
	    size_i, bytes_written );
#endif
#endif /* H5_HAVE_MPI_GET_COUNT */

    /*
     * KLUGE rky, 1998-02-02
     * MPI_Get_count incorrectly returns negative count; fake a complete
     * write.
     */
    bytes_written = size_i;

    /* Check for write failure */
    if (bytes_written<0 || bytes_written>size_i)
        HGOTO_ERROR(H5E_IO, H5E_READERROR, FAIL, "file write failed");

    /*
     * Reset the file view when we used MPI derived types
     */
    if (use_view_this_time) {
        /*OKAY: CAST DISCARDS CONST QUALIFIER*/
        if (MPI_SUCCESS != (mpi_code=MPI_File_set_view(file->f, 0, MPI_BYTE, MPI_BYTE, (char*)"native",  file->info)))
            HMPI_GOTO_ERROR(FAIL, "MPI_File_set_view failed", mpi_code);
    } /* end if */
    
    /* Forget the EOF value (see H5FD_mpio_get_eof()) --rpm 1999-08-06 */
    file->eof = HADDR_UNDEF;
    
done:
    /* Guard against getting into metadate broadcast in failure cases */
    if(ret_value!=FAIL) {
        /* if only p<round> writes, need to broadcast the ret_value to other processes */
        if ((type!=H5FD_MEM_DRAW) && H5_mpi_1_metawrite_g) {
            if (MPI_SUCCESS != (mpi_code=MPI_Bcast(&ret_value, sizeof(ret_value), MPI_BYTE, file->mpi_round, file->comm)))
                HMPI_GOTO_ERROR(FAIL, "MPI_Bcast failed", mpi_code);

            /* Round-robin rotate to the next process */
            file->mpi_round = (++file->mpi_round)%file->mpi_size;
        } /* end if */
    } /* end if */

#ifdef H5FDmpio_DEBUG
    if (H5FD_mpio_Debug[(int)'t'])
    	fprintf(stdout, "proc %d: Leaving H5FD_mpio_write with ret_value=%d\n",
	    file->mpi_rank, ret_value );
#endif
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:    H5FD_mpio_flush
 *
 * Purpose:     Makes sure that all data is on disk.  This is collective.
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
 *
 *              Robb Matzke, 2000-12-29
 *              Make sure file size is at least as large as the last
 *              allocated byte.
 *
 *              Quincey Koziol, 2002-06-??
 *              Changed file extension method to use MPI_File_set_size instead 
 *              read->write method.
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD_mpio_flush(H5FD_t *_file, unsigned closing)
{
    H5FD_mpio_t		*file = (H5FD_mpio_t*)_file;
    int			mpi_code;	/* mpi return code */
    MPI_Offset          mpi_off;
    herr_t              ret_value=SUCCEED;
#ifdef OLD_WAY
    uint8_t             byte=0;
    MPI_Status          mpi_stat;
#endif  /* OLD_WAY */

    FUNC_ENTER_NOAPI(H5FD_mpio_flush, FAIL);

#ifdef H5FDmpio_DEBUG
    if (H5FD_mpio_Debug[(int)'t'])
    	fprintf(stdout, "Entering H5FD_mpio_flush\n" );
#endif
    assert(file);
    assert(H5FD_MPIO==file->pub.driver_id);

#ifdef OLD_WAY
    /* Portably initialize MPI status variable */
    HDmemset(&mpi_stat,0,sizeof(MPI_Status));
#endif /* OLD_WAY */

    /* Extend the file to make sure it's large enough, then sync.
     * Unfortunately, keeping track of EOF is an expensive operation, so
     * we can't just check whether EOF<EOA like with other drivers.
     * Therefore we'll just read the byte at EOA-1 and then write it back. */
    if(file->eoa>file->last_eoa) {
#ifdef OLD_WAY
        if (0==file->mpi_rank) {
            if (H5FD_mpio_haddr_to_MPIOff(file->eoa-1, &mpi_off)<0)
                HGOTO_ERROR(H5E_INTERNAL, H5E_BADRANGE, FAIL, "cannot convert from haddr_t to MPI_Offset");
            if (MPI_SUCCESS != (mpi_code=MPI_File_read_at(file->f, mpi_off, &byte, 1, MPI_BYTE, &mpi_stat)))
                HMPI_GOTO_ERROR(FAIL, "MPI_File_read_at failed", mpi_code);
            if (MPI_SUCCESS != (mpi_code=MPI_File_write_at(file->f, mpi_off, &byte, 1, MPI_BYTE, &mpi_stat)))
                HMPI_GOTO_ERROR(FAIL, "MPI_File_write_at failed", mpi_code);
        } /* end if */
#else /* OLD_WAY */
        if (H5FD_mpio_haddr_to_MPIOff(file->eoa, &mpi_off)<0)
            HGOTO_ERROR(H5E_INTERNAL, H5E_BADRANGE, FAIL, "cannot convert from haddr_t to MPI_Offset");

        /* Extend the file's size */
        if (MPI_SUCCESS != (mpi_code=MPI_File_set_size(file->f, mpi_off)))
            HMPI_GOTO_ERROR(FAIL, "MPI_File_set_size failed", mpi_code);

	/* Don't let any proc return until all have extended the file.
         * (Prevents race condition where some processes go ahead and write
         * more data to the file before all the processes have finished making
         * it the shorter length, potentially truncating the file and dropping
         * the new data written)
         */
        if (MPI_SUCCESS!= (mpi_code=MPI_Barrier(file->comm)))
            HMPI_GOTO_ERROR(FAIL, "MPI_Barrier failed", mpi_code);
#endif /* OLD_WAY */

        /* Update the 'last' eoa value */
        file->last_eoa=file->eoa;
    } /* end if */

    /* Only sync the file if we are not going to immediately close it */
    if(!closing) {
        if (MPI_SUCCESS != (mpi_code=MPI_File_sync(file->f)))
            HMPI_GOTO_ERROR(FAIL, "MPI_File_sync failed", mpi_code);
    } /* end if */

done:
#ifdef H5FDmpio_DEBUG
    if (H5FD_mpio_Debug[(int)'t'])
    	fprintf(stdout, "Leaving H5FD_mpio_flush\n" );
#endif

    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:    H5FD_mpio_MPIOff_to_haddr
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
H5FD_mpio_MPIOff_to_haddr(MPI_Offset mpi_off)
{
    haddr_t ret_value=HADDR_UNDEF;

    FUNC_ENTER_NOINIT(H5FD_mpio_MPIOff_to_haddr);

    if (mpi_off != (MPI_Offset)(haddr_t)mpi_off)
        ret_value=HADDR_UNDEF;
    else
        ret_value=(haddr_t)mpi_off;

    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:    H5FD_mpio_haddr_to_MPIOff
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
H5FD_mpio_haddr_to_MPIOff(haddr_t addr, MPI_Offset *mpi_off/*out*/)
{
    herr_t ret_value=FAIL;

    FUNC_ENTER_NOINIT(H5FD_mpio_haddr_to_MPIOff);

    if (mpi_off)
        *mpi_off = (MPI_Offset)addr;
    if (addr != (haddr_t)(MPI_Offset)addr)
        ret_value=FAIL;
    else
        ret_value=SUCCEED;

    FUNC_LEAVE_NOAPI(ret_value);
}
#endif /* H5_HAVE_PARALLEL */
