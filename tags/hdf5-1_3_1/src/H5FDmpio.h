/*
 * Copyright � 1999 NCSA
 *                  All rights reserved.
 *
 * Programmer:  Robb Matzke <matzke@llnl.gov>
 *              Monday, August  2, 1999
 *
 * Purpose:	The public header file for the mpio driver.
 */
#ifndef H5FDmpio_H
#define H5FDmpio_H

#include <H5FDpublic.h>
#include <H5Ipublic.h>

#ifdef HAVE_PARALLEL
#   define H5FD_MPIO	(H5FD_mpio_init())
#else
#   define H5FD_MPIO	(-1)
#endif

/* Type of I/O for data transfer properties */
typedef enum H5FD_mpio_xfer_t {
    H5FD_MPIO_INDEPENDENT = 0, 		/*zero is the default*/
    H5FD_MPIO_COLLECTIVE
} H5FD_mpio_xfer_t;

/*
 * MPIO-specific data transfer properties. This struct is here only because
 * we need it in special case code throughout the library. Applications
 * please use H5Pset_dxpl_mpio() instead.
 */
typedef struct H5FD_mpio_dxpl_t {
    H5FD_mpio_xfer_t	xfer_mode;	/*collective or independent I/O	*/
} H5FD_mpio_dxpl_t;
    
/* Function prototypes */
#ifdef HAVE_PARALLEL
__DLL__ hid_t H5FD_mpio_init(void);
__DLL__ herr_t H5Pset_fapl_mpio(hid_t fapl_id, MPI_Comm comm, MPI_Info info);
__DLL__ herr_t H5Pget_fapl_mpio(hid_t fapl_id, MPI_Comm *comm/*out*/,
			MPI_Info *info/*out*/);
__DLL__ herr_t H5Pset_dxpl_mpio(hid_t dxpl_id, H5FD_mpio_xfer_t xfer_mode);
__DLL__ herr_t H5Pget_dxpl_mpio(hid_t dxpl_id, H5FD_mpio_xfer_t *xfer_mode/*out*/);
__DLL__ htri_t H5FD_mpio_tas_allsame(H5FD_t *_file, hbool_t newval);
__DLL__ MPI_Comm H5FD_mpio_communicator(H5FD_t *_file);
__DLL__ herr_t H5FD_mpio_setup(H5FD_t *_file, MPI_Datatype btype, MPI_Datatype ftype,
		       haddr_t disp, hbool_t use_types);
<<<<<<<<<<<<<< variant A
__DLL__herr_t H5FD_mpio_wait_for_left_neighbor(H5FD_t *file);
__DLL__herr_t H5FD_mpio_signal_right_neighbor(H5FD_t *file);
>>>>>>>>>>>>>> variant B
__DLL__ herr_t H5FD_mpio_wait_for_left_neighbor(H5FD_t *file);
__DLL__ herr_t H5FD_mpio_signal_right_neighbor(H5FD_t *file);
======= end of combination

#endif /*HAVE_PARALLEL*/

#endif
