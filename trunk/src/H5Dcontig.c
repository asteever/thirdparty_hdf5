/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdf.ncsa.uiuc.edu/HDF5/doc/Copyright.html.  If you do not have     *
 * access to either file, you may request a copy from hdfhelp@ncsa.uiuc.edu. *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * Programmer: 	Quincey Koziol <koziol@ncsa.uiuc.edu>
 *	       	Thursday, September 28, 2000
 *
 * Purpose:
 *      Contiguous dataset I/O functions. These routines are similar to
 *      the H5D_istore_* routines and really only an abstract way of dealing
 *      with the data sieve buffer from H5F_seq_read/write.
 */

#define H5D_PACKAGE		/*suppress error about including H5Dpkg	  */

/* Pablo information */
/* (Put before include files to avoid problems with inline functions) */
#define PABLO_MASK	H5Dcontig_mask

#include "H5private.h"		/* Generic Functions			*/
#include "H5Dpkg.h"		/* Dataset functions			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5Fprivate.h"		/* Files				*/
#include "H5FDprivate.h"	/* File drivers				*/
#include "H5FLprivate.h"	/* Free Lists                           */
#include "H5MFprivate.h"	/* File memory management		*/
#include "H5Oprivate.h"		/* Object headers		  	*/
#include "H5Pprivate.h"		/* Property lists			*/
#include "H5Sprivate.h"		/* Dataspace functions			*/
#include "H5Vprivate.h"		/* Vector and array functions		*/

/* Private prototypes */
static herr_t H5D_contig_write(H5F_t *f, hid_t dxpl_id, H5D_t *dset,
    hsize_t offset, size_t size, const void *buf);

/* Declare a PQ free list to manage the sieve buffer information */
H5FL_BLK_DEFINE(sieve_buf);

/* Declare the free list to manage blocks of non-zero fill-value data */
H5FL_BLK_DEFINE_STATIC(non_zero_fill);

/* Declare the free list to manage blocks of zero fill-value data */
H5FL_BLK_DEFINE_STATIC(zero_fill);


/*-------------------------------------------------------------------------
 * Function:	H5D_contig_create
 *
 * Purpose:	Allocate file space for a contiguously stored dataset
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		April 19, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5D_contig_create(H5F_t *f, hid_t dxpl_id, H5D_t *dset)
{
    herr_t ret_value=SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI(H5D_contig_create, FAIL);

    /* check args */
    assert(f);
    assert(dset);

    /* Allocate space for the contiguous data */
    if (HADDR_UNDEF==(dset->layout.u.contig.addr=H5MF_alloc(f, H5FD_MEM_DRAW, dxpl_id, dset->layout.u.contig.size)))
        HGOTO_ERROR (H5E_IO, H5E_NOSPACE, FAIL, "unable to reserve file space");

done:
    FUNC_LEAVE_NOAPI(ret_value);
} /* end H5D_contig_create */


/*-------------------------------------------------------------------------
 * Function:	H5D_contig_fill
 *
 * Purpose:	Write fill values to a contiguously stored dataset.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		August 22, 2002
 *
 * Modifications:
 *          Bill Wendling, February 20, 2003
 *          Added support for getting the barrier COMM if you're using
 *          Flexible PHDF5.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5D_contig_fill(H5F_t *f, hid_t dxpl_id, H5D_t *dset)
{
    hssize_t    snpoints;       /* Number of points in space (for error checking) */
    size_t      npoints;        /* Number of points in space */
    size_t      ptsperbuf;      /* Maximum # of points which fit in the buffer */
    size_t      elmt_size;      /* Size of each element */
    size_t	bufsize=64*1024; /* Size of buffer to write */
    size_t	size;           /* Current # of points to write */
    hsize_t	offset;         /* Offset of dataset */
    void       *buf = NULL;     /* Buffer for fill value writing */
#ifdef H5_HAVE_PARALLEL
    MPI_Comm	mpi_comm=MPI_COMM_NULL;	/* MPI communicator for file */
    int         mpi_rank=(-1);  /* This process's rank  */
    int         mpi_code;       /* MPI return code */
    unsigned    blocks_written=0; /* Flag to indicate that chunk was actually written */
    unsigned    using_mpi=0;    /* Flag to indicate that the file is being accessed with an MPI-capable file driver */
#endif /* H5_HAVE_PARALLEL */
    int         non_zero_fill_f=(-1);   /* Indicate that a non-zero fill-value was used */
    herr_t	ret_value=SUCCEED;	/* Return value */
    
    FUNC_ENTER_NOAPI(H5D_contig_fill, FAIL);

    /* Check args */
    assert(f);
    assert(TRUE==H5P_isa_class(dxpl_id,H5P_DATASET_XFER));
    assert(dset && H5D_CONTIGUOUS==dset->layout.type);
    assert(H5F_addr_defined(dset->layout.u.contig.addr));
    assert(dset->layout.u.contig.size>0);
    assert(dset->space);

#ifdef H5_HAVE_PARALLEL
    /* Retrieve MPI parameters */
    if(IS_H5FD_MPI(f)) {
        /* Get the MPI communicator */
        if (MPI_COMM_NULL == (mpi_comm=H5F_mpi_get_comm(f)))
            HGOTO_ERROR(H5E_INTERNAL, H5E_MPI, FAIL, "Can't retrieve MPI communicator");

        /* Get the MPI rank & size */
        if ((mpi_rank=H5F_mpi_get_rank(f))<0)
            HGOTO_ERROR(H5E_INTERNAL, H5E_MPI, FAIL, "Can't retrieve MPI rank");

        /* Set the MPI-capable file driver flag */
        using_mpi=1;
    } /* end if */
#endif  /* H5_HAVE_PARALLEL */

    /* Get size of elements */
    elmt_size=H5T_get_size(dset->type);
    assert(elmt_size>0);

    /* Get the number of elements in the dataset's dataspace */
    snpoints = H5S_GET_EXTENT_NPOINTS(dset->space);
    assert(snpoints>=0);
    H5_ASSIGN_OVERFLOW(npoints,snpoints,hssize_t,size_t);

    /* If fill value is not library default, use it to set the element size */
    if(dset->fill.buf)
        elmt_size=dset->fill.size;

    /*
     * Fill the entire current extent with the fill value.  We can do
     * this quite efficiently by making sure we copy the fill value
     * in relatively large pieces.
     */
    ptsperbuf = MAX(1, bufsize/elmt_size);
    bufsize = ptsperbuf*elmt_size;

    /* Fill the buffer with the user's fill value */
    if(dset->fill.buf) {
        /* Allocate temporary buffer */
        if ((buf=H5FL_BLK_MALLOC(non_zero_fill,bufsize))==NULL)
            HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for fill buffer");

        H5V_array_fill(buf, dset->fill.buf, elmt_size, ptsperbuf);

        /* Indicate that a non-zero fill buffer was used */
        non_zero_fill_f=1;
    } /* end if */
    else {      /* Fill the buffer with the default fill value */
        htri_t buf_avail;

        /* Check if there is an already zeroed out buffer available */
        buf_avail=H5FL_BLK_AVAIL(zero_fill,bufsize);
        assert(buf_avail!=FAIL);

        /* Allocate temporary buffer (zeroing it if no buffer is available) */
        if(!buf_avail)
            buf=H5FL_BLK_CALLOC(zero_fill,bufsize);
        else
            buf=H5FL_BLK_MALLOC(zero_fill,bufsize);
        if(buf==NULL)
            HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for fill buffer");

        /* Indicate that a zero fill buffer was used */
        non_zero_fill_f=0;
    } /* end else */
     
    /* Start at the beginning of the dataset */
    offset = 0;

    /* Loop through writing the fill value to the dataset */
    while (npoints>0) {
          size = MIN(ptsperbuf, npoints) * elmt_size;

#ifdef H5_HAVE_PARALLEL
            /* Check if this file is accessed with an MPI-capable file driver */
            if(using_mpi) {
                /* Write the chunks out from only one process */
                /* !! Use the internal "independent" DXPL!! -QAK */
                if(H5_PAR_META_WRITE==mpi_rank) {
                    if (H5D_contig_write(f, H5AC_ind_dxpl_id, dset, offset, size, buf)<0)
                        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to write fill value to dataset");
                } /* end if */

                /* Indicate that blocks are being written */
                blocks_written=1;
            } /* end if */
            else {
#endif /* H5_HAVE_PARALLEL */
                H5_CHECK_OVERFLOW(size,size_t,hsize_t);
                if (H5D_contig_write(f, dxpl_id, dset, offset, size, buf)<0)
                    HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to write fill value to dataset");
#ifdef H5_HAVE_PARALLEL
            } /* end else */
#endif /* H5_HAVE_PARALLEL */

          npoints -= MIN(ptsperbuf, npoints);
          offset += size;
      } /* end while */
            
#ifdef H5_HAVE_PARALLEL
    /* Only need to block at the barrier if we actually wrote fill values */
    /* And if we are using an MPI-capable file driver */
    if(using_mpi && blocks_written) {
        /* Wait at barrier to avoid race conditions where some processes are
         * still writing out fill values and other processes race ahead to data
         * in, getting bogus data.
         */
        if (MPI_SUCCESS != (mpi_code=MPI_Barrier(mpi_comm)))
            HMPI_GOTO_ERROR(FAIL, "MPI_Barrier failed", mpi_code);
    } /* end if */
#endif /* H5_HAVE_PARALLEL */

done:
    /* Free the buffer for fill values */
    if (buf) {
        assert(non_zero_fill_f>=0);
        if(non_zero_fill_f)
            H5FL_BLK_FREE(non_zero_fill,buf);
        else
            H5FL_BLK_FREE(zero_fill,buf);
    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value);
} /* end H5D_contig_fill() */


/*-------------------------------------------------------------------------
 * Function:	H5D_contig_delete
 *
 * Purpose:	Delete the file space for a contiguously stored dataset
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		March 20, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5D_contig_delete(H5F_t *f, hid_t dxpl_id, const struct H5O_layout_t *layout)
{
    herr_t ret_value=SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI(H5D_contig_delete, FAIL);

    /* check args */
    assert(f);
    assert(layout);

    /* Free the file space for the chunk */
    if (H5MF_xfree(f, H5FD_MEM_DRAW, dxpl_id, layout->u.contig.addr, layout->u.contig.size)<0)
        HGOTO_ERROR(H5E_OHDR, H5E_CANTFREE, FAIL, "unable to free object header");

done:
    FUNC_LEAVE_NOAPI(ret_value);
} /* end H5D_contig_delete */


/*-------------------------------------------------------------------------
 * Function:	H5D_contig_get_addr
 *
 * Purpose:	Get the offset of the contiguous data on disk
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		June  2, 2004
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
haddr_t
H5D_contig_get_addr(const H5D_t *dset)
{
    FUNC_ENTER_NOAPI_NOFUNC(H5D_contig_get_addr);

    /* check args */
    assert(dset);
    assert(dset->layout.type==H5D_CONTIGUOUS);

    FUNC_LEAVE_NOAPI(dset->layout.u.contig.addr);
} /* end H5D_contig_get_addr */


/*-------------------------------------------------------------------------
 * Function:	H5D_contig_write
 *
 * Purpose:	Writes some data from a dataset into a buffer.
 *		The data is contiguous.	 The address is relative to the base
 *		address for the file.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              Thursday, September 28, 2000
 *
 * Modifications:
 *              Re-written in terms of the new writevv call, QAK, 5/7/03
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D_contig_write(H5F_t *f, hid_t dxpl_id, H5D_t *dset,
    hsize_t offset, size_t size, const void *buf)
{
    hsize_t dset_off=offset;    /* Offset in dataset */
    size_t dset_len=size;       /* Length in dataset */
    size_t dset_curr_seq=0;     /* "Current sequence" in dataset */
    hsize_t mem_off=0;          /* Offset in memory */
    size_t mem_len=size;        /* Length in memory */
    size_t mem_curr_seq=0;      /* "Current sequence" in memory */
    herr_t ret_value=SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI(H5D_contig_write, FAIL);

    assert (f);
    assert (dset);
    assert (buf);

    if (H5D_contig_writevv(f, dxpl_id, dset, dset->layout.u.contig.addr, dset->layout.u.contig.size,
            1, &dset_curr_seq, &dset_len, &dset_off, 1, &mem_curr_seq, &mem_len, &mem_off, buf)<0)
        HGOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, "vector write failed");

done:
    FUNC_LEAVE_NOAPI(ret_value);
}   /* end H5D_contig_write() */


/*-------------------------------------------------------------------------
 * Function:	H5D_contig_readvv
 *
 * Purpose:	Reads some data vectors from a dataset into a buffer.
 *		The data is contiguous.	 The address is the start of the dataset,
 *              relative to the base address for the file and the offsets and
 *              sequence lengths are in bytes.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              Friday, May 3, 2001
 *
 * Notes:
 *      Offsets in the sequences must be monotonically increasing
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
ssize_t
H5D_contig_readvv(H5F_t *f, hid_t dxpl_id, H5D_t *dset,
    haddr_t dset_addr, hsize_t dset_size,
    size_t dset_max_nseq, size_t *dset_curr_seq, size_t dset_len_arr[], hsize_t dset_offset_arr[],
    size_t mem_max_nseq, size_t *mem_curr_seq, size_t mem_len_arr[], hsize_t mem_offset_arr[],
    void *_buf)
{
    unsigned char *buf=(unsigned char *)_buf;      /* Pointer to buffer to fill */
    haddr_t abs_eoa;	        /* Absolute end of file address		*/
    haddr_t rel_eoa;	        /* Relative end of file address		*/
    haddr_t addr;               /* Actual address to read */
    hsize_t max_data;           /* Actual maximum size of data to cache */
    size_t size;                /* Size of sequence in bytes */
    size_t u;                   /* Counting variable */
    size_t v;                   /* Counting variable */
    ssize_t ret_value=0;        /* Return value */
   
    FUNC_ENTER_NOAPI(H5D_contig_readvv, FAIL);

    /* Check args */
    assert(f);
    assert(dset);
    assert(buf);

    /* Check if data sieving is enabled */
    if(H5F_HAS_FEATURE(f,H5FD_FEAT_DATA_SIEVE)) {
        haddr_t sieve_start, sieve_end;     /* Start & end locations of sieve buffer */
        haddr_t contig_end;             /* End locations of block to write */
        size_t sieve_size;              /* size of sieve buffer */

        /* Set offsets in sequence lists */
        u=*dset_curr_seq;
        v=*mem_curr_seq;

        /* No data sieve buffer yet, go allocate one */
        if(dset->cache.contig.sieve_buf==NULL) {
            /* Choose smallest buffer to write */
            if(mem_len_arr[v]<dset_len_arr[u])
                size=mem_len_arr[v];
            else
                size=dset_len_arr[u];

            /* Compute offset on disk */
            addr=dset_addr+dset_offset_arr[u];

            /* Compute offset in memory */
            buf = (unsigned char *)_buf + mem_offset_arr[v];

            /* Set up the buffer parameters */
            max_data=dset_size-dset_offset_arr[u];

            /* Check if we can actually hold the I/O request in the sieve buffer */
            if(size>dset->cache.contig.sieve_buf_size) {
                if (H5F_block_read(f, H5FD_MEM_DRAW, addr, size, dxpl_id, buf)<0)
                    HGOTO_ERROR(H5E_IO, H5E_READERROR, FAIL, "block read failed");
            } /* end if */
            else {
                /* Allocate room for the data sieve buffer */
                if (NULL==(dset->cache.contig.sieve_buf=H5FL_BLK_MALLOC(sieve_buf,dset->cache.contig.sieve_buf_size)))
                    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

                /* Determine the new sieve buffer size & location */
                dset->cache.contig.sieve_loc=addr;

                /* Make certain we don't read off the end of the file */
                if (HADDR_UNDEF==(abs_eoa=H5F_get_eoa(f)))
                    HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, FAIL, "unable to determine file size");

                /* Adjust absolute EOA address to relative EOA address */
                rel_eoa=abs_eoa-H5F_get_base_addr(f);

                /* Compute the size of the sieve buffer */
                H5_ASSIGN_OVERFLOW(dset->cache.contig.sieve_size,MIN3(rel_eoa-dset->cache.contig.sieve_loc,max_data,dset->cache.contig.sieve_buf_size),hsize_t,size_t);

                /* Read the new sieve buffer */
                if (H5F_block_read(f, H5FD_MEM_DRAW, dset->cache.contig.sieve_loc, dset->cache.contig.sieve_size, dxpl_id, dset->cache.contig.sieve_buf)<0)
                    HGOTO_ERROR(H5E_IO, H5E_READERROR, FAIL, "block read failed");

                /* Grab the data out of the buffer (must be first piece of data in buffer ) */
                HDmemcpy(buf,dset->cache.contig.sieve_buf,size);

                /* Reset sieve buffer dirty flag */
                dset->cache.contig.sieve_dirty=0;
            } /* end else */

            /* Update memory information */
            mem_len_arr[v]-=size;
            mem_offset_arr[v]+=size;
            if(mem_len_arr[v]==0)
                v++;

            /* Update file information */
            dset_len_arr[u]-=size;
            dset_offset_arr[u]+=size;
            if(dset_len_arr[u]==0)
                u++;

            /* Increment number of bytes copied */
            ret_value+=size;
        } /* end if */

        /* Stash local copies of these value */
        sieve_start=dset->cache.contig.sieve_loc;
        sieve_size=dset->cache.contig.sieve_size;
        sieve_end=sieve_start+sieve_size;
        
        /* Works through sequences as fast as possible */
        for(; u<dset_max_nseq && v<mem_max_nseq; ) {
            /* Choose smallest buffer to write */
            if(mem_len_arr[v]<dset_len_arr[u])
                size=mem_len_arr[v];
            else
                size=dset_len_arr[u];

            /* Compute offset on disk */
            addr=dset_addr+dset_offset_arr[u];

            /* Compute offset in memory */
            buf = (unsigned char *)_buf + mem_offset_arr[v];

            /* Compute end of sequence to retrieve */
            contig_end=addr+size-1;

            /* If entire read is within the sieve buffer, read it from the buffer */
            if(addr>=sieve_start && contig_end<sieve_end) {
                unsigned char *base_sieve_buf=dset->cache.contig.sieve_buf+(addr-sieve_start);

                /* Grab the data out of the buffer */
                HDmemcpy(buf,base_sieve_buf,size);
            } /* end if */
            /* Entire request is not within this data sieve buffer */
            else {
                /* Check if we can actually hold the I/O request in the sieve buffer */
                if(size>dset->cache.contig.sieve_buf_size) {
                    /* Check for any overlap with the current sieve buffer */
                    if((sieve_start>=addr && sieve_start<(contig_end+1))
                            || ((sieve_end-1)>=addr && (sieve_end-1)<(contig_end+1))) {
                        /* Flush the sieve buffer, if it's dirty */
                        if(dset->cache.contig.sieve_dirty) {
                            /* Write to file */
                            if (H5F_block_write(f, H5FD_MEM_DRAW, sieve_start, sieve_size, dxpl_id, dset->cache.contig.sieve_buf)<0)
                                HGOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, "block write failed");

                            /* Reset sieve buffer dirty flag */
                            dset->cache.contig.sieve_dirty=0;
                        } /* end if */
                    } /* end if */

                    /* Read directly into the user's buffer */
                    if (H5F_block_read(f, H5FD_MEM_DRAW, addr, size, dxpl_id, buf)<0)
                        HGOTO_ERROR(H5E_IO, H5E_READERROR, FAIL, "block read failed");
                } /* end if */
                /* Element size fits within the buffer size */
                else {
                    /* Flush the sieve buffer if it's dirty */
                    if(dset->cache.contig.sieve_dirty) {
                        /* Write to file */
                        if (H5F_block_write(f, H5FD_MEM_DRAW, sieve_start, sieve_size, dxpl_id, dset->cache.contig.sieve_buf)<0)
                            HGOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, "block write failed");

                        /* Reset sieve buffer dirty flag */
                        dset->cache.contig.sieve_dirty=0;
                    } /* end if */

                    /* Determine the new sieve buffer size & location */
                    dset->cache.contig.sieve_loc=addr;

                    /* Make certain we don't read off the end of the file */
                    if (HADDR_UNDEF==(abs_eoa=H5F_get_eoa(f)))
                        HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, FAIL, "unable to determine file size");

                    /* Adjust absolute EOA address to relative EOA address */
                    rel_eoa=abs_eoa-H5F_get_base_addr(f);

                    /* Only need this when resizing sieve buffer */
                    max_data=dset_size-dset_offset_arr[u];

                    /* Compute the size of the sieve buffer */
                    /* Don't read off the end of the file, don't read past the end of the data element and don't read more than the buffer size */
                    H5_ASSIGN_OVERFLOW(dset->cache.contig.sieve_size,MIN3(rel_eoa-dset->cache.contig.sieve_loc,max_data,dset->cache.contig.sieve_buf_size),hsize_t,size_t);

                    /* Update local copies of sieve information */
                    sieve_start=dset->cache.contig.sieve_loc;
                    sieve_size=dset->cache.contig.sieve_size;
                    sieve_end=sieve_start+sieve_size;

                    /* Read the new sieve buffer */
                    if (H5F_block_read(f, H5FD_MEM_DRAW, dset->cache.contig.sieve_loc, dset->cache.contig.sieve_size, dxpl_id, dset->cache.contig.sieve_buf)<0)
                        HGOTO_ERROR(H5E_IO, H5E_READERROR, FAIL, "block read failed");

                    /* Grab the data out of the buffer (must be first piece of data in buffer ) */
                    HDmemcpy(buf,dset->cache.contig.sieve_buf,size);

                    /* Reset sieve buffer dirty flag */
                    dset->cache.contig.sieve_dirty=0;
                } /* end else */
            } /* end else */

            /* Update memory information */
            mem_len_arr[v]-=size;
            mem_offset_arr[v]+=size;
            if(mem_len_arr[v]==0)
                v++;

            /* Update file information */
            dset_len_arr[u]-=size;
            dset_offset_arr[u]+=size;
            if(dset_len_arr[u]==0)
                u++;

            /* Increment number of bytes copied */
            ret_value+=size;
        } /* end for */
    } /* end if */
    else {
        /* Work through all the sequences */
        for(u=*dset_curr_seq, v=*mem_curr_seq; u<dset_max_nseq && v<mem_max_nseq; ) {
            /* Choose smallest buffer to write */
            if(mem_len_arr[v]<dset_len_arr[u])
                size=mem_len_arr[v];
            else
                size=dset_len_arr[u];

            /* Compute offset on disk */
            addr=dset_addr+dset_offset_arr[u];

            /* Compute offset in memory */
            buf = (unsigned char *)_buf + mem_offset_arr[v];

            /* Write data */
            if (H5F_block_read(f, H5FD_MEM_DRAW, addr, size, dxpl_id, buf)<0)
                HGOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, "block write failed");

            /* Update memory information */
            mem_len_arr[v]-=size;
            mem_offset_arr[v]+=size;
            if(mem_len_arr[v]==0)
                v++;

            /* Update file information */
            dset_len_arr[u]-=size;
            dset_offset_arr[u]+=size;
            if(dset_len_arr[u]==0)
                u++;

            /* Increment number of bytes copied */
            ret_value+=size;
        } /* end for */
    } /* end else */

    /* Update current sequence vectors */
    *dset_curr_seq=u;
    *mem_curr_seq=v;

done:
    FUNC_LEAVE_NOAPI(ret_value);
}   /* end H5D_contig_readvv() */


/*-------------------------------------------------------------------------
 * Function:	H5D_contig_writevv
 *
 * Purpose:	Writes some data vectors into a dataset from vectors into a
 *              buffer.  The address is the start of the dataset,
 *              relative to the base address for the file and the offsets and
 *              sequence lengths are in bytes.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              Friday, May 2, 2003
 *
 * Notes:
 *      Offsets in the sequences must be monotonically increasing
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
ssize_t
H5D_contig_writevv(H5F_t *f, hid_t dxpl_id, H5D_t *dset,
    haddr_t dset_addr, hsize_t dset_size,
    size_t dset_max_nseq, size_t *dset_curr_seq, size_t dset_len_arr[], hsize_t dset_offset_arr[],
    size_t mem_max_nseq, size_t *mem_curr_seq, size_t mem_len_arr[], hsize_t mem_offset_arr[],
    const void *_buf)
{
    const unsigned char *buf=_buf;      /* Pointer to buffer to fill */
    haddr_t abs_eoa;	        /* Absolute end of file address		*/
    haddr_t rel_eoa;	        /* Relative end of file address		*/
    haddr_t addr;               /* Actual address to read */
    hsize_t max_data;           /* Actual maximum size of data to cache */
    size_t size;                /* Size of sequence in bytes */
    size_t u;                   /* Counting variable */
    size_t v;                   /* Counting variable */
    ssize_t ret_value=0;        /* Return value */
   
    FUNC_ENTER_NOAPI(H5D_contig_writevv, FAIL);

    /* Check args */
    assert(f);
    assert(dset);
    assert(buf);

    /* Check if data sieving is enabled */
    if(H5F_HAS_FEATURE(f,H5FD_FEAT_DATA_SIEVE)) {
        haddr_t sieve_start, sieve_end;     /* Start & end locations of sieve buffer */
        haddr_t contig_end;             /* End locations of block to write */
        size_t sieve_size;              /* size of sieve buffer */

        /* Set offsets in sequence lists */
        u=*dset_curr_seq;
        v=*mem_curr_seq;

        /* No data sieve buffer yet, go allocate one */
        if(dset->cache.contig.sieve_buf==NULL) {
            /* Choose smallest buffer to write */
            if(mem_len_arr[v]<dset_len_arr[u])
                size=mem_len_arr[v];
            else
                size=dset_len_arr[u];

            /* Compute offset on disk */
            addr=dset_addr+dset_offset_arr[u];

            /* Compute offset in memory */
            buf = (const unsigned char *)_buf + mem_offset_arr[v];

            /* Set up the buffer parameters */
            max_data=dset_size-dset_offset_arr[u];

            /* Check if we can actually hold the I/O request in the sieve buffer */
            if(size>dset->cache.contig.sieve_buf_size) {
                if (H5F_block_write(f, H5FD_MEM_DRAW, addr, size, dxpl_id, buf)<0)
                    HGOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, "block write failed");
            } /* end if */
            else {
                /* Allocate room for the data sieve buffer */
                if (NULL==(dset->cache.contig.sieve_buf=H5FL_BLK_MALLOC(sieve_buf,dset->cache.contig.sieve_buf_size)))
                    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

                /* Determine the new sieve buffer size & location */
                dset->cache.contig.sieve_loc=addr;

                /* Make certain we don't read off the end of the file */
                if (HADDR_UNDEF==(abs_eoa=H5F_get_eoa(f)))
                    HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, FAIL, "unable to determine file size");

                /* Adjust absolute EOA address to relative EOA address */
                rel_eoa=abs_eoa-H5F_get_base_addr(f);

                /* Compute the size of the sieve buffer */
                H5_ASSIGN_OVERFLOW(dset->cache.contig.sieve_size,MIN3(rel_eoa-dset->cache.contig.sieve_loc,max_data,dset->cache.contig.sieve_buf_size),hsize_t,size_t);

                /* Check if there is any point in reading the data from the file */
                if(dset->cache.contig.sieve_size>size) {
                    /* Read the new sieve buffer */
                    if (H5F_block_read(f, H5FD_MEM_DRAW, dset->cache.contig.sieve_loc, dset->cache.contig.sieve_size, dxpl_id, dset->cache.contig.sieve_buf)<0)
                        HGOTO_ERROR(H5E_IO, H5E_READERROR, FAIL, "block read failed");
                } /* end if */

                /* Grab the data out of the buffer (must be first piece of data in buffer ) */
                HDmemcpy(dset->cache.contig.sieve_buf,buf,size);

                /* Set sieve buffer dirty flag */
                dset->cache.contig.sieve_dirty=1;
            } /* end else */

            /* Update memory information */
            mem_len_arr[v]-=size;
            mem_offset_arr[v]+=size;
            if(mem_len_arr[v]==0)
                v++;

            /* Update file information */
            dset_len_arr[u]-=size;
            dset_offset_arr[u]+=size;
            if(dset_len_arr[u]==0)
                u++;

            /* Increment number of bytes copied */
            ret_value+=size;
        } /* end if */

        /* Stash local copies of these value */
        sieve_start=dset->cache.contig.sieve_loc;
        sieve_size=dset->cache.contig.sieve_size;
        sieve_end=sieve_start+sieve_size;
        
        /* Works through sequences as fast as possible */
        for(; u<dset_max_nseq && v<mem_max_nseq; ) {
            /* Choose smallest buffer to write */
            if(mem_len_arr[v]<dset_len_arr[u])
                size=mem_len_arr[v];
            else
                size=dset_len_arr[u];

            /* Compute offset on disk */
            addr=dset_addr+dset_offset_arr[u];

            /* Compute offset in memory */
            buf = (const unsigned char *)_buf + mem_offset_arr[v];

            /* Compute end of sequence to retrieve */
            contig_end=addr+size-1;

            /* If entire write is within the sieve buffer, write it to the buffer */
            if(addr>=sieve_start && contig_end<sieve_end) {
                unsigned char *base_sieve_buf=dset->cache.contig.sieve_buf+(addr-sieve_start);

                /* Put the data into the sieve buffer */
                HDmemcpy(base_sieve_buf,buf,size);

                /* Set sieve buffer dirty flag */
                dset->cache.contig.sieve_dirty=1;

            } /* end if */
            /* Entire request is not within this data sieve buffer */
            else {
                /* Check if we can actually hold the I/O request in the sieve buffer */
                if(size>dset->cache.contig.sieve_buf_size) {
                    /* Check for any overlap with the current sieve buffer */
                    if((sieve_start>=addr && sieve_start<(contig_end+1))
                            || ((sieve_end-1)>=addr && (sieve_end-1)<(contig_end+1))) {
                        /* Flush the sieve buffer, if it's dirty */
                        if(dset->cache.contig.sieve_dirty) {
                            /* Write to file */
                            if (H5F_block_write(f, H5FD_MEM_DRAW, sieve_start, sieve_size, dxpl_id, dset->cache.contig.sieve_buf)<0)
                                HGOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, "block write failed");

                            /* Reset sieve buffer dirty flag */
                            dset->cache.contig.sieve_dirty=0;
                        } /* end if */

                        /* Force the sieve buffer to be re-read the next time */
                        dset->cache.contig.sieve_loc=HADDR_UNDEF;
                        dset->cache.contig.sieve_size=0;
                    } /* end if */

                    /* Write directly from the user's buffer */
                    if (H5F_block_write(f, H5FD_MEM_DRAW, addr, size, dxpl_id, buf)<0)
                        HGOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, "block write failed");
                } /* end if */
                /* Element size fits within the buffer size */
                else {
                    /* Check if it is possible to (exactly) prepend or append to existing (dirty) sieve buffer */
                    if(((addr+size)==sieve_start || addr==sieve_end) &&
                            (size+sieve_size)<=dset->cache.contig.sieve_buf_size &&
                            dset->cache.contig.sieve_dirty) {
                        /* Prepend to existing sieve buffer */
                        if((addr+size)==sieve_start) {
                            /* Move existing sieve information to correct location */
                            HDmemmove(dset->cache.contig.sieve_buf+size,dset->cache.contig.sieve_buf,sieve_size);

                            /* Copy in new information (must be first in sieve buffer) */
                            HDmemcpy(dset->cache.contig.sieve_buf,buf,size);

                            /* Adjust sieve location */
                            dset->cache.contig.sieve_loc=addr;
                            
                        } /* end if */
                        /* Append to existing sieve buffer */
                        else {
                            /* Copy in new information */
                            HDmemcpy(dset->cache.contig.sieve_buf+sieve_size,buf,size);
                        } /* end else */

                        /* Adjust sieve size */
                        dset->cache.contig.sieve_size += size;
                        
                        /* Update local copies of sieve information */
                        sieve_start=dset->cache.contig.sieve_loc;
                        sieve_size=dset->cache.contig.sieve_size;
                        sieve_end=sieve_start+sieve_size;

                    } /* end if */
                    /* Can't add the new data onto the existing sieve buffer */
                    else {
                        /* Flush the sieve buffer if it's dirty */
                        if(dset->cache.contig.sieve_dirty) {
                            /* Write to file */
                            if (H5F_block_write(f, H5FD_MEM_DRAW, sieve_start, sieve_size, dxpl_id, dset->cache.contig.sieve_buf)<0)
                                HGOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, "block write failed");

                            /* Reset sieve buffer dirty flag */
                            dset->cache.contig.sieve_dirty=0;
                        } /* end if */

                        /* Determine the new sieve buffer size & location */
                        dset->cache.contig.sieve_loc=addr;

                        /* Make certain we don't read off the end of the file */
                        if (HADDR_UNDEF==(abs_eoa=H5F_get_eoa(f)))
                            HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, FAIL, "unable to determine file size");

                        /* Adjust absolute EOA address to relative EOA address */
                        rel_eoa=abs_eoa-H5F_get_base_addr(f);

                        /* Only need this when resizing sieve buffer */
                        max_data=dset_size-dset_offset_arr[u];

                        /* Compute the size of the sieve buffer */
                        /* Don't read off the end of the file, don't read past the end of the data element and don't read more than the buffer size */
                        H5_ASSIGN_OVERFLOW(dset->cache.contig.sieve_size,MIN3(rel_eoa-dset->cache.contig.sieve_loc,max_data,dset->cache.contig.sieve_buf_size),hsize_t,size_t);

                        /* Update local copies of sieve information */
                        sieve_start=dset->cache.contig.sieve_loc;
                        sieve_size=dset->cache.contig.sieve_size;
                        sieve_end=sieve_start+sieve_size;

                        /* Check if there is any point in reading the data from the file */
                        if(dset->cache.contig.sieve_size>size) {
                            /* Read the new sieve buffer */
                            if (H5F_block_read(f, H5FD_MEM_DRAW, dset->cache.contig.sieve_loc, dset->cache.contig.sieve_size, dxpl_id, dset->cache.contig.sieve_buf)<0)
                                HGOTO_ERROR(H5E_IO, H5E_READERROR, FAIL, "block read failed");
                        } /* end if */

                        /* Grab the data out of the buffer (must be first piece of data in buffer ) */
                        HDmemcpy(dset->cache.contig.sieve_buf,buf,size);

                        /* Set sieve buffer dirty flag */
                        dset->cache.contig.sieve_dirty=1;

                    } /* end else */
                } /* end else */
            } /* end else */

            /* Update memory information */
            mem_len_arr[v]-=size;
            mem_offset_arr[v]+=size;
            if(mem_len_arr[v]==0)
                v++;

            /* Update file information */
            dset_len_arr[u]-=size;
            dset_offset_arr[u]+=size;
            if(dset_len_arr[u]==0)
                u++;

            /* Increment number of bytes copied */
            ret_value+=size;
        } /* end for */
    } /* end if */
    else {
        /* Work through all the sequences */
        for(u=*dset_curr_seq, v=*mem_curr_seq; u<dset_max_nseq && v<mem_max_nseq; ) {
            /* Choose smallest buffer to write */
            if(mem_len_arr[v]<dset_len_arr[u])
                size=mem_len_arr[v];
            else
                size=dset_len_arr[u];

            /* Compute offset on disk */
            addr=dset_addr+dset_offset_arr[u];

            /* Compute offset in memory */
            buf = (const unsigned char *)_buf + mem_offset_arr[v];

            /* Write data */
            if (H5F_block_write(f, H5FD_MEM_DRAW, addr, size, dxpl_id, buf)<0)
                HGOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, "block write failed");

            /* Update memory information */
            mem_len_arr[v]-=size;
            mem_offset_arr[v]+=size;
            if(mem_len_arr[v]==0)
                v++;

            /* Update file information */
            dset_len_arr[u]-=size;
            dset_offset_arr[u]+=size;
            if(dset_len_arr[u]==0)
                u++;

            /* Increment number of bytes copied */
            ret_value+=size;
        } /* end for */
    } /* end else */

    /* Update current sequence vectors */
    *dset_curr_seq=u;
    *mem_curr_seq=v;

done:
    FUNC_LEAVE_NOAPI(ret_value);
}   /* end H5D_contig_writevv() */

