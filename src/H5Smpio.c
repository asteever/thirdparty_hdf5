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
 * Programmer:  rky 980813
 *
 * Purpose:	Functions to read/write directly between app buffer and file.
 *
 * 		Beware of the ifdef'ed print statements.
 *		I didn't make them portable.
 */

#define H5S_PACKAGE		/*suppress error about including H5Spkg	  */

/* Pablo information */
/* (Put before include files to avoid problems with inline functions) */
#define PABLO_MASK	H5S_mpio_mask

#include "H5private.h"		/* Generic Functions			*/
#include "H5Dprivate.h"		/* Datasets				*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5Fprivate.h"		/* File access				*/
#include "H5FDprivate.h"	/* File drivers				*/
#include "H5Iprivate.h"		/* IDs			  		*/
#include "H5Oprivate.h"		/* Object headers		  	*/
#include "H5Pprivate.h"         /* Property lists                       */
#include "H5Spkg.h"		/* Dataspaces 				*/

#ifdef H5_HAVE_PARALLEL

static herr_t
H5S_mpio_all_type( const H5S_t *space, size_t elmt_size,
		     /* out: */
		     MPI_Datatype *new_type,
		     size_t *count,
		     hsize_t *extra_offset,
		     hbool_t *is_derived_type );
static herr_t
H5S_mpio_none_type( const H5S_t *space, size_t elmt_size,
		     /* out: */
		     MPI_Datatype *new_type,
		     size_t *count,
		     hsize_t *extra_offset,
		     hbool_t *is_derived_type );
static herr_t
H5S_mpio_hyper_type( const H5S_t *space, size_t elmt_size,
		     /* out: */
		     MPI_Datatype *new_type,
		     size_t *count,
		     hsize_t *extra_offset,
		     hbool_t *is_derived_type );


/*-------------------------------------------------------------------------
 * Function:	H5S_mpio_all_type
 *
 * Purpose:	Translate an HDF5 "all" selection into an MPI type.
 *
 * Return:	non-negative on success, negative on failure.
 *
 * Outputs:	*new_type	  the MPI type corresponding to the selection
 *		*count		  how many objects of the new_type in selection
 *				  (useful if this is the buffer type for xfer)
 *		*extra_offset     Number of bytes of offset within dataset
 *		*is_derived_type  0 if MPI primitive type, 1 if derived
 *
 * Programmer:	rky 980813
 *
 * Modifications:
 *
 *      Quincey Koziol, June 18, 2002
 *      Added 'extra_offset' parameter
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5S_mpio_all_type( const H5S_t *space, size_t elmt_size,
		     /* out: */
		     MPI_Datatype *new_type,
		     size_t *count,
		     hsize_t *extra_offset,
		     hbool_t *is_derived_type )
{
    hsize_t	total_bytes;
    hssize_t	snelmts;                /*total number of elmts	(signed) */
    hsize_t	nelmts;                 /*total number of elmts	*/
    herr_t		ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT(H5S_mpio_all_type);

    /* Check args */
    assert (space);

    /* Just treat the entire extent as a block of bytes */
    if((snelmts = H5S_GET_EXTENT_NPOINTS(space))<0)
	HGOTO_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL, "src dataspace has invalid selection")
    H5_ASSIGN_OVERFLOW(nelmts,snelmts,hssize_t,hsize_t);

    total_bytes = (hsize_t)elmt_size*nelmts;

    /* fill in the return values */
    *new_type = MPI_BYTE;
    H5_ASSIGN_OVERFLOW(*count, total_bytes, hsize_t, size_t);
    *extra_offset = 0;
    *is_derived_type = 0;

done:
    FUNC_LEAVE_NOAPI(ret_value);
} /* H5S_mpio_all_type() */


/*-------------------------------------------------------------------------
 * Function:	H5S_mpio_none_type
 *
 * Purpose:	Translate an HDF5 "none" selection into an MPI type.
 *
 * Return:	non-negative on success, negative on failure.
 *
 * Outputs:	*new_type	  the MPI type corresponding to the selection
 *		*count		  how many objects of the new_type in selection
 *				  (useful if this is the buffer type for xfer)
 *		*extra_offset     Number of bytes of offset within dataset
 *		*is_derived_type  0 if MPI primitive type, 1 if derived
 *
 * Programmer:	Quincey Koziol, October 29, 2002
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5S_mpio_none_type( const H5S_t UNUSED *space, size_t UNUSED elmt_size,
		     /* out: */
		     MPI_Datatype *new_type,
		     size_t *count,
		     hsize_t *extra_offset,
		     hbool_t *is_derived_type )
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5S_mpio_none_type);

    /* fill in the return values */
    *new_type = MPI_BYTE;
    *count = 0;
    *extra_offset = 0;
    *is_derived_type = 0;

    FUNC_LEAVE_NOAPI(SUCCEED);
} /* H5S_mpio_none_type() */


/*-------------------------------------------------------------------------
 * Function:	H5S_mpio_hyper_type
 *
 * Purpose:	Translate an HDF5 hyperslab selection into an MPI type.
 *
 * Return:	non-negative on success, negative on failure.
 *
 * Outputs:	*new_type	  the MPI type corresponding to the selection
 *		*count		  how many objects of the new_type in selection
 *				  (useful if this is the buffer type for xfer)
 *		*extra_offset     Number of bytes of offset within dataset
 *		*is_derived_type  0 if MPI primitive type, 1 if derived
 *
 * Programmer:	rky 980813
 *
 * Modifications:  ppw 990401
 *		rky, ppw 2000-09-26 Freed old type after creating struct type.
 *		rky 2000-10-05 Changed displacements to be MPI_Aint.
 *		rky 2000-10-06 Added code for cases of empty hyperslab.
 *		akc, rky 2000-11-16 Replaced hard coded dimension size with
 *		    H5S_MAX_RANK.
 *
 *      	Quincey Koziol, June 18, 2002
 *      	Added 'extra_offset' parameter.  Also accomodate selection
 *      	offset in MPI type built.
 *
 *      	Albert Cheng, August 4, 2004
 *      	Reimplemented the algorithm of forming the outer_type by
 *      	defining it as (start, vector, extent) in one call.
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5S_mpio_hyper_type( const H5S_t *space, size_t elmt_size,
		     /* out: */
		     MPI_Datatype *new_type,
		     size_t *count,
		     hsize_t *extra_offset,
		     hbool_t *is_derived_type )
{
    H5S_sel_iter_t sel_iter;    /* Selection iteration info */
    hbool_t sel_iter_init=0;    /* Selection iteration info has been initialized */

    struct dim {	/* less hassle than malloc/free & ilk */
        hssize_t start;
        hsize_t strid;
        hsize_t block;
        hsize_t xtent;
        hsize_t count;
    } d[H5S_MAX_RANK];

    int			i;
    int			offset[H5S_MAX_RANK];
    int			max_xtent[H5S_MAX_RANK];
    H5S_hyper_dim_t	*diminfo;		/* [rank] */
    int		rank;
    int			block_length[3];
    MPI_Datatype	inner_type, outer_type, old_types[3];
    MPI_Aint            extent_len, displacement[3];
    int                 mpi_code;               /* MPI return code */
    herr_t		ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT(H5S_mpio_hyper_type);

    /* Check args */
    assert (space);
    assert(sizeof(MPI_Aint) >= sizeof(elmt_size));
    if (0==elmt_size)
        goto empty;

    /* Initialize selection iterator */
    if (H5S_select_iter_init(&sel_iter, space, elmt_size)<0)
        HGOTO_ERROR (H5E_DATASPACE, H5E_CANTINIT, FAIL, "unable to initialize selection iterator");
    sel_iter_init=1;	/* Selection iteration info has been initialized */

    /* Abbreviate args */
    diminfo=sel_iter.u.hyp.diminfo;
    assert (diminfo);

    /* make a local copy of the dimension info so we can operate with them */

    /* Check if this is a "flattened" regular hyperslab selection */
    if(sel_iter.u.hyp.iter_rank!=0 && sel_iter.u.hyp.iter_rank<space->extent.rank) {
        /* Flattened selection */
        rank=sel_iter.u.hyp.iter_rank;
        assert (rank >= 0 && rank<=H5S_MAX_RANK);	/* within array bounds */
        if (0==rank)
            goto empty;

#ifdef H5Smpi_DEBUG
            HDfprintf(stderr, "%s: Flattened selection\n",FUNC);
#endif
        for ( i=0; i<rank; ++i) {
            d[i].start = diminfo[i].start+sel_iter.u.hyp.sel_off[i];
            d[i].strid = diminfo[i].stride;
            d[i].block = diminfo[i].block;
            d[i].count = diminfo[i].count;
            d[i].xtent = sel_iter.u.hyp.size[i];
#ifdef H5Smpi_DEBUG
            HDfprintf(stderr, "%s: start=%Hd  stride=%Hu  count=%Hu  block=%Hu  xtent=%Hu",
                FUNC, d[i].start, d[i].strid, d[i].count, d[i].block, d[i].xtent );
            if (i==0)
                HDfprintf(stderr, "  rank=%d\n", rank );
            else
                HDfprintf(stderr, "\n" );
#endif
            if (0==d[i].block)
                goto empty;
            if (0==d[i].count)
                goto empty;
            if (0==d[i].xtent)
                goto empty;
        }
    } /* end if */
    else {
        /* Non-flattened selection */
        rank = space->extent.rank;
        assert (rank >= 0 && rank<=H5S_MAX_RANK);	/* within array bounds */
        if (0==rank)
            goto empty;

#ifdef H5Smpi_DEBUG
            HDfprintf(stderr, "%s: Non-flattened selection\n",FUNC);
#endif
        for ( i=0; i<rank; ++i) {
            d[i].start = diminfo[i].start+space->select.offset[i];
            d[i].strid = diminfo[i].stride;
            d[i].block = diminfo[i].block;
            d[i].count = diminfo[i].count;
            d[i].xtent = space->extent.size[i];
#ifdef H5Smpi_DEBUG
            HDfprintf(stderr, "%s: start=%Hd  stride=%Hu  count=%Hu  block=%Hu  xtent=%Hu",
                FUNC, d[i].start, d[i].strid, d[i].count, d[i].block, d[i].xtent );
            if (i==0)
                HDfprintf(stderr, "  rank=%d\n", rank );
            else
                HDfprintf(stderr, "\n" );
#endif
            if (0==d[i].block)
                goto empty;
            if (0==d[i].count)
                goto empty;
            if (0==d[i].xtent)
                goto empty;
        }
    } /* end else */
    
/**********************************************************************
    Compute array "offset[rank]" which gives the offsets for a multi-
    dimensional array with dimensions "d[i].xtent" (i=0,1,...,rank-1). 
**********************************************************************/
    offset[rank-1] = 1;
    max_xtent[rank-1] = d[rank-1].xtent;
#ifdef H5Smpi_DEBUG
    i=rank-1;
    HDfprintf(stderr, " offset[%2d]=%d; max_xtent[%2d]=%d\n",
                          i, offset[i], i, max_xtent[i]);
#endif
    for (i=rank-2; i>=0; --i) {
        offset[i] = offset[i+1]*d[i+1].xtent;
        max_xtent[i] = max_xtent[i+1]*d[i].xtent;
#ifdef H5Smpi_DEBUG
        HDfprintf(stderr, " offset[%2d]=%d; max_xtent[%2d]=%d\n",
                          i, offset[i], i, max_xtent[i]);
#endif
    }

    /*  Create a type covering the selected hyperslab.
     *  Multidimensional dataspaces are stored in row-major order.
     *  The type is built from the inside out, going from the
     *  fastest-changing (i.e., inner) dimension * to the slowest (outer). */

/*******************************************************
*  Construct contig type for inner contig dims:
*******************************************************/
#ifdef H5Smpi_DEBUG
    HDfprintf(stderr, "%s: Making contig type %d MPI_BYTEs\n", FUNC,elmt_size );
    for (i=rank-1; i>=0; --i)
        HDfprintf(stderr, "d[%d].xtent=%Hu \n", i, d[i].xtent);
#endif
    if (MPI_SUCCESS != (mpi_code= MPI_Type_contiguous( (int)elmt_size, MPI_BYTE, &inner_type )))
        HMPI_GOTO_ERROR(FAIL, "MPI_Type_contiguous failed", mpi_code);

/*******************************************************
*  Construct the type by walking the hyperslab dims
*  from the inside out:
*******************************************************/
    for ( i=rank-1; i>=0; --i) {
#ifdef H5Smpi_DEBUG
        HDfprintf(stderr, "%s: Dimension i=%d \n"
            "start=%Hd count=%Hu block=%Hu stride=%Hu, xtent=%Hu max_xtent=%d\n",
            FUNC, i, d[i].start, d[i].count, d[i].block, d[i].strid, d[i].xtent, max_xtent[i]);
#endif

#ifdef H5Smpi_DEBUG
        HDfprintf(stderr, "%s: i=%d  Making vector-type \n", FUNC,i);
#endif
       /****************************************
       *  Build vector type of the selection.
       ****************************************/
	mpi_code =MPI_Type_vector((int)(d[i].count),        /* count */
				  (int)(d[i].block),        /* blocklength */
				  (int)(d[i].strid),   	    /* stride */
				  inner_type,	            /* old type */
				  &outer_type);            /* new type */

        MPI_Type_free( &inner_type );
        if (mpi_code!=MPI_SUCCESS)
            HMPI_GOTO_ERROR(FAIL, "couldn't create MPI vector type", mpi_code);

       /****************************************
       *  Then build the dimension type as (start, vector type, xtent).
       ****************************************/
       /* calculate start and extent values of this dimension */
	displacement[1] = d[i].start * offset[i] * elmt_size;
        displacement[2] = (MPI_Aint)elmt_size * max_xtent[i];
        if(MPI_SUCCESS != (mpi_code = MPI_Type_extent(outer_type, &extent_len)))
            HMPI_GOTO_ERROR(FAIL, "MPI_Type_extent failed", mpi_code);

       /*************************************************
       *  Restructure this datatype ("outer_type")
       *  so that it still starts at 0, but its extent
       *  is the full extent in this dimension.
       *************************************************/
        if (displacement[1] > 0 || (int)extent_len < displacement[2]) {

            block_length[0] = 1;
            block_length[1] = 1;
            block_length[2] = 1;
  
            displacement[0] = 0;
  
            old_types[0] = MPI_LB;
            old_types[1] = outer_type;
            old_types[2] = MPI_UB;
#ifdef H5Smpi_DEBUG
            HDfprintf(stderr, "%s: i=%d Extending struct type\n"
                "***displacements: %d, %d, %d\n",
		FUNC, i, displacement[0], displacement[1], displacement[2]);
#endif

            mpi_code = MPI_Type_struct ( 3,               /* count */
                                    block_length,    /* blocklengths */
                                    displacement,    /* displacements */
                                    old_types,       /* old types */
                                    &inner_type);    /* new type */
  
            MPI_Type_free (&outer_type);
    	    if (mpi_code!=MPI_SUCCESS)
                HMPI_GOTO_ERROR(FAIL, "couldn't resize MPI vector type", mpi_code);
        }
        else {
            inner_type = outer_type;
        }
    } /* end for */
/***************************
*  End of loop, walking 
*  thru dimensions.
***************************/


    /* At this point inner_type is actually the outermost type, even for 0-trip loop */

    *new_type = inner_type;
    if (MPI_SUCCESS != (mpi_code= MPI_Type_commit( new_type )))
        HMPI_GOTO_ERROR(FAIL, "MPI_Type_commit failed", mpi_code);

    /* fill in the remaining return values */
    *count = 1;			/* only have to move one of these suckers! */
    *extra_offset = 0;
    *is_derived_type = 1;
    HGOTO_DONE(SUCCEED);

empty:
    /* special case: empty hyperslab */
    *new_type = MPI_BYTE;
    *count = 0;
    *extra_offset = 0;
    *is_derived_type = 0;

done:
    /* Release selection iterator */
    if(sel_iter_init) {
        if (H5S_SELECT_ITER_RELEASE(&sel_iter)<0)
            HDONE_ERROR (H5E_DATASPACE, H5E_CANTRELEASE, FAIL, "unable to release selection iterator");
    } /* end if */

#ifdef H5Smpi_DEBUG
    HDfprintf(stderr, "Leave %s, count=%ld  is_derived_type=%d\n",
		FUNC, *count, *is_derived_type );
#endif
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5S_mpio_space_type
 *
 * Purpose:	Translate an HDF5 dataspace selection into an MPI type.
 *		Currently handle only hyperslab and "all" selections.
 *
 * Return:	non-negative on success, negative on failure.
 *
 * Outputs:	*new_type	  the MPI type corresponding to the selection
 *		*count		  how many objects of the new_type in selection
 *				  (useful if this is the buffer type for xfer)
 *		*extra_offset     Number of bytes of offset within dataset
 *		*is_derived_type  0 if MPI primitive type, 1 if derived
 *
 * Programmer:	rky 980813
 *
 * Modifications:
 *
 *      Quincey Koziol, June 18, 2002
 *      Added 'extra_offset' parameter
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5S_mpio_space_type( const H5S_t *space, size_t elmt_size,
		     /* out: */
		     MPI_Datatype *new_type,
		     size_t *count,
		     hsize_t *extra_offset,
		     hbool_t *is_derived_type )
{
    herr_t	ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT(H5S_mpio_space_type);

    /* Check args */
    assert (space);

    /* Creat MPI type based on the kind of selection */
    switch (H5S_GET_EXTENT_TYPE(space)) {
        case H5S_NULL:
        case H5S_SCALAR:
        case H5S_SIMPLE:
            switch(H5S_GET_SELECT_TYPE(space)) {
                case H5S_SEL_NONE:
                    if ( H5S_mpio_none_type( space, elmt_size,
                        /* out: */ new_type, count, extra_offset, is_derived_type ) <0)
                        HGOTO_ERROR(H5E_DATASPACE, H5E_BADTYPE, FAIL,"couldn't convert \"all\" selection to MPI type");
                    break;

                case H5S_SEL_ALL:
                    if ( H5S_mpio_all_type( space, elmt_size,
                        /* out: */ new_type, count, extra_offset, is_derived_type ) <0)
                        HGOTO_ERROR(H5E_DATASPACE, H5E_BADTYPE, FAIL,"couldn't convert \"all\" selection to MPI type");
                    break;

                case H5S_SEL_POINTS:
                    /* not yet implemented */
                    ret_value = FAIL;
                    break;

                case H5S_SEL_HYPERSLABS:
                    if(H5S_mpio_hyper_type( space, elmt_size,
                            /* out: */ new_type, count, extra_offset, is_derived_type )<0)
                        HGOTO_ERROR(H5E_DATASPACE, H5E_BADTYPE, FAIL,"couldn't convert \"all\" selection to MPI type");
                    break;

                default:
                    assert("unknown selection type" && 0);
                    break;
            } /* end switch */
            break;

        case H5S_COMPLEX:
            /* not yet implemented */
            HGOTO_ERROR(H5E_DATASPACE, H5E_UNSUPPORTED, FAIL, "complex data spaces are not supported yet");

        default:
            assert("unknown data space type" && 0);
            break;
    }

done:
    FUNC_LEAVE_NOAPI(ret_value);
}
#endif  /* H5_HAVE_PARALLEL */
