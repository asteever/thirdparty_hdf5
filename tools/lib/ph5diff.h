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

#ifndef _PH5DIFF_H__
#define _PH5DIFF_H__


#define OUTBUFF_SIZE 10000
/* Send from manager to workers */
#define MPI_TAG_ARGS		1	
#define MPI_TAG_PRINT_TOK	2

/*Sent from workers to manager */
#define MPI_TAG_TOK_REQUEST	3
#define MPI_TAG_DONE		4
#define MPI_TAG_TOK_RETURN	5

/* Operational tags used to init and complete diff */
#define MPI_TAG_END		6
#define MPI_TAG_PARALLEL	7

int	g_nTasks;
char    outBuff[OUTBUFF_SIZE];
unsigned int	outBuffOffset;

struct diff_args
{
    char	name[256];
    H5G_obj_t   type;   
    diff_opt_t	options;
};   

#ifdef H5_HAVE_PARALLEL
#define H5_HAVE_PH5DIFF 1
#endif 

#ifdef H5_HAVE_PH5DIFF
#include <mpi.h>
#endif


#endif  /* _PH5DIFF_H__ */
