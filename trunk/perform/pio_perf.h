/*
 * Copyright (C) 2001
 *     National Center for Supercomputing Applications
 *     All rights reserved.
 *
 */
#ifndef PIO_PERF_H__
#define PIO_PERF_H__

#include "pio_timer.h"
#include "H5private.h"
#include "h5test.h"

/* setup the dataset no fill option if this is v1.5 or more */
#if H5_VERS_MAJOR > 1 || H5_VERS_MINOR > 4
#define H5_HAVE_NOFILL 1
#endif

typedef enum iotype_ {
    POSIXIO,
    MPIO,
    PHDF5
    /*NUM_TYPES*/
} iotype;

typedef struct parameters_ {
    iotype	io_type;        /* The type of IO test to perform       */
    int		num_procs;      /* Maximum number of processes to use   */
    int		num_files;      /* Number of files to create            */
    long	num_dsets;      /* Number of datasets to create         */
    off_t	num_elmts;      /* Number of native ints in each dset   */
    int		num_iters;      /* Number of times to loop doing the IO */
    size_t 	buf_size;       /* Buffer size                          */
    size_t      block_size;     /* interleaved block size               */
    hsize_t 	h5_align;       /* HDF5 object alignment                */
    hsize_t 	h5_thresh;      /* HDF5 object alignment threshold      */
    unsigned 	h5_use_chunks;  /* Make HDF5 dataset chunked            */
    unsigned    h5_no_fill;     /* Disable HDF5 writing fill values     */
    unsigned    h5_write_only;  /* Perform the write tests only         */
} parameters;

typedef struct results_ {
    herr_t      ret_code;
    pio_time   *timers;
} results;

#ifndef SUCCESS
#define SUCCESS     0
#endif  /* !SUCCESS */

#ifndef FAIL
#define FAIL        -1
#endif  /* !FAIL */

extern FILE     *output;            /* output file                          */
extern pio_time *timer_g;           /* timer: global for stub functions     */
extern int      comm_world_rank_g;  /* my rank in MPI_COMM_RANK             */
extern int      comm_world_nprocs_g;/* num. of processes of MPI_COMM_WORLD  */
extern MPI_Comm pio_comm_g;         /* Communicator to run the PIO          */
extern int      pio_mpi_rank_g;     /* MPI rank of pio_comm_g               */
extern int      pio_mpi_nprocs_g;   /* number of processes of pio_comm_g    */
extern int      pio_debug_level;    /* The debug level:
                                     *   0 - Off
                                     *   1 - Minimal
                                     *   2 - Some more
                                     *   3 - Maximal
                                     *   4 - Even More Debugging (timer stuff)
                                     */

#define HDprint_rank(f)              /* print rank in MPI_COMM_WORLD */    \
    HDfprintf(f, "%d: ", comm_world_rank_g);
#define HDprint_size(f)              /* print size of MPI_COMM_WORLD */    \
    HDfprintf(f, "%d", comm_world_nprocs_g);
#define HDprint_rank_size(f)         /* print rank/size of MPI_COMM_WORLD */  \
    HDfprintf(f, "%d/%d: ", comm_world_rank_g, comm_world_nprocs_g);

#ifdef __cplusplus
extern "C" {
#endif  /* __cplusplus */

extern results do_pio(parameters param);

#ifdef __cplusplus
}
#endif  /* __cplusplus */

#endif  /* PIO_PERF_H__ */
