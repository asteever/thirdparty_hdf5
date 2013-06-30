/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have          *
 * access to either file, you may request a copy from help@hdfgroup.org.     *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#include "use.h"

void
usage(const char *prog)
{
    fprintf(stderr, "usage: %s [OPTIONS]\n", prog);
    fprintf(stderr, "  OPTIONS\n");
    fprintf(stderr, "     -h, --help            Print a usage message and exit\n");
    fprintf(stderr, "     -f FN                 Test file name [default: %s.h5]\n", prog);
    fprintf(stderr, "     -i N, --iteration=N   Number of iterations to repeat the whole thing. [default: 1]\n");
    fprintf(stderr, "     -l w|r                launch writer or reader only. [default: launch both]\n");
    fprintf(stderr, "     -n N, --nplanes=N     Number of planes to write/read. [default: 1000]\n");
    fprintf(stderr, "     -s N, --swmr=N        Use SWMR mode (0: no, non-0: yes) default is yes\n");
    fprintf(stderr, "     -z N, --chunksize=N   Chunk size [default: %d]\n", Chunksize_DFT);
    fprintf(stderr, "     -y N, --chunkplanes=N Number of planes per chunk [default: 1]\n");
    fprintf(stderr, "\n");
}

/* Setup Use Case parameters by parsing command line options.
* Setup default values if not set by options. */
int
parse_option(int argc, char * const argv[])
{
    int ret_value=0;
    int c;
    /* command line options: See function usage for a description */
    const char *nagg_options = "f:hi:l:n:s:y:z:";

    /* suppress getopt from printing error */
    opterr = 0;

    while (1){
	c = getopt (argc, argv, nagg_options);
	if (-1 == c)
	    break;
	switch (c) {
	  case 'h':
	    usage(progname_g);
	    exit(0);
	    break;
	  case 'f':	/* usecase data file name */
	    UC_opts.filename = optarg;
	    break;
	  case 'i':	/* iterations */
	    if ((UC_opts.iterations = atoi(optarg)) <= 0){
		fprintf(stderr, "bad iterations number %s, must be a positive integer\n", optarg);
		usage(progname_g);
		Hgoto_error(-1);
	    };
	    break;
	  case 'l':	/* launch reader or writer only */
	    switch (*optarg) {
	      case 'r':	/* reader only */
		UC_opts.launch = UC_READER;
		break;
	      case 'w': /* writer only */
		UC_opts.launch = UC_WRITER;
		break;
	      default:
		fprintf(stderr, "launch value(%c) should be w or r only.\n", *optarg);
		usage(progname_g);
		Hgoto_error(-1);
		break;
	    }
	    break;
	  case 'n':	/* number of planes to write/read */
	    if ((UC_opts.nplanes = atoi(optarg)) <= 0){
		fprintf(stderr, "bad number of planes %s, must be a positive integer\n", optarg);
		usage(progname_g);
		Hgoto_error(-1);
	    };
	    break;
	  case 's':	/* use swmr file open mode */
	    if ((UC_opts.use_swmr = atoi(optarg)) < 0){
		fprintf(stderr, "swmr value should be 0(no) or 1(yes)\n");
		usage(progname_g);
		Hgoto_error(-1);
	    };
	    break;
	  case 'y':	/* Number of planes per chunk */
	    if ((UC_opts.chunkplanes = atoi(optarg)) <= 0){
		fprintf(stderr, "bad number of planes per chunk %s, must be a positive integer\n", optarg);
		usage(progname_g);
		Hgoto_error(-1);
	    };
	    break;
	  case 'z':	/* size of chunk=(z,z) */
	    if ((UC_opts.chunksize = atoi(optarg)) <= 0){
		fprintf(stderr, "bad chunksize %s, must be a positive integer\n", optarg);
		usage(progname_g);
		Hgoto_error(-1);
	    };
	    break;
	  case '?':
	    fprintf(stderr, "getopt returned '%c'.\n", c);
	    Hgoto_error(-1);
	  default:
	    fprintf(stderr, "getopt returned unexpected value.\n");
	    fprintf(stderr, "Unexpected value is %d\n", c);
	    Hgoto_error(-1);
	}
    }

    /* set test file name if not given */
    if (!UC_opts.filename){
	/* default data file name is <progname>.h5 */
	if ((UC_opts.filename=(char*)HDmalloc(HDstrlen(progname_g)+4))==NULL) {
	    fprintf(stderr, "malloc: failed\n");
	    Hgoto_error(-1);
	};
	HDstrcpy(UC_opts.filename, progname_g);
	HDstrcat(UC_opts.filename, ".h5");
    }

done:
    /* All done. */
    return(ret_value);
}

/* Show parameters used for this use case */
void show_parameters(void){
    printf("===Parameters used:===\n");
    printf("chunk dims=(%llu, %llu, %llu)\n", UC_opts.chunkdims[0], UC_opts.chunkdims[1], UC_opts.chunkdims[2]);
    printf("dataset max dims=(%llu, %llu, %llu)\n", UC_opts.max_dims[0], UC_opts.max_dims[1], UC_opts.max_dims[2]);
    printf("number of planes to write=%d\n", UC_opts.nplanes);
    printf("using SWMR mode=%s\n", UC_opts.use_swmr ? "yes(1)" : "no(0)");
    printf("data filename=%s\n", UC_opts.filename);
    printf("launch part=");
	switch (UC_opts.launch){
	case UC_READWRITE:
	    printf("Reader/Writer\n");
	    break;
	case UC_WRITER:
	    printf("Writer\n");
	    break;
	case UC_READER:
	    printf("Reader\n");
	    break;
	default:
	    /* should not happen */
	    printf("Illegal part(%d)\n", UC_opts.launch);
	};
    printf("number of iterations=%d (not used yet)\n", UC_opts.iterations);
    printf("===Parameters shown===\n");
}

/* Create the skeleton use case file for testing.
 * It has one 3d dataset using chunked storage.
 * The dataset is (unlimited, chunksize, chunksize).
 * Dataset type is 2 bytes integer.
 * It starts out "empty", i.e., first dimension is 0.
 *
 * Return: 0 succeed; -1 fail.
 */
int create_uc_file(void)
{
    hsize_t dims[3];		/* Dataset starting dimensions */
    hid_t fid;          /* File ID for new HDF5 file */
    hid_t dcpl;         /* Dataset creation property list */
    hid_t sid;          /* Dataspace ID */
    hid_t dsid;         /* Dataset ID */

    /* Create the file */
    if((fid = H5Fcreate(UC_opts.filename, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        return -1;

    /* Set up dimension sizes */
    dims[0] = 0;
    dims[1] = dims[2] = UC_opts.max_dims[1];

    /* Create dataspace for creating datasets */
    if((sid = H5Screate_simple(3, dims, UC_opts.max_dims)) < 0)
        return -1;

    /* Create dataset creation property list */
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        return -1;
    if(H5Pset_chunk(dcpl, 3, UC_opts.chunkdims) < 0)
        return -1;

    /* create dataset of progname */
    if((dsid = H5Dcreate2(fid, progname_g, UC_DATATYPE, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
	return -1;

    /* Close everythign */
    if(H5Dclose(dsid) < 0)
	return -1;

    if(H5Sclose(sid) < 0)
        return -1;
    if(H5Fclose(fid) < 0)
        return -1;

    return 0;
}

/* Append planes, each of (1,2*chunksize,2*chunksize) to the dataset.
 * In other words, 4 chunks are appended to the dataset at a time.
 * Fill each plan with the plane number and then write it at the nth plane.
 * Increase the plane number and repeat till the end of dataset, when it
 * reaches chunksize long. End product is a (2*chunksize)^3 cube.
 *
 * Return: 0 succeed; -1 fail.
 */
int write_uc_file(void)
{
    hid_t	fid;          /* File ID for new HDF5 file */
    hid_t	dsid;         /* dataset ID */
    hid_t	dcpl;         /* Dataset creation property list */
    char	*name;
    UC_CTYPE	*buffer, *bufptr;	/* data buffer */
    int		cz=UC_opts.chunksize;		/* Chunk size */
    hid_t	f_sid;	    /* dataset file space id */
    hid_t	m_sid;	    /* memory space id */
    int		rank;	    /* rank */
    hsize_t 	chunk_dims[3];	/* Chunk dimensions */
    hsize_t	dims[3];    /* Dataspace dimensions */
    hsize_t	memdims[3]; /* Memory space dimensions */
    hsize_t	start[3] = {0,0,0}, count[3];    /* Hyperslab selection values */
    int		i, j, k;

    name = UC_opts.filename;

    /* Open the file */
    if((fid = H5Fopen(name, H5F_ACC_RDWR | (UC_opts.use_swmr ? H5F_ACC_SWMR_WRITE : 0), H5P_DEFAULT)) < 0){
	fprintf(stderr, "H5Fopen failed\n");
        return -1;
    }

    /* Open the dataset of the program name */
    if((dsid = H5Dopen2(fid, progname_g, H5P_DEFAULT)) < 0){
	fprintf(stderr, "H5Dopen2 failed\n");
	return -1;
    }

    /* Find chunksize used */
    if ((dcpl = H5Dget_create_plist(dsid)) < 0){
	fprintf(stderr, "H5Dget_create_plist failed\n");
	return -1;
    }
    if (H5D_CHUNKED != H5Pget_layout(dcpl)){
	fprintf(stderr, "storage layout is not chunked\n");
	return -1;
    }
    if ((rank = H5Pget_chunk(dcpl, 3, chunk_dims)) != 3){
	fprintf(stderr, "storage rank is not 3\n");
	return -1;
    }

    /* verify chunk_dims against set paramenters */
    if (chunk_dims[0]!=UC_opts.chunkdims[0] || chunk_dims[1] != cz || chunk_dims[2] != cz){
	fprintf(stderr, "chunk size is not as expected. Got dims=(%ld,%ld,%ld)\n",
	    (long)chunk_dims[0], (long)chunk_dims[1], (long)chunk_dims[2]);
	return -1;
    }

    /* allocate space for data buffer 1 X dims[1] X dims[2] of UC_CTYPE */
    memdims[0]=1;
    memdims[1] = UC_opts.dims[1];
    memdims[2] = UC_opts.dims[2];
    if ((buffer=(UC_CTYPE*)HDmalloc(memdims[1]*memdims[2]*sizeof(UC_CTYPE)))==NULL) {
	fprintf(stderr, "malloc: failed\n");
	return -1;
    };

    /*
     * Get dataset rank and dimension.
     */
    f_sid = H5Dget_space(dsid);    /* Get filespace handle first. */
    rank  = H5Sget_simple_extent_ndims(f_sid);
    if (rank != UC_RANK){
	fprintf(stderr, "rank(%d) of dataset does not match\n", rank);
	return -1;
    }
    if (H5Sget_simple_extent_dims(f_sid, dims, NULL) < 0){
	fprintf(stderr, "H5Sget_simple_extent_dims got error\n");
	return -1;
    }
    printf("dataset rank %d, dimensions %lu x %lu x %lu\n",
	   rank, (unsigned long)(dims[0]), (unsigned long)(dims[1]), (unsigned long)(dims[2]));
    /* verify that file space dims are as expected and are consistent with memory space dims */
    if (dims[0] != 0 || dims[1] != memdims[1] || dims[2] != memdims[2]){
	fprintf(stderr, "dataset is not empty. Got dims=(%ld,%ld,%ld)\n",
	    (long)dims[0], (long)dims[1], (long)dims[2]);
	return -1;
    }
    
    /* setup mem-space for buffer */
    if ((m_sid=H5Screate_simple(rank, memdims, NULL))<0){
	fprintf(stderr, "H5Screate_simple for memory failed\n");
	return -1;
    };

    /* write planes */
    count[0]=1;
    count[1]=dims[1];
    count[2]=dims[2];
    for (i=0; i<UC_opts.nplanes; i++){
	/* fill buffer with value i+1 */
	bufptr = buffer;
	for (j=0; j<dims[1]; j++)
	    for (k=0; k<dims[2]; k++)
		*bufptr++ = i;

	/* extend the dataset by one for new plane */
	dims[0]=i+1;
        if(H5Dset_extent(dsid, dims) < 0){
	    fprintf(stderr, "H5Dset_extent failed\n");
            return -1;
	}

        /* Get the dataset's dataspace */
        if((f_sid = H5Dget_space(dsid)) < 0){
	    fprintf(stderr, "H5Dset_extent failed\n");
            return -1;
	}

	start[0]=i;
        /* Choose the next plane to write */
        if(H5Sselect_hyperslab(f_sid, H5S_SELECT_SET, start, NULL, count, NULL) < 0){
	    fprintf(stderr, "Failed H5Sselect_hyperslab\n");
            return -1;
	}

        /* Write plane to the dataset */
        if(H5Dwrite(dsid, UC_DATATYPE, m_sid, f_sid, H5P_DEFAULT, buffer) < 0){
	    fprintf(stderr, "Failed H5Dwrite\n");
            return -1;
	}
	/* flush file to make the just written plane available. */
#if 0
	if(H5Fflush(fid, H5F_SCOPE_GLOBAL) < 0)
#else
	if(H5Dflush(dsid) < 0)
#endif
	{
	    fprintf(stderr, "Failed to H5Fflush file\n");
	    return -1;
	}
    }

    /* Done writing. Free/Close all resources including data file */
    HDfree(buffer);
    if (H5Dclose(dsid) < 0){
	fprintf(stderr, "Failed to close datasete\n");
	return -1;
    }
    if (H5Sclose(m_sid) < 0){
	fprintf(stderr, "Failed to close memory space\n");
	return -1;
    }
    if (H5Sclose(f_sid) < 0){
	fprintf(stderr, "Failed to close file space\n");
	return -1;
    }
    if (H5Fclose(fid) < 0){
	fprintf(stderr, "Failed to close file id\n");
	return -1;
    }

    return 0;
}


/* Read planes from the dataset.
 * It expects the dataset is being changed (growing).
 * It checks the unlimited dimension (1st one). When it increases,
 * it will read in the new planes, one by one, and verify the data correctness.
 * (The nth plan should contain all "n".)
 * When the unlimited dimension grows to the chunksize (it becomes a cube),
 * that is the expected end of data, the reader exits.
 *
 * Return: 0 succeed; -1 fail.
 */
int read_uc_file(void)
{
    hid_t	fid;          /* File ID for new HDF5 file */
    hid_t	dsid;         /* dataset ID */
    char	*name;
    UC_CTYPE	*buffer, *bufptr;	/* read data buffer */
    int		cz=UC_opts.chunksize;		/* Chunk size */
    hid_t	f_sid;	    /* dataset file space id */
    hid_t	m_sid;	    /* memory space id */
    int		rank;	    /* rank */
    hsize_t	dims[3];    /* Dataspace dimensions */
    hsize_t	memdims[3]; /* Memory space dimensions */
    int		nplane=0, nplane_old=0;	/* nth plane, last nth plane */
    hsize_t	start[3] = {0,0,0}, count[3];    /* Hyperslab selection values */
    int		j, k;
    int		nreadererr=0;
    int		nerrs;
    int		nonewplane;

    name = UC_opts.filename;

    /* Open the file */
    if((fid = H5Fopen(name, H5F_ACC_RDONLY | (UC_opts.use_swmr ? H5F_ACC_SWMR_READ : 0), H5P_DEFAULT)) < 0){
	fprintf(stderr, "H5Fopen failed\n");
        return -1;
    }

    /* Open the dataset of the program name */
    if((dsid = H5Dopen2(fid, progname_g, H5P_DEFAULT)) < 0){
	fprintf(stderr, "H5Dopen2 failed\n");
	return -1;
    }

    /* allocate space for data buffer 1 X dims[1] X dims[2] of UC_CTYPE */
    memdims[0]=1;
    memdims[1] = UC_opts.dims[1];
    memdims[2] = UC_opts.dims[2];
    if ((buffer=(UC_CTYPE*)HDmalloc(memdims[1]*memdims[2]*sizeof(UC_CTYPE)))==NULL) {
	fprintf(stderr, "malloc: failed\n");
	return -1;
    };

    /*
     * Get dataset rank and dimension.
     * Verify dimension is as expected (unlimited,2*chunksize,2*chunksize).
     */
    f_sid = H5Dget_space(dsid);    /* Get filespace handle first. */
    rank  = H5Sget_simple_extent_ndims(f_sid);
    if (rank != UC_RANK){
	fprintf(stderr, "rank(%d) of dataset does not match\n", rank);
	return -1;
    }
    if (H5Sget_simple_extent_dims(f_sid, dims, NULL) < 0){
	fprintf(stderr, "H5Sget_simple_extent_dims got error\n");
	return -1;
    }
    printf("dataset rank %d, dimensions %lu x %lu x %lu\n",
	   rank, (unsigned long)(dims[0]), (unsigned long)(dims[1]), (unsigned long)(dims[2]));
    /* verify that file space dims are as expected and are consistent with memory space dims */
    if (dims[1] != memdims[1] || dims[2] != memdims[2]){
	fprintf(stderr, "dataset dimension is not as expected. Got dims=(%ld,%ld,%ld)\n",
	    (long)dims[0], (long)dims[1], (long)dims[2]);
	fprintf(stderr, "But memdims=(%ld,%ld,%ld)\n",
	    (long)memdims[0], (long)memdims[1], (long)memdims[2]);
	return -1;
    }
    
    /* setup mem-space for buffer */
    if ((m_sid=H5Screate_simple(rank, memdims, NULL))<0){
	fprintf(stderr, "H5Screate_simple for memory failed\n");
	return -1;
    };

    /* Read 1 plane at a time whenever the dataset grows larger
     * (along dim[0]) */
    count[0]=1;
    count[1]=dims[1];
    count[2]=dims[2];
    /* quit when all nplanes, default cz, have been read */
    nonewplane=0;
    while (nplane_old < UC_opts.nplanes ){
	/* print progress message according to if new planes are availalbe */
	if (nplane_old < dims[0]) {
	    if (nonewplane){
		/* end the previous message */
		printf("\n");
		nonewplane=0;
	    }
	    printf("reading planes %d to %d\n", nplane_old, (int)dims[0]);
	}else{
	    if (nonewplane){
		printf(".");
		if (nonewplane>=30){
		    fprintf(stderr, "waited too long for new plane, quit.\n");
		    return -1;
		}
	    }else{
		/* print mesg only the first time; dots still no new plane */
		printf("no new planes to read ");
	    }
	    nonewplane++;
	    /* pause for a second */
	    sleep(1);
	}
	for (nplane=nplane_old; nplane < dims[0]; nplane++){
	    /* read planes between last old nplanes and current extent */
	    /* Get the dataset's dataspace */
	    if((f_sid = H5Dget_space(dsid)) < 0){
		fprintf(stderr, "H5Dget_space failed\n");
		return -1;
	    }

	    start[0]=nplane;
	    /* Choose the next plane to read */
	    if(H5Sselect_hyperslab(f_sid, H5S_SELECT_SET, start, NULL, count, NULL) < 0){
		fprintf(stderr, "H5Sselect_hyperslab failed\n");
		return -1;
	    }

	    /* Read the plane from the dataset */
	    if(H5Dread(dsid, UC_DATATYPE, m_sid, f_sid, H5P_DEFAULT, buffer) < 0){
		fprintf(stderr, "H5Dread failed\n");
		return -1;
	    }

	    /* compare read data with expected data value which is nplane */
	    bufptr = buffer;
	    nerrs=0;
	    for (j=0; j<dims[1]; j++){
		for (k=0; k<dims[2]; k++){
		    if (*bufptr++ != nplane){
			if (++nerrs < ErrorReportMax){
			    fprintf(stderr,
				"found error %d plane(%d,%d), expected %d, got %d\n",
				nplane, j, k, nplane, (int)*(bufptr-1));
			}
		    }
		}
	    }
	    if (nerrs){
		nreadererr++;
		fprintf(stderr, "found %d unexpected values in plane %d\n", nerrs, nplane);
	    }
	}
	/* Have read all current planes */
	nplane_old=dims[0];

	/* check if dataset has grown since last time */
#if 0
	/* close dsid and file, then reopen them */
	if (H5Dclose(dsid) < 0){
	    fprintf(stderr, "H5Dclose failed\n");
	    return -1;
	}
	if (H5Fclose(fid) < 0){
	    fprintf(stderr, "H5Fclose failed\n");
	    return -1;
	}
	if((fid = H5Fopen(name, H5F_ACC_RDONLY | (UC_opts.use_swmr ? H5F_ACC_SWMR_READ : 0), H5P_DEFAULT)) < 0){
	    fprintf(stderr, "H5Fopen failed\n");
	    return -1;
	}
	if((dsid = H5Dopen2(fid, progname_g, H5P_DEFAULT)) < 0){
	    fprintf(stderr, "H5Dopen2 failed\n");
	    return -1;
	}
#else
	H5Drefresh(dsid);
#endif
	f_sid = H5Dget_space(dsid);    /* Get filespace handle first. */
	if (H5Sget_simple_extent_dims(f_sid, dims, NULL) < 0){
	    fprintf(stderr, "H5Sget_simple_extent_dims got error\n");
	    return -1;
	}
    }

    if (nreadererr)
	return -1;
    else
	return 0;
}
