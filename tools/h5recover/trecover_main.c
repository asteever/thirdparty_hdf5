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

/*  
 * This is the Main body of the HDF5 Test program for the h5recover tool.
 * It creates two HDF5 files, one as the Data file, the other the Control file.
 * The two files are intended to contain identical data. The Control file will
 * be completed with no error but the Data file, though written with the same
 * data, is intentional crashed without proper file flush or close.
 * 
 * (The two files are then verified by processes outside of this program.)
 * The crashed Data file is restored by the h5recover tool and compared with
 * the Control file, expecting identical file content up to the last successful
 * file flush or close of the Data file.
 *
 * Creator: Albert Cheng, Jan 28, 2008.
 */
 
#include "trecover.h"

/* Global variables */
CrasherParam_t	AsyncCrashParam;
int		CrashMode = SyncCrash;	/* default to synchronous crash */
hid_t		datafile, ctl_file;    /* data and control file ids */

/* local variables */
#if 0
static int	DSTypes=DSNone;		/* set to none first. */
char *		dsetname=null;		/* dataset name */
#else
static int	DSTypes=DSChunked;	/* set to Chunked temp. first. */
char *		dsetname=CHUNKDATASETNAME;		/* dataset name */
#endif
extern int	PatchMode=0;		/* patch mode, default no. */

int		VerifyRecovery=0; 	/* verify recovery mode */

/* Command arguments parser.
 * 	-a <seconds>	Do Async crash with a floating value of seconds
 */
void
parser(int ac, char **av)
{
    while (--ac > 0) {
	av++;
	if (av[0][0] == '-'){
	    switch (av[0][1]){
	    case 'a':	/* -a async option */
		if (--ac > 0){
		    av++;
		    AsyncCrashParam.tinterval = atof(*av);
		}else{
		    fprintf(stderr, "Missing async time value\n");
		    help();
		    exit(1);
		}
		break;
	    case 'd':	/* -d dataset type */
		if (--ac > 0){
		    av++;
		    switch (**av){
		    case 'C':
			DSTypes=DSTypes|DSContig;
			dsetname=DATASETNAME;
			break;
		    case 'K':
			DSTypes=DSTypes|DSChunked;
			dsetname=CHUNKDATASETNAME;
			break;
		    case 'G':
			DSTypes=DSTypes|DSZip;
			dsetname=ZDATASETNAME;
			break;
		    case 'S':
			DSTypes=DSTypes|DSSZip;
			dsetname=SZDATASETNAME;
			break;
		    case 'A':
			DSTypes=DSTypes|DSAll;
			fprintf(stderr, "option A temporary disabled\n");
			exit(1);
			break;
		    default:
			fprintf(stderr, "Unknown Dataset type (%c)\n", **av);
			help();
			exit(1);
		    }
		}else{
		    fprintf(stderr, "Missing data type value\n");
		    help();
		    exit(1);
		}
		break;
	    case 'p':	/* patch option */
		PatchMode=1;
		break;
	    case 'v':   /* verify option */
		VerifyRecovery = 1;
		break;
	    case 'h':	/* -h help option */
		help();
		exit(0);
		break;
	    default:
		fprintf(stderr, "Unknown command option(%s)\n", *av);
		help();
		exit(1);
	    }
	}else{
	    fprintf(stderr, "Unknown command option(%s)\n", *av);
	    help(); 
	    exit(1);
	}
    } /* end of while */

    if ( ( VerifyRecovery ) &&
	 ( ( AsyncCrashParam.tinterval > 0 ) ||
	   ( DSTypes != DSChunked ) ||
	   ( PatchMode != 0 ) ) ) {

        fprintf(stderr, 
		"-v option cannot be combined with other options\n");
        help(); 
        exit(1);
    }

    if (DSTypes==DSNone){
	/* reset to default all datasets */
	DSTypes=DSAll;
    }
}


/* initialization */
void
init(void)
{
    AsyncCrashParam.tinterval = 0;
}


/* show help pages */
void
help(void)
{
    fprintf(stderr, 
        "Usage: trecover ([-a <seconds>] [-d <dataset-type>] [-h]) | [-v]\n"
	"\t-a\tAsync crash seconds where <seconds> is a real number.\n"
	"\t-d\tDataset to create. <dataset-type> can be:\n"
	"\t\t  A\tAll datasets\n"
	"\t\t  C\tContingous datasets\n"
	"\t\t  G\tGzip compressed datasets\n"
	"\t\t  K\tChunked datasets\n"
	"\t\t  S\tSzip compressed datasets\n"
	"\t\tDefault is all datasets\n"
	"\t\tTemp Default is Chunked datasets\n"
	"\t-v\tVerify recovery -- may not be combined with any other option.\n"
   );
}


int
main (int ac, char **av)
{
    int ret_val = 0;
    hsize_t     dims[RANK]={NX,NY};              /* dataset dimensions */
    hsize_t     dimschunk[RANK]={ChunkX,ChunkY}; /* dataset chunk dimensions */

    init();
    parser(ac, av);

    if ( VerifyRecovery ) {

        ret_val = verify_recovery();

    } else {

        if (!PatchMode){
	    /* create/open both files. */
	    create_files(H5FILE_NAME, CTL_H5FILE_NAME);

	    /* Create datasets in both Control file and data files.  
	     * Close both. 
	     */
	    create_dataset(ctl_file, DSTypes, RANK, dims, dimschunk);
	    close_file(ctl_file);
	    create_dataset(datafile, DSTypes, RANK, dims, dimschunk);
	    close_file(datafile);
        }

        /* Open data file with Journaling and control file without. */
        journal_files(H5FILE_NAME, CTL_H5FILE_NAME, JNL_H5FILE_NAME, PatchMode);
    
        if (PatchMode){
	    /* extend the datafile again without writing data, then close it. */
	    extend_dataset(datafile, NX, 4*NX-1, PatchMode);
	    close_file(datafile);
	    close_file(ctl_file);
        }else{
	    /* Extend control file, then close it. */
	    extend_dataset(ctl_file, NX, 4*NX-1, 0);
	    close_file(ctl_file);

	    /* Schedule Async crash if requested. */
	    if (AsyncCrashParam.tinterval > 0)
	        crasher(AsyncCrash, &AsyncCrashParam);

	    /* Extend datafile, then close it. */
	    extend_dataset(datafile, NX, 4*NX-1, 0);

	    /* Do a sync crash. */
	    if (AsyncCrashParam.tinterval == 0)
	        CRASH;

	    /* Close file only if Async crash is scheduled but has 
	     * not occurred yet. 
	     */
	    close_file(datafile);
        }
    }

    return(ret_val);
}     
