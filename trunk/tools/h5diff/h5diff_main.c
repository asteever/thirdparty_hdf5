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

#include "h5diff.h"
#include "ph5diff.h"
#include <stdlib.h>
#include <assert.h>

static void usage(void);
static int check_n_input( const char* );
static int check_f_input( const char* );
void h5diff_exit(int status);
#ifdef H5_HAVE_PARALLEL
static void ph5diff_worker( void );
#endif

/*-------------------------------------------------------------------------
 * Function: main
 *
 * Purpose: h5diff/ph5diff main program
 *
 * Return: An  exit status of 0 means no differences were found, 1 means some 
 *   differences were found.
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: May 9, 2003
 *
 * Comments:
 *
 * Modifications: July 2004
 *  Introduced the four modes:
 *   Normal mode: print the number of differences found and where they occured
 *   Report mode: print the above plus the differences
 *   Verbose mode: print the above plus a list of objects and warnings
 *   Quiet mode: do not print output 
 *
 * November 2004: Leon Arber (larber@uiuc.edu)
 * 		  Additions that allow h5diff to be run in parallel
 *
 * This function drives the diff process and will do a serial or parallel diff depending 
 * on the value of the global variable g_Parallel (default is 0), set to 1 when the program
 * is run as "ph5diff"
 *-------------------------------------------------------------------------
 */

    int	       nID = 0;

int main(int argc, const char *argv[])
{
    int        i;
    const char *s = NULL;
    const char *fname1 = NULL;
    const char *fname2 = NULL;
    const char *objname1  = NULL;
    const char *objname2  = NULL;
    hsize_t    nfound=0;
    int        ret;
    diff_opt_t options;

#ifdef H5_HAVE_PARALLEL
    MPI_Status Status;
#endif

    /* See what we were called as to determine whether to run serial or parallel version
     *
     * It has been determined that:
     * If argv[0] is greater than 6 characters AND the last 7 equal "ph5diff" we run parallel
     * In all other cases, we run serial */

/*printf("argv[0]=%s\n", argv[0]);*/
    if( (strlen(argv[0]) > strlen("h5diff")) && (strcmp(argv[0] + (strlen(argv[0]) - strlen("ph5diff")), "ph5diff") == 0) )
	g_Parallel = 1;


    if(g_Parallel)
    {
#ifdef H5_HAVE_PARALLEL
	MPI_Init(&argc, (char***) &argv);

	MPI_Comm_rank(MPI_COMM_WORLD, &nID);
	MPI_Comm_size(MPI_COMM_WORLD, &g_nTasks);
#else
	printf("You cannot run ph5diff unless you compile a parallel build of HDF5\n");
	h5diff_exit(2);
#endif
    }
    else
	g_nTasks = 1;

    /* Have the manager process the command-line */
    if(nID == 0)
    {
	memset(&options, 0, sizeof (diff_opt_t));

	/*-------------------------------------------------------------------------
	 * initial check of command line options
	 *-------------------------------------------------------------------------
	 */

	if ( argc==2 && (strcmp("-h",argv[1])==0) ) 
	    usage();

	if ( argc<3 ) 
	{
	    printf("Number of arguments is only %d\n", argc );
	    usage();
	}

	/*-------------------------------------------------------------------------
	 * file names are first
	 *-------------------------------------------------------------------------
	 */
	if ( argc>=3 )
	{
	    fname1 = argv[1];
	    fname2 = argv[2];
	}
	/*-------------------------------------------------------------------------
	 * parse command line options
	 *-------------------------------------------------------------------------
	 */
	for (i=3; i<argc ; i++) 
	{
	    /* get the single-letter switches */
	    if ( '-'==argv[i][0] )
	    {
		for (s=argv[i]+1; *s; s++) 
		{
		    switch (*s) {
			default:
			    printf("-%s is an invalid option\n", s );
			    usage();
			    break;
			case 'h': 
			    usage();
			    break;
			case 'v': 
			    options.m_verbose = 1;
			    break;
			case 'q': 
			    /* use quiet mode; supress the message "0 differences found" */
			    options.m_quiet = 1;
			    break;
			case 'r': 
			    options.m_report = 1;
			    break;
			case 'd': 
			    /* if it is not another option */
			    if ( i<argc-1 &&'-' != argv[i+1][0] )
			    {
				options.d=1;
				if ( check_f_input(argv[i+1])==-1)
				{
				    printf("<-d %s> is not a valid option\n", argv[i+1] );
				    usage();
				}
				options.delta = atof(argv[i+1]);
				i++; /* go to next */
			    }
			    else
			    {
				printf("Not a valid -d option\n");
				usage();
			    }
			    break;
			case 'p': 
			    if ( i<argc-1 &&'-' !=argv[i+1][0] )
			    {
				options.p=1;
				if ( check_f_input(argv[i+1])==-1)
				{
				    printf("<-p %s> is not a valid option\n", argv[i+1] );
				    usage();
				}
				options.percent = atof(argv[i+1]);
				i++; /* go to next */
			    }
			    else
			    {
				printf("Not a valid -p option\n");
				usage();
			    }
			    break;
			case 'n': 
			    if ( i<argc-1 && '-' !=argv[i+1][0] )
			    {
				options.n=1;
				if ( check_n_input(argv[i+1])==-1)
				{
				    printf("<-n %s> is not a valid option\n", argv[i+1] );
				    usage();
				}
				options.count = atoi(argv[i+1]);
				i++; /* go to next */
			    }
			    else
			    {
				printf("Not a valid -n option\n");
				usage();
			    }
			    break;
		    } /*switch*/
		} /*for*/ 
	    } /*if*/

	    else /* not single-letter switches */

	    {
		/* check if it is not a -d, -p parameter */
		if ( '-'==argv[i-1][0] && ('d'==argv[i-1][1] ||'p'==argv[i-1][1] ))
		    continue;
		else
		{
		    if ( objname1==NULL )
			objname1 = argv[i];
		    if ( objname2==NULL )
		    {
			/* check if we have a second object name */
			if ( i+1<argc && '-' !=argv[i+1][0] ) {
			    /* yes */
			    objname2 = argv[i+1];
			    i++; /* go to next */
			}
			else
			    /* no */
			    objname2 = objname1;
		    } /*objname2*/
		} /*else*/
	    } /*else*/

	}/*for*/

	nfound = h5diff(fname1,fname2,objname1,objname2,&options);

#ifdef H5_HAVE_PARALLEL
	if(g_nTasks > 1)
	    MPI_Barrier(MPI_COMM_WORLD);
#endif

	/*-------------------------------------------------------------------------
	 * print how many differences were found
	 *-------------------------------------------------------------------------
	 */
	if (!options.m_quiet) 
	{
	    if (options.cmn_objs==0)
	    {
		printf("No common objects found. Files are not comparable.\n");
		if (!options.m_verbose)
		    printf("Use -v for a list of objects.\n");
	    }
	    else
	    {
		if (!options.err_stat)
		{
		    print_found(nfound);
		    print_manager_output();
		}
	    }
	}

	/*-------------------------------------------------------------------------
	 * exit code 
	 *   >0 if differences, 0 if no differences, <0 if error
	 *-------------------------------------------------------------------------
	 */

#ifdef H5_HAVE_PARALLEL
	if(g_Parallel)
	    MPI_Finalize();
#endif

	ret= (nfound==0 ? 0 : 1 );
	if (options.err_stat)
	    ret=-1;
	return ret;
    }
#ifdef H5_HAVE_PARALLEL
    /* All other tasks become workers and wait for assignments. */
    else
	ph5diff_worker();
#endif
}

#ifdef H5_HAVE_PARALLEL
/*-------------------------------------------------------------------------
 * Function: ph5diff_worker
 *
 * Purpose: worker process of ph5diff
 *
 * Return: none
 *
 * Programmer: Leon Arber
 * Date: January 2005
 *
 * Comments:
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void
ph5diff_worker(void)
{	
    struct diff_args args;
    hid_t file1_id, file2_id;	
    char	filenames[2][1024];
    hsize_t    nfound=0;
    int	       nID;
    MPI_Status Status;

    MPI_Comm_rank(MPI_COMM_WORLD, &nID);
    outBuffOffset = 0;
    
    MPI_Recv(filenames, 1024*2, MPI_CHAR, 0, MPI_ANY_TAG, MPI_COMM_WORLD, &Status);
    if(Status.MPI_TAG == MPI_TAG_PARALLEL)
    {
	/*printf("We're in parallel mode...opening the files\n");*/

	/* disable error reporting */
	H5E_BEGIN_TRY
	{
	    /* Open the files */
	    if ((file1_id = H5Fopen (filenames[0], H5F_ACC_RDONLY, H5P_DEFAULT)) < 0)
	    {
		printf ("h5diff Task [%d]: <%s>: unable to open file\n", nID, filenames[0]);
		MPI_Abort(MPI_COMM_WORLD, 0);
	    }
	    if ((file2_id = H5Fopen (filenames[1], H5F_ACC_RDONLY, H5P_DEFAULT)) < 0)
	    {
		printf ("h5diff Task [%d]: <%s>: unable to open file\n", nID, filenames[1]);
		MPI_Abort(MPI_COMM_WORLD, 0);
	    }
	    /* enable error reporting */
	}
	H5E_END_TRY;


	while(1)
	{
	    MPI_Probe(0, MPI_ANY_TAG, MPI_COMM_WORLD, &Status);

	    if(Status.MPI_TAG == MPI_TAG_ARGS)
	    {
		/*Recv parameters for diff from manager task */
		MPI_Recv(&args, sizeof(struct diff_args), MPI_BYTE, 0, MPI_TAG_ARGS, MPI_COMM_WORLD, &Status);
		/*Do the diff */
		nfound = diff(file1_id, args.name, file2_id, args.name, &(args.options), args.type);

		/*If print buffer has something in it, request print token.*/
		if(outBuffOffset>0)
		{
		    MPI_Send(NULL, 0, MPI_BYTE, 0, MPI_TAG_TOK_REQUEST, MPI_COMM_WORLD);

		    /*Wait for print token. */
		    MPI_Recv(NULL, 0, MPI_BYTE, 0, MPI_TAG_PRINT_TOK, MPI_COMM_WORLD, &Status);

		    /*When get token, print stuff out and return token */
		    printf("%s", outBuff);
		    fflush(stdout);
		    memset(outBuff, 0, OUTBUFF_SIZE);
		    outBuffOffset = 0;

		    MPI_Send(&nfound, 1, MPI_LONG_LONG, 0, MPI_TAG_TOK_RETURN, MPI_COMM_WORLD);
		}
		else
		    MPI_Send(&nfound, 1, MPI_LONG_LONG, 0, MPI_TAG_DONE, MPI_COMM_WORLD);

	    }
	    else if(Status.MPI_TAG == MPI_TAG_END)
	    {
		MPI_Recv(NULL, 0, MPI_BYTE, 0, MPI_TAG_END, MPI_COMM_WORLD, &Status);
	/*	printf("exiting..., task: %d\n", nID); */
		break;
	    }
	    else
	    {
		printf("ERROR....invalid tag received\n");
		MPI_Abort(MPI_COMM_WORLD, 0);
	    }

	}
    }

    MPI_Barrier(MPI_COMM_WORLD);
    MPI_Finalize();	
}
#endif

/*-------------------------------------------------------------------------
 * Function: check_n_input
 *
 * Purpose: check for valid input
 *
 * Return: 1 for ok, -1 for fail
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: May 9, 2003
 *
 * Comments:
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
    static 
int check_n_input( const char *str )
{
    unsigned i;
    char c;

    for ( i = 0; i < strlen(str); i++)
    {
	c = str[i];
	if ( i==0 )
	{
	    if ( c < 49 || c > 57  ) /* ascii values between 1 and 9 */
		return -1;
	}
	else
	    if ( c < 48 || c > 57  ) /* 0 also */
		return -1;
    }
    return 1;
}

/*-------------------------------------------------------------------------
 * Function: check_f_input
 *
 * Purpose: check for a valid floating point input
 *
 * Return: 1 for ok, -1 for fail
 *
 * Date: May 9, 2003
 *
 * Comments:
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
    static 
int check_f_input( const char *str )
{
    double x;

    /* 
       the atof return value on a hexadecimal input is different 
       on some systems; we do a character check for this
     */
    if (strlen(str)>2 && str[0]=='0' && str[1]=='x')
	return -1;

    x=atof(str);
    if (x==0)
	return -1;

    return 1;
}

/*-------------------------------------------------------------------------
 * Function: usage
 *
 * Purpose: print a usage message  
 *
 * Return: void
 *
 *-------------------------------------------------------------------------
 */
    static 
void usage(void)
{
    printf("Usage: h5diff file1 file2 [OPTIONS] [obj1[obj2]] \n");
    printf("\n");
    printf("file1             File name of the first HDF5 file\n");
    printf("file2             File name of the second HDF5 file\n");
    printf("[obj1]            Name of an HDF5 object, in absolute path\n");
    printf("[obj2]            Name of an HDF5 object, in absolute path\n");
    printf("[OPTIONS] are:\n");
    printf("[-h]              Print out this information\n");
    printf("[-r]              Report mode. Print the differences\n");
    printf("[-v]              Verbose mode. Print the differences, list of objects, warnings\n");
    printf("[-q]              Quiet mode. Do not do output\n");
    printf("[-n count]        Print difference up to count number\n");
    printf("[-d delta]        Print difference when it is greater than limit delta\n");
    printf("[-p relative]     Print difference when it is greater than a relative limit\n");
    printf("\n");
    printf("Items in [] are optional\n");
    printf("[obj1] and [obj2] are HDF5 objects (datasets, groups or datatypes)\n");
    printf("The 'count' value must be a positive integer\n");
    printf("The 'delta' and 'relative' values must be positive numbers\n");
    printf("The -d compare criteria is |a - b| > delta\n");
    printf("The -p compare criteria is |1 - b/a| > relative\n");
    printf("\n");
    printf("h5diff has four modes of output:\n");
    printf(" Normal mode: print the number of differences found and where they occured\n");
    printf(" Report mode: print the above plus the differences\n");
    printf(" Verbose mode: print the above plus a list of objects and warnings\n");
    printf(" Quiet mode: do not print output (h5diff always returns an exit code of 1 when differences are found)\n");
    printf("\n");
    printf("Examples of use:\n");
    printf("\n");
    printf("1) h5diff file1 file2 /g1/dset1 /g1/dset2\n");
    printf("\n");
    printf("   Compares object '/g1/dset1' in file1 with '/g1/dset2' in file2\n");
    printf("\n");
    printf("2) h5diff file1 file2 /g1/dset1\n");
    printf("\n");
    printf("   Compares object '/g1/dset1' in both files\n");
    printf("\n");
    printf("3) h5diff file1 file2\n");
    printf("\n");
    printf("   Compares all objects in both files\n");
    printf("\n");
    printf("Note)  file1 and file2 can be the same file. Use\n");
    printf("\n");
    printf("   h5diff file1 file1 /g1/dset1 /g1/dset2\n");
    printf("\n");
    printf("   to compare '/g1/dset1' and '/g1/dset2' in the same file\n");
    h5diff_exit(0);
}


/*-------------------------------------------------------------------------
 * Function: h5diff_exit
 *
 * Purpose: dismiss phdiff worker processes and exit
 *
 * Return: none
 *
 * Programmer: Albert Cheng
 * Date: Feb 6, 2005
 *
 * Comments:
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void h5diff_exit(int status)
{

#ifdef H5_HAVE_PARALLEL
    /* if in parallel mode, dismiss workers, close down MPI, then exit */
    if(g_nTasks > 1){
	phdiff_dismiss_workers();
	MPI_Barrier(MPI_COMM_WORLD);
    }
    if(g_Parallel)
	MPI_Finalize();
#endif
    exit(status);
}

