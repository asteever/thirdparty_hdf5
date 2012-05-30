#include "swmr_common.h"
#include <unistd.h>

#define TIMEOUT 300

static hid_t symbol_tid = (-1);

static int
check_dataset(hid_t fid, unsigned verbose, const symbol_info_t *symbol, symbol_t *record,
    hid_t rec_sid)
{
    hid_t dsid;                 /* Dataset ID */
    hid_t file_sid;             /* Dataset's space ID */
    hsize_t start[2] = {0, 0}, count[2] = {1, 1};   /* Hyperslab selection values */

    /* Open dataset for symbol */
    if((dsid = H5Dopen2(fid, symbol->name, H5P_DEFAULT)) < 0)
        return(-1);

    /* Get the dataset's dataspace */
    if((file_sid = H5Dget_space(dsid)) < 0)
        return(-1);

    /* Choose the random record in the dataset (will be the same as chosen by
     * the writer) */
    start[1] = (hsize_t)random() % symbol->nrecords;
    if(H5Sselect_hyperslab(file_sid, H5S_SELECT_SET, start, NULL, count, NULL) < 0)
        return(-1);

    /* Emit informational message */
    if(verbose)
        printf("Symbol = '%s', location = %lld\n", symbol->name, (long long)start);

    /* Read record from dataset */
#ifdef OHDR_DEPS_WORK
    /* Even with the sequence number attribute and all the flush dependencues,
     * it is still currently possible for the attribute to be updated before the
     * index and/or raw data, because the attribute may reside in an object
     * header chunk afer the first.  Until this is fixed, just allow the read
     * value to be 0. */
    record->rec_id = (uint64_t)ULLONG_MAX;
#else /* OHDR_DEPS_WORK */
    record->rec_id = (uint64_t)0;
#endif /* OHDR_DEPS_WORK */
    if(H5Dread(dsid, symbol_tid, rec_sid, file_sid, H5P_DEFAULT, record) < 0)
        return(-1);

    /* Verify record value */
    if(record->rec_id != start[1]
#ifndef OHDR_DEPS_WORK
             && record->rec_id != (uint64_t)0
#endif
            ) {
        printf("Incorrect record value!\n");
        printf("Symbol = '%s', location = %lld, record->rec_id = %llu\n", symbol->name, (long long)start, (unsigned long long)record->rec_id);
        return(-1);
    } /* end if */

    /* Close the dataset's dataspace */
    if(H5Sclose(file_sid) < 0)
        return(-1);

    /* Close dataset for symbol */
    if(H5Dclose(dsid) < 0)
        return(-1);

    return(0);
} /* end check_dataset() */

static int
read_records(const char *filename, unsigned verbose, unsigned long nrecords,
    unsigned poll_time, unsigned reopen_count)
{
    hid_t fid;                  /* File ID */
    hid_t aid;                  /* Attribute ID */
    time_t start_time;          /* Starting time */
    hid_t mem_sid;              /* Memory dataspace ID */
    symbol_t record;            /* The record to add to the dataset */
    unsigned seed;              /* Seed for random number generator */
    unsigned iter_to_reopen = reopen_count; /* # of iterations until reopen */
    unsigned long u;            /* Local index variable */
hid_t fapl;
fapl = H5Pcreate(H5P_FILE_ACCESS);
H5Pset_fclose_degree(fapl, H5F_CLOSE_SEMI);
    /* Emit informational message */
        if(verbose)
            printf("Opening file: %s\n", filename);

    /* Open the file */
    if((fid = H5Fopen(filename, H5F_ACC_RDONLY | H5F_ACC_SWMR_READ, fapl)) < 0)
        return(-1);

    /* Seed the random number generator with the attribute in the file */
    if((aid = H5Aopen(fid, "seed", H5P_DEFAULT)) < 0)
        return(-1);
    if(H5Aread(aid, H5T_NATIVE_UINT, &seed) < 0)
        return(-1);
    if(H5Aclose(aid) < 0)
        return(-1);
    srandom(seed);

    /* Reset the record */
    /* (record's 'info' field might need to change for each record written, also) */
    memset(&record, 0, sizeof(record));

    /* Create a dataspace for the record to read */
    if((mem_sid = H5Screate(H5S_SCALAR)) < 0)
        return(-1);

    /* Emit informational message */
    if(verbose)
        printf("Reading records\n");

    /* Get the starting time */
    start_time = time(NULL);

    /* Read records */
    for(u = 0; u < nrecords; u++) {
        symbol_info_t *symbol = NULL;   /* Symbol (dataset) */
        htri_t attr_exists;             /* Whether the sequence number attribute exists */
        unsigned long file_u;           /* Attribute sequence number (writer's "u") */

        /* Get a random dataset, according to the symbol distribution */
        symbol = choose_dataset();

        /* Fill in "nrecords" field.  Note that this depends on the writer
         * using the same algorithm and "nrecords" */
        symbol->nrecords = nrecords / 5;

        /* Wait until we can read the dataset */
        do {
            /* Check if sequence attribute exists */
            if((attr_exists = H5Aexists_by_name(fid, symbol->name, "seq", H5P_DEFAULT)) < 0)
                return(-1);

            if(attr_exists) {
                /* Read sequence number attribute */
                if((aid = H5Aopen_by_name(fid, symbol->name, "seq", H5P_DEFAULT, H5P_DEFAULT)) < 0)
                    return(-1);
                if(H5Aread(aid, H5T_NATIVE_ULONG, &file_u) < 0)
                    return(-1);
                if(H5Aclose(aid) < 0)
                    return(-1);

                /* Check if sequence number is at least u - if so, this should
                 * guarantee that this record has been written */
                if(file_u >= u)
                    break;
            } /* end if */

            /* Check for timeout */
            if(time(NULL) >= (time_t)(start_time + (time_t)TIMEOUT)) {
                printf("Reader timed out\n");
                return(-1);
            } /* end if */

            /* Pause */
            sleep(poll_time);

            /* Reopen the file */
            if(H5Fclose(fid) < 0)
                return(-1);
            if((fid = H5Fopen(filename, H5F_ACC_RDONLY | H5F_ACC_SWMR_READ, fapl)) < 0)
                return(-1);
            iter_to_reopen = reopen_count;
        } while(1);

        /* Emit informational message */
        if(verbose)
            printf("Checking dataset %lu\n", u);

        /* Check dataset */
        if(check_dataset(fid, verbose, symbol, &record, mem_sid) < 0)
            return(-1);

        /* Check for reopen */
        iter_to_reopen--;
        if(iter_to_reopen == 0) {
            /* Emit informational message */
            if(verbose)
                printf("Reopening file: %s\n", filename);

            /* Reopen the file */
            if(H5Fclose(fid) < 0)
                return(-1);
            if((fid = H5Fopen(filename, H5F_ACC_RDONLY | H5F_ACC_SWMR_READ, fapl)) < 0)
                return(-1);
            iter_to_reopen = reopen_count;
        } /* end if */
    } /* end while */

    /* Close file */
    if(H5Fclose(fid) < 0)
        return(-1);

    /* Close the memory dataspace */
    if(H5Sclose(mem_sid) < 0)
        return(-1);

    return(0);
} /* end read_records() */

static void
usage(void)
{
    printf("Usage error!\n");
    printf("Usage: swmr_sparse_reader [-q] [-s <# of seconds to wait for writer>] [-r <# of reads between reopens>] <# of records>\n");
    printf("Defaults to verbose (no '-q' given), 1 second wait ('-s 1') and 1 read between reopens ('-r 1')\n");
    printf("Note that the # of records *must* be the same as that supplied to swmr_sparse_writer\n");
    exit(1);
} /* end usage() */

int main(int argc, const char *argv[])
{
    long nrecords = 0;  /* # of records to read */
    int poll_time = 1;  /* # of seconds to sleep when waiting for writer */
    int reopen_count = 1; /* # of reads between reopens */
    unsigned verbose = 1;       /* Whether to emit some informational messages */
    unsigned u;         /* Local index variables */

    /* Parse command line options */
    if(argc < 2)
        usage();
    if(argc > 1) {
        u = 1;
        while(u < (unsigned)argc) {
            if(argv[u][0] == '-') {
                switch(argv[u][1]) {
                    /* # of reads between reopens */
                    case 'r':
                        reopen_count = atoi(argv[u + 1]);
                        if(reopen_count < 0)
                            usage();
                        u += 2;
                        break;

                    /* Be quiet */
                    case 'q':
                        verbose = 0;
                        u++;
                        break;

                    /* # of seconds between polling */
                    case 's':
                        poll_time = atoi(argv[u + 1]);
                        if(poll_time < 0)
                            usage();
                        u += 2;
                        break;

                    default:
                        usage();
                        break;
                } /* end switch */
            } /* end if */
            else {
                /* Get the number of records to read */
                nrecords = atol(argv[u]);
                if(nrecords <= 0)
                    usage();

                u++;
            } /* end else */
        } /* end while */
    } /* end if */

    /* Emit informational message */
    if(verbose) {
        printf("Parameters:\n");
        printf("\t# of seconds between polling = %d\n", poll_time);
        printf("\t# of reads between reopens = %d\n", reopen_count);
        printf("\t# of records to read = %ld\n", nrecords);
    } /* end if */

    /* Emit informational message */
    if(verbose)
        printf("Generating symbol names\n");

    /* Generate dataset names */
    if(generate_symbols() < 0) {
        printf("Error generating symbol names!\n");
        exit(1);
    } /* end if */

    /* Create datatype for creating datasets */
    if((symbol_tid = create_symbol_datatype()) < 0)
        return(-1);

    /* Reading records from datasets */
    if(read_records(FILENAME, verbose, (unsigned long) nrecords, (unsigned)poll_time, (unsigned)reopen_count) < 0) {
        printf("Error reading records from datasets!\n");
        exit(1);
    } /* end if */

    /* Emit informational message */
    if(verbose)
        printf("Releasing symbols\n");

    /* Clean up the symbols */
    if(shutdown_symbols() < 0) {
        printf("Error releasing symbols!\n");
        exit(1);
    } /* end if */

    /* Emit informational message */
    if(verbose)
        printf("Closing objects\n");

    /* Close objects created */
    if(H5Tclose(symbol_tid) < 0) {
        printf("Error closing symbol datatype!\n");
        exit(1);
    } /* end if */

    return(0);
}

