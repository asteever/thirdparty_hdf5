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

/*-------------------------------------------------------------------------
 *
 * Created:             debug.c
 *                      Jul 18 1997
 *                      Robb Matzke <matzke@llnl.gov>
 *
 * Purpose:             Debugs an existing HDF5 file at a low level.
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
#define H5B2_PACKAGE		/*suppress error about including H5B2pkg  */
#define H5B2_TESTING		/*suppress warning about H5B2 testing funcs*/
#define H5F_PACKAGE		/*suppress error about including H5Fpkg	  */
#define H5FS_PACKAGE		/*suppress error about including H5FSpkg  */
#define H5HF_PACKAGE		/*suppress error about including H5HFpkg  */
#define H5O_PACKAGE		/*suppress error about including H5Opkg	  */

#include "H5private.h"		/* Generic Functions			*/
#include "H5Bprivate.h"		/* B-trees				*/
#include "H5B2pkg.h"		/* v2 B-trees				*/
#include "H5Dprivate.h"		/* Datasets				*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5Fpkg.h"             /* File access				*/
#include "H5FSpkg.h"		/* File free space			*/
#include "H5Gprivate.h"		/* Groups				*/
#include "H5HFpkg.h"		/* Fractal heaps			*/
#include "H5HGprivate.h"	/* Global Heaps				*/
#include "H5HLprivate.h"	/* Local Heaps				*/
#include "H5Iprivate.h"		/* IDs			  		*/
#include "H5Opkg.h"             /* Object headers			*/
#include "H5Pprivate.h"		/* Property lists			*/

/* File drivers */
#include "H5FDfamily.h"

#define INDENT  3
#define VCOL    50
#define SIGLEN  H5F_SIGNATURE_LEN       /* Size of signature buffer */


/*-------------------------------------------------------------------------
 * Function:    main
 *
 * Usage:       debug FILENAME [OFFSET]
 *
 * Return:      Success:        exit (0)
 *
 *              Failure:        exit (non-zero)
 *
 * Programmer:  Robb Matzke
 *              matzke@llnl.gov
 *              Jul 18 1997
 *
 *-------------------------------------------------------------------------
 */
int
main(int argc, char *argv[])
{
    hid_t	fid, fapl, dxpl;
    H5F_t       *f;
    haddr_t     addr = 0, extra = 0, extra2 = 0;
    uint8_t     sig[SIGLEN];
    size_t      u;
    herr_t      status = SUCCEED;

    if(argc == 1) {
	fprintf(stderr, "Usage: %s filename [signature-addr [extra]]\n", argv[0]);
	HDexit(1);
    } /* end if */

    /* Initialize the library */
    if(H5open() < 0) {
        fprintf(stderr, "cannot initialize the library\n");
        HDexit(1);
    } /* end if */

    /*
     * Open the file and get the file descriptor.
     */
    if((dxpl = H5Pcreate (H5P_DATASET_XFER))<0) {
        fprintf(stderr, "cannot create dataset transfer property list\n");
        HDexit(1);
    } /* end if */
    if((fapl = H5Pcreate (H5P_FILE_ACCESS))<0) {
        fprintf(stderr, "cannot create file access property list\n");
        HDexit(1);
    } /* end if */
    if(strchr (argv[1], '%'))
	H5Pset_fapl_family (fapl, (hsize_t)0, H5P_DEFAULT);
    if((fid = H5Fopen(argv[1], H5F_ACC_RDONLY, fapl)) < 0) {
        fprintf(stderr, "cannot open file\n");
        HDexit(1);
    } /* end if */
    if(NULL == (f = H5I_object(fid))) {
        fprintf(stderr, "cannot obtain H5F_t pointer\n");
        HDexit(2);
    } /* end if */

    /*
     * Parse command arguments.
     */
    if(argc > 2)
        addr = HDstrtoll(argv[2], NULL, 0);
    if(argc > 3)
        extra = HDstrtoll(argv[3], NULL, 0);
    if(argc > 4)
        extra2 = HDstrtoll(argv[4], NULL, 0);

    /*
     * Read the signature at the specified file position.
     */
    HDfprintf(stdout, "Reading signature at address %a (rel)\n", addr);
    if(H5F_block_read(f, H5FD_MEM_SUPER, addr, sizeof(sig), dxpl, sig) < 0) {
        fprintf(stderr, "cannot read signature\n");
        HDexit(3);
    }
    if(!HDmemcmp(sig, H5F_SIGNATURE, H5F_SIGNATURE_LEN)) {
        /*
         * Debug the file's super block.
         */
        status = H5F_debug(f, H5P_DATASET_XFER_DEFAULT, stdout, 0, VCOL);

    } else if(!HDmemcmp(sig, H5HL_MAGIC, H5HL_SIZEOF_MAGIC)) {
        /*
         * Debug a local heap.
         */
        status = H5HL_debug(f, H5P_DATASET_XFER_DEFAULT, addr, stdout, 0, VCOL);

    } else if(!HDmemcmp (sig, H5HG_MAGIC, H5HG_SIZEOF_MAGIC)) {
	/*
	 * Debug a global heap collection.
	 */
	status = H5HG_debug (f, H5P_DATASET_XFER_DEFAULT, addr, stdout, 0, VCOL);

    } else if(!HDmemcmp(sig, H5G_NODE_MAGIC, H5G_NODE_SIZEOF_MAGIC)) {
        /*
         * Debug a symbol table node.
         */

        /* Check for extra parameters */
        if(extra == 0) {
            fprintf(stderr, "\nWarning: Providing the group's local heap address will give more information\n");
            fprintf(stderr, "Symbol table node usage:\n");
            fprintf(stderr, "\th5debug <filename> <Symbol table node address> <address of local heap>\n\n");
        } /* end if */

        status = H5G_node_debug(f, H5P_DATASET_XFER_DEFAULT, addr, stdout, 0, VCOL, extra);

    } else if(!HDmemcmp(sig, H5B_MAGIC, H5B_SIZEOF_MAGIC)) {
        /*
         * Debug a B-tree.  B-trees are debugged through the B-tree
         * subclass.  The subclass identifier is the byte immediately
         * after the B-tree signature.
         */
        H5B_subid_t subtype = (H5B_subid_t)sig[H5B_SIZEOF_MAGIC];
        unsigned    ndims;

        switch(subtype) {
            case H5B_SNODE_ID:
                /* Check for extra parameters */
                if(extra == 0) {
                    fprintf(stderr, "\nWarning: Providing the group's local heap address will give more information\n");
                    fprintf(stderr, "B-tree symbol table node usage:\n");
                    fprintf(stderr, "\th5debug <filename> <B-tree node address> <address of local heap>\n\n");
                } /* end if */

                status = H5G_node_debug(f, H5P_DATASET_XFER_DEFAULT, addr, stdout, 0, VCOL, extra);
                break;

            case H5B_ISTORE_ID:
                /* Check for extra parameters */
                if(extra == 0) {
                    fprintf(stderr, "ERROR: Need number of dimensions of chunk in order to dump chunk B-tree node\n");
                    fprintf(stderr, "B-tree chunked storage node usage:\n");
                    fprintf(stderr, "\th5debug <filename> <B-tree node address> <# of dimensions>\n");
                    HDexit(4);
                } /* end if */

                ndims = (unsigned)extra;
                status = H5D_istore_debug (f, H5P_DATASET_XFER_DEFAULT, addr, stdout, 0, VCOL, ndims);
                break;

            default:
                fprintf(stderr, "Unknown B-tree subtype %u\n", (unsigned)(subtype));
                HDexit(4);
        }

    } else if(!HDmemcmp(sig, H5B2_HDR_MAGIC, H5B2_SIZEOF_MAGIC)) {
        /*
         * Debug a v2 B-tree.  B-trees are debugged through the B-tree
         * subclass.  The subclass identifier is two bytes after the
         * B-tree signature.
         */
        H5B2_subid_t subtype = (H5B2_subid_t)sig[H5B2_SIZEOF_MAGIC+1];

        switch(subtype) {
            case H5B2_TEST_ID:
                status = H5B2_hdr_debug(f, H5P_DATASET_XFER_DEFAULT, addr, stdout, 0, VCOL, H5B2_TEST);
                break;

            case H5B2_FHEAP_HUGE_INDIR_ID:
                status = H5B2_hdr_debug(f, H5P_DATASET_XFER_DEFAULT, addr, stdout, 0, VCOL, H5HF_BT2_INDIR);
                break;

            case H5B2_FHEAP_HUGE_FILT_INDIR_ID:
                status = H5B2_hdr_debug(f, H5P_DATASET_XFER_DEFAULT, addr, stdout, 0, VCOL, H5HF_BT2_FILT_INDIR);
                break;

            case H5B2_FHEAP_HUGE_DIR_ID:
                status = H5B2_hdr_debug(f, H5P_DATASET_XFER_DEFAULT, addr, stdout, 0, VCOL, H5HF_BT2_DIR);
                break;

            case H5B2_FHEAP_HUGE_FILT_DIR_ID:
                status = H5B2_hdr_debug(f, H5P_DATASET_XFER_DEFAULT, addr, stdout, 0, VCOL, H5HF_BT2_FILT_DIR);
                break;

            default:
                fprintf(stderr, "Unknown B-tree subtype %u\n", (unsigned)(subtype));
                HDexit(4);
        } /* end switch */

    } else if(!HDmemcmp(sig, H5B2_BRCH_MAGIC, H5B2_SIZEOF_MAGIC)) {
        /*
         * Debug a v2 B-tree.  B-trees are debugged through the B-tree
         * subclass.  The subclass identifier is two bytes after the
         * B-tree signature.
         */
        H5B2_subid_t subtype = (H5B2_subid_t)sig[H5B2_SIZEOF_MAGIC+1];

        /* Check for enough valid parameters */
        if(extra == 0 || extra2 == 0) {
            fprintf(stderr, "ERROR: Need v2 B-tree header address and number of records node in order to dump internal node\n");
            fprintf(stderr, "v2 B-tree internal node usage:\n");
            fprintf(stderr, "\th5debug <filename> <internal node address> <v2 B-tree header address> <number of records>\n");
            HDexit(4);
        } /* end if */

        /* Note: a depth of '2' is used here because the actual depth isn't
         *      important, as long as it's > 1, to indicate a 'branch' internal
         *      node.
         */
        switch(subtype) {
            case H5B2_TEST_ID:
                status = H5B2_int_debug(f, H5P_DATASET_XFER_DEFAULT, addr, stdout, 0, VCOL, H5B2_TEST, extra, (unsigned)extra2, (unsigned)2);
                break;

            case H5B2_FHEAP_HUGE_INDIR_ID:
                status = H5B2_int_debug(f, H5P_DATASET_XFER_DEFAULT, addr, stdout, 0, VCOL, H5HF_BT2_INDIR, extra, (unsigned)extra2, (unsigned)2);
                break;

            case H5B2_FHEAP_HUGE_FILT_INDIR_ID:
                status = H5B2_int_debug(f, H5P_DATASET_XFER_DEFAULT, addr, stdout, 0, VCOL, H5HF_BT2_FILT_INDIR, extra, (unsigned)extra2, (unsigned)2);
                break;

            case H5B2_FHEAP_HUGE_DIR_ID:
                status = H5B2_int_debug(f, H5P_DATASET_XFER_DEFAULT, addr, stdout, 0, VCOL, H5HF_BT2_DIR, extra, (unsigned)extra2, (unsigned)2);
                break;

            case H5B2_FHEAP_HUGE_FILT_DIR_ID:
                status = H5B2_int_debug(f, H5P_DATASET_XFER_DEFAULT, addr, stdout, 0, VCOL, H5HF_BT2_FILT_DIR, extra, (unsigned)extra2, (unsigned)2);
                break;

            default:
                fprintf(stderr, "Unknown B-tree subtype %u\n", (unsigned)(subtype));
                HDexit(4);
        } /* end switch */

    } else if(!HDmemcmp(sig, H5B2_TWIG_MAGIC, H5B2_SIZEOF_MAGIC)) {
        /*
         * Debug a v2 B-tree.  B-trees are debugged through the B-tree
         * subclass.  The subclass identifier is two bytes after the
         * B-tree signature.
         */
        H5B2_subid_t subtype = (H5B2_subid_t)sig[H5B2_SIZEOF_MAGIC+1];

        /* Check for enough valid parameters */
        if(extra == 0 || extra2 == 0) {
            fprintf(stderr, "ERROR: Need v2 B-tree header address and number of records node in order to dump internal node\n");
            fprintf(stderr, "v2 B-tree internal node usage:\n");
            fprintf(stderr, "\th5debug <filename> <internal node address> <v2 B-tree header address> <number of records>\n");
            HDexit(4);
        } /* end if */

        /* Note: a depth of '1' is used here to indicate a 'twig' internal node */
        switch(subtype) {
            case H5B2_TEST_ID:
                status = H5B2_int_debug(f, H5P_DATASET_XFER_DEFAULT, addr, stdout, 0, VCOL, H5B2_TEST, extra, (unsigned)extra2, (unsigned)1);
                break;

            case H5B2_FHEAP_HUGE_INDIR_ID:
                status = H5B2_int_debug(f, H5P_DATASET_XFER_DEFAULT, addr, stdout, 0, VCOL, H5HF_BT2_INDIR, extra, (unsigned)extra2, (unsigned)1);
                break;

            case H5B2_FHEAP_HUGE_FILT_INDIR_ID:
                status = H5B2_int_debug(f, H5P_DATASET_XFER_DEFAULT, addr, stdout, 0, VCOL, H5HF_BT2_FILT_INDIR, extra, (unsigned)extra2, (unsigned)1);
                break;

            case H5B2_FHEAP_HUGE_DIR_ID:
                status = H5B2_int_debug(f, H5P_DATASET_XFER_DEFAULT, addr, stdout, 0, VCOL, H5HF_BT2_DIR, extra, (unsigned)extra2, (unsigned)1);
                break;

            case H5B2_FHEAP_HUGE_FILT_DIR_ID:
                status = H5B2_int_debug(f, H5P_DATASET_XFER_DEFAULT, addr, stdout, 0, VCOL, H5HF_BT2_FILT_DIR, extra, (unsigned)extra2, (unsigned)1);
                break;

            default:
                fprintf(stderr, "Unknown B-tree subtype %u\n", (unsigned)(subtype));
                HDexit(4);
        } /* end switch */

    } else if(!HDmemcmp(sig, H5B2_LEAF_MAGIC, H5B2_SIZEOF_MAGIC)) {
        /*
         * Debug a v2 B-tree.  B-trees are debugged through the B-tree
         * subclass.  The subclass identifier is two bytes after the
         * B-tree signature.
         */
        H5B2_subid_t subtype = (H5B2_subid_t)sig[H5B2_SIZEOF_MAGIC+1];

        /* Check for enough valid parameters */
        if(extra == 0 || extra2 == 0) {
            fprintf(stderr, "ERROR: Need v2 B-tree header address and number of records in order to dump leaf node\n");
            fprintf(stderr, "v2 B-tree leaf node usage:\n");
            fprintf(stderr, "\th5debug <filename> <leaf node address> <v2 B-tree header address> <number of records>\n");
            HDexit(4);
        } /* end if */

        switch(subtype) {
            case H5B2_TEST_ID:
                status = H5B2_leaf_debug(f, H5P_DATASET_XFER_DEFAULT, addr, stdout, 0, VCOL, H5B2_TEST, extra, (unsigned)extra2);
                break;

            case H5B2_FHEAP_HUGE_INDIR_ID:
                status = H5B2_leaf_debug(f, H5P_DATASET_XFER_DEFAULT, addr, stdout, 0, VCOL, H5HF_BT2_INDIR, extra, (unsigned)extra2);
                break;

            case H5B2_FHEAP_HUGE_FILT_INDIR_ID:
                status = H5B2_leaf_debug(f, H5P_DATASET_XFER_DEFAULT, addr, stdout, 0, VCOL, H5HF_BT2_FILT_INDIR, extra, (unsigned)extra2);
                break;

            case H5B2_FHEAP_HUGE_DIR_ID:
                status = H5B2_leaf_debug(f, H5P_DATASET_XFER_DEFAULT, addr, stdout, 0, VCOL, H5HF_BT2_DIR, extra, (unsigned)extra2);
                break;

            case H5B2_FHEAP_HUGE_FILT_DIR_ID:
                status = H5B2_leaf_debug(f, H5P_DATASET_XFER_DEFAULT, addr, stdout, 0, VCOL, H5HF_BT2_FILT_DIR, extra, (unsigned)extra2);
                break;

            default:
                fprintf(stderr, "Unknown B-tree subtype %u\n", (unsigned)(subtype));
                HDexit(4);
        } /* end switch */

    } else if(!HDmemcmp(sig, H5HF_HDR_MAGIC, H5HF_SIZEOF_MAGIC)) {
        /*
         * Debug a fractal heap header.
         */
        status = H5HF_hdr_debug(f, H5P_DATASET_XFER_DEFAULT, addr, stdout, 0, VCOL);

    } else if(!HDmemcmp(sig, H5HF_DBLOCK_MAGIC, H5HF_SIZEOF_MAGIC)) {
        /*
         * Debug a fractal heap direct block.
         */

        /* Check for enough valid parameters */
        if(extra == 0 || extra2 == 0) {
            fprintf(stderr, "ERROR: Need fractal heap header address and size of direct block in order to dump direct block\n");
            fprintf(stderr, "Fractal heap direct block usage:\n");
            fprintf(stderr, "\th5debug <filename> <direct block address> <heap header address> <size of direct block>\n");
            HDexit(4);
        } /* end if */

        status = H5HF_dblock_debug(f, H5P_DATASET_XFER_DEFAULT, addr, stdout, 0, VCOL, extra, (size_t)extra2);

    } else if(!HDmemcmp(sig, H5HF_IBLOCK_MAGIC, H5HF_SIZEOF_MAGIC)) {
        /*
         * Debug a fractal heap indirect block.
         */

        /* Check for enough valid parameters */
        if(extra == 0 || extra2 == 0) {
            fprintf(stderr, "ERROR: Need fractal heap header address and number of rows in order to dump indirect block\n");
            fprintf(stderr, "Fractal heap indirect block usage:\n");
            fprintf(stderr, "\th5debug <filename> <indirect block address> <heap header address> <number of rows>\n");
            HDexit(4);
        } /* end if */

        status = H5HF_iblock_debug(f, H5P_DATASET_XFER_DEFAULT, addr, stdout, 0, VCOL, extra, (unsigned)extra2);

    } else if(!HDmemcmp(sig, H5FS_HDR_MAGIC, H5FS_SIZEOF_MAGIC)) {
        /*
         * Debug a free space header.
         */

        status = H5FS_debug(f, H5P_DATASET_XFER_DEFAULT, addr, stdout, 0, VCOL);

    } else if(!HDmemcmp(sig, H5FS_SINFO_MAGIC, H5FS_SIZEOF_MAGIC)) {
        /*
         * Debug free space serialized sections.
         */

        /* Check for enough valid parameters */
        if(extra == 0 || extra2 == 0) {
            fprintf(stderr, "ERROR: Need free space header address and client address in order to dump serialized sections\n");
            fprintf(stderr, "Free space serialized sections usage:\n");
            fprintf(stderr, "\th5debug <filename> <serialized sections address> <free space header address> <client address>\n");
            HDexit(4);
        } /* end if */

        status = H5FS_sects_debug(f, H5P_DATASET_XFER_DEFAULT, addr, stdout, 0, VCOL, extra, extra2);

    } else if(sig[0] == H5O_VERSION) {
        /*
         * This could be an object header.  Since they don't have a signature
         * it's a somewhat "ify" detection.
         */
        status = H5O_debug(f, H5P_DATASET_XFER_DEFAULT, addr, stdout, 0, VCOL);

    } else {
        /*
         * Got some other unrecognized signature.
         */
        printf("%-*s ", VCOL, "Signature:");
        for (u = 0; u < sizeof(sig); u++) {
            if (sig[u] > ' ' && sig[u] <= '~' && '\\' != sig[u])
                HDputchar(sig[u]);
            else if ('\\' == sig[u]) {
                HDputchar('\\');
                HDputchar('\\');
            } else
                printf("\\%03o", sig[u]);
        }
        HDputchar('\n');

        fprintf(stderr, "unknown signature\n");
        HDexit(4);
    } /* end else */

    /* Check for an error when dumping information */
    if(status < 0) {
        fprintf(stderr, "An error occurred!\n");
        H5Eprint(stderr);
        HDexit(5);
    } /* end if */

    H5Pclose(dxpl);
    H5Pclose(fapl);
    H5Fclose(fid);

    return 0;
} /* main() */

