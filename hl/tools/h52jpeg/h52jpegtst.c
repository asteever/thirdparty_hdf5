/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
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

#include "hdf5.h"
#include "hdf5_hl.h"
#include "H5private.h"
#include "h52jpegtst.h"
#include <stdlib.h>
#include <string.h>

#define IM_1PAL "im8_1pal"
#define IM_2PAL "im8_2pal"
#define PAL1 "pal1"
#define PAL2 "pal2"

static int make_images( hid_t fid );
static int read_data(const char* fname, hsize_t *width, hsize_t *height );
unsigned char *gbuf = NULL;  /* global buffer for image data */


/*-------------------------------------------------------------------------
 * Function: main
 *
 * Purpose: h52jpegtst main program. Generate images to be used
 *  by h52jpeg tests
 *
 * Programmer: Pedro Vicente, pvn@hdfgroup.org
 *
 * Date: May 30, 2008
 *
 *-------------------------------------------------------------------------
 */
int main( void )
{
    hid_t fid; /* HDF5 file identifier */
    
    /* create a new HDF5 file using default properties. */
    if (( fid = H5Fcreate( "h52jpegtst.h5", H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT )) < 0 ) 
        return 1;
    
    /* make images */
    if ( make_images( fid ) < 0 )
        goto out;
      
    /* close the file. */
    H5Fclose( fid );
    
    return 0;
    
out:
    printf("Error on return function...Exiting\n");
    H5Fclose( fid );
    return 1;
}

/*-------------------------------------------------------------------------
 * Function: make_images
 *
 * Purpose: generate images 
 *
 *-------------------------------------------------------------------------
 */

static int make_images( hid_t fid )
{
    hid_t          gid;
    hsize_t        width;                         /* width of image */
    hsize_t        height;                        /* height of image */
    hsize_t        pal_dims[2] = {PAL_ENTRIES,3}; /* palette dimensions */
    unsigned char  pal[ PAL_ENTRIES * 3 ];        /* palette array */
    int            i, n;
    
    /*-------------------------------------------------------------------------
    * indexed image with 1 palette 
    *-------------------------------------------------------------------------
    */
    
    /* read first data file */
    if ( read_data( "image8.txt", &width, &height ) < 0 )
        goto out;
    
    /* make the image */
    if ( H5IMmake_image_8bit( fid, IM_1PAL, width, height, gbuf ) < 0 )
        goto out;
    
    /* make a palette */
    if ( H5IMmake_palette( fid, PAL1, pal_dims, pal_rgb ) < 0 )
        goto out;
    
    /* attach the 1st palette to the image */
    if ( H5IMlink_palette( fid, IM_1PAL, PAL1 ) < 0 )
    {
        goto out;
    }

     /*-------------------------------------------------------------------------
    * indexed image with 2 palettes
    *-------------------------------------------------------------------------
    */

    /* make the image */
    if ( H5IMmake_image_8bit( fid, IM_2PAL, width, height, gbuf ) < 0 )
        goto out;

    /* attach the 1st palette to the image */
    if ( H5IMlink_palette( fid, IM_2PAL, PAL1 ) < 0 )
    {
        goto out;
    }
    
    /*-------------------------------------------------------------------------
    * define another palette, green tones
    *-------------------------------------------------------------------------
    */
    for ( i = 0, n = 0; i < PAL_ENTRIES*3; i+=3, n++)
    {
        pal[i]  =0;      /* red */
        pal[i+1]=n;      /* green */
        pal[i+2]=0;      /* blue */
    }

    /* save the palette */
    if ( H5IMmake_palette( fid, PAL2, pal_dims, pal ) < 0 )
        goto out;
    
    /* attach the palette to the image */
    if ( H5IMlink_palette( fid, IM_2PAL, PAL2 ) < 0 )
    {
        goto out;
    }  

    /*-------------------------------------------------------------------------
    * make another image, in a group and no palette
    *-------------------------------------------------------------------------
    */

    if (( gid = H5Gcreate2(fid, "g1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0 )
        goto out;

    /* make the image with a name with "%", "@", "$", "/", ":", "&", and "*" */
    if ( H5IMmake_image_8bit( gid, "1%2@3$45:6&7*", width, height, gbuf ) < 0 )
        goto out;

    H5Gclose(gid);

    
    /*-------------------------------------------------------------------------
    * true color image with pixel interlace in RGB type
    *-------------------------------------------------------------------------
    */
    
    /* read second data file */
    if ( read_data( "image24pixel.txt", &width, &height ) < 0 )
        goto out;
    
    /* make dataset */
    if ( H5IMmake_image_24bit( fid, "img24", width, height, "INTERLACE_PIXEL", gbuf ) < 0 )
        goto out;
    
    return 0;
    
out:    
    printf("Error on return function...Exiting\n");
    return -1;
}





/*-------------------------------------------------------------------------
 * read_data
 * utility function to read ASCII image data
 * the files have a header of the type
 *
 *   components
 *   n
 *   height
 *   n
 *   width
 *   n
 *
 * followed by the image data
 *
 *-------------------------------------------------------------------------
 */

static int read_data( const char* fname, /*IN*/
                      hsize_t *width, /*OUT*/
                      hsize_t *height /*OUT*/ )
{
    int    i, n;
    int    color_planes;
    char   str[20];
    FILE   *f;
    int    w, h;
    char   *srcdir = getenv("srcdir"); /* the source directory */
    char   data_file[512]="";          /* buffer to hold name of existing data file */
    
    /*-------------------------------------------------------------------------
    * compose the name of the file to open, using "srcdir", if appropriate
    *-------------------------------------------------------------------------
    */
    strcpy(data_file, "");
    if (srcdir)
    {
        strcpy(data_file, srcdir);
        strcat(data_file, "/");
    }
    strcat(data_file,fname);
    
    /*-------------------------------------------------------------------------
    * read
    *-------------------------------------------------------------------------
    */
    
    f = fopen(data_file, "r");
    if ( f == NULL )
    {
        printf( "Could not open file %s. Try set $srcdir \n", data_file );
        return -1;
    }
    
    fscanf( f, "%s", str );
    fscanf( f, "%d", &color_planes );
    fscanf( f, "%s", str );
    fscanf( f, "%d", &h);
    fscanf( f, "%s", str );
    fscanf( f, "%d", &w);
    
    *width = (hsize_t)w;
    *height = (hsize_t)h;
    
    if ( gbuf )
    {
        free( gbuf );
        gbuf=NULL;
    }
    
    gbuf = (unsigned char*) malloc (w * h * color_planes * sizeof( unsigned char ));
    
    for (i = 0; i < h * w * color_planes ; i++)
    {
        fscanf( f, "%d",&n );
        gbuf[i] = (unsigned char)n;
    }
    fclose(f);
    
    return 1;
    
}





