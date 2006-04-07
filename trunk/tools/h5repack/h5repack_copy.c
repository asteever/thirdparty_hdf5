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


#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "H5private.h"
#include "h5repack.h"

#define PER(A,B) { per = 0;                                      \
                   if (A!=0)                                      \
                    per = (float)fabs(1-( (float)B / (float)A )); \
                 }

#define FORMAT_OBJ      " %-21s %s\n"  /* obj type, name */
#define FORMAT_OBJ_ATTR "  %-21s %s\n"  /* obj type, name */



/*-------------------------------------------------------------------------
 * Function: print_dataset_info
 *
 * Purpose: print name, filters, percentage compression of a dataset
 *
 *-------------------------------------------------------------------------
 */
static void print_dataset_info(hid_t dcpl_id, 
                               char *objname,
                               float per)
{
 char         strfilter[255];
#if defined (PRINT_DEBUG )
 char         temp[255];
#endif
 int          nfilters;       /* number of filters */
 unsigned     filt_flags;     /* filter flags */
 H5Z_filter_t filtn;          /* filter identification number */
 unsigned     cd_values[20];  /* filter client data values */
 size_t       cd_nelmts;      /* filter client number of values */
 char         f_objname[256];    /* filter objname */
 int          i;

 strcpy(strfilter,"\0");

 /* get information about input filters */
 if ((nfilters = H5Pget_nfilters(dcpl_id))<0)
  return;

 for ( i=0; i<nfilters; i++)
 {
  cd_nelmts = NELMTS(cd_values);

#ifdef H5_WANT_H5_V1_6_COMPAT
  filtn = H5Pget_filter(dcpl_id,
   (unsigned)i,
   &filt_flags,
   &cd_nelmts,
   cd_values,
   sizeof(f_objname),
   f_objname);
#else
  filtn = H5Pget_filter(dcpl_id,
   (unsigned)i,
   &filt_flags,
   &cd_nelmts,
   cd_values,
   sizeof(f_objname),
   f_objname,
   NULL);
#endif /* H5_WANT_H5_V1_6_COMPAT */


  switch (filtn)
  {
  default:
   break;
  case H5Z_FILTER_DEFLATE:
   strcat(strfilter,"GZIP ");

#if defined (PRINT_DEBUG)
   {
    unsigned level=cd_values[0];
    sprintf(temp,"(%d)",level);
    strcat(strfilter,temp);
   }
#endif

   break;
  case H5Z_FILTER_SZIP:
   strcat(strfilter,"SZIP ");

#if defined (PRINT_DEBUG)
   {
    unsigned options_mask=cd_values[0]; /* from dcpl, not filt*/
    unsigned ppb=cd_values[1];
    sprintf(temp,"(%d,",ppb);
    strcat(strfilter,temp);
    if (options_mask & H5_SZIP_EC_OPTION_MASK)
     strcpy(temp,"EC) ");
    else if (options_mask & H5_SZIP_NN_OPTION_MASK)
     strcpy(temp,"NN) ");
   }
   strcat(strfilter,temp);

#endif

   break;
  case H5Z_FILTER_SHUFFLE:
   strcat(strfilter,"SHUF ");
   break;
  case H5Z_FILTER_FLETCHER32:
   strcat(strfilter,"FLET ");
   break;
  case H5Z_FILTER_NBIT:
   strcat(strfilter,"NBIT ");
   break;
  case H5Z_FILTER_SCALEOFFSET:
   strcat(strfilter,"SCALEOFFSET ");
   break;
  } /* switch */
 }/*i*/

 if (strcmp(strfilter,"\0")==0)
  printf(FORMAT_OBJ,"dset",objname );
 else
 {
  char str[255], temp[20];
  strcpy(str,"dset  ");
  strcat(str,strfilter);
  sprintf(temp,"(%.2f%%)",per);
  strcat(str,temp);
  printf(FORMAT_OBJ,str,objname);
 }
  


}


/*-------------------------------------------------------------------------
 * Function: copy_objects
 *
 * Purpose: duplicate all HDF5 objects in the file
 *
 * Return: 0, ok, -1 no
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: October, 23, 2003
 *
 *-------------------------------------------------------------------------
 */

int copy_objects(const char* fnamein,
                 const char* fnameout,
                 pack_opt_t *options,
                 int argc, 
                 const char *argv[])
{
 hid_t         fidin;
 hid_t         fidout;
 trav_table_t  *travt=NULL;

/*-------------------------------------------------------------------------
 * open the files
 *-------------------------------------------------------------------------
 */
 if ((fidin=h5tools_fopen(fnamein, NULL, NULL, 0, argc, argv))<0 ){
  printf("<%s>: %s\n", fnamein, H5FOPENERROR );
  goto out;
 }
 if ((fidout=H5Fcreate(fnameout,H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT))<0 ){
  printf("<%s>: Could not create file\n", fnameout );
  goto out;
 }

 if (options->verbose)
  printf("Making file <%s>...\n",fnameout);

 /* init table */
 trav_table_init(&travt);

 /* get the list of objects in the file */
 if (h5trav_gettable(fidin,travt)<0)
  goto out;

/*-------------------------------------------------------------------------
 * do the copy
 *-------------------------------------------------------------------------
 */
 if(do_copy_objects(fidin,fidout,travt,options)<0) {
  printf("<%s>: Could not copy data to: %s\n", fnamein, fnameout);
  goto out;
 }

/*-------------------------------------------------------------------------
 * do the copy of referenced objects
 * and create hard links
 *-------------------------------------------------------------------------
 */
 if(do_copy_refobjs(fidin,fidout,travt,options)<0) {
  printf("<%s>: Could not copy data to: %s\n", fnamein, fnameout);
  goto out;
 }

 /* free table */
 trav_table_free(travt);

/*-------------------------------------------------------------------------
 * close
 *-------------------------------------------------------------------------
 */

 H5Fclose(fidin);
 H5Fclose(fidout);
 return 0;

/*-------------------------------------------------------------------------
 * out
 *-------------------------------------------------------------------------
 */

out:
 H5E_BEGIN_TRY {
  H5Fclose(fidin);
  H5Fclose(fidout);
 } H5E_END_TRY;
 if (travt)
  trav_table_free(travt);

 return -1;
}

/*-------------------------------------------------------------------------
 * Function: do_copy_objects
 *
 * Purpose: duplicate all HDF5 objects in the file
 *
 * Return: 0, ok, -1 no
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: October, 23, 2003
	*   Modified: December, 03, 2004 - added a check for H5Dcreate; if the dataset
	*    cannot be created with the requested filter, use the input one
 *
 *-------------------------------------------------------------------------
 */

int do_copy_objects(hid_t fidin,
                    hid_t fidout,
                    trav_table_t *travt,
                    pack_opt_t *options) /* repack options */
{
 hid_t     grp_in;       /* group ID */
 hid_t     grp_out;      /* group ID */
 hid_t     dset_in;      /* read dataset ID */
 hid_t     dset_out;     /* write dataset ID */
 hid_t     type_in;      /* named type ID */
 hid_t     type_out;     /* named type ID */
 hid_t     dcpl_id;      /* dataset creation property list ID */
 hid_t     dcpl_out;     /* dataset creation property list ID */
 hid_t     space_id;     /* space ID */
 hid_t     ftype_id;     /* file data type ID */
 hid_t     mtype_id;     /* memory data type ID */
 size_t    msize;        /* memory size of memory type */
 void      *buf=NULL;    /* data buffer */
 hsize_t   nelmts;       /* number of elements in dataset */
 int       rank;         /* rank of dataset */
 hsize_t   dims[H5S_MAX_RANK];/* dimensions of dataset */
 hsize_t   dsize_in;     /* input dataset size before filter */
 hsize_t   dsize_out;    /* output dataset size after filter */
 int       next;         /* external files */
 int       apply_s;      /* flag for apply filter to small dataset sizes */
 int       apply_f;      /* flag for apply filter to return error on H5Dcreate with filter */
 float     per;          /* percent utilization of storage */
 int       i, j;



/*-------------------------------------------------------------------------
 * copy the suppplied object list
 *-------------------------------------------------------------------------
 */

 if (options->verbose) {
  printf("-----------------------------------------\n");
  printf(" Type  Filter (Ratio)  Name\n");
  printf("-----------------------------------------\n");
 }

 for ( i = 0; i < travt->nobjs; i++)
 {

  buf=NULL;
  switch ( travt->objs[i].type )
  {
/*-------------------------------------------------------------------------
 * H5G_GROUP
 *-------------------------------------------------------------------------
 */
  case H5G_GROUP:
   if (options->verbose)
    printf(FORMAT_OBJ,"group",travt->objs[i].name );

   if ((grp_out=H5Gcreate(fidout,travt->objs[i].name, 0))<0)
    goto error;

   if((grp_in = H5Gopen (fidin,travt->objs[i].name))<0)
    goto error;

   /*-------------------------------------------------------------------------
    * copy attrs
    *-------------------------------------------------------------------------
    */
   if (copy_attr(grp_in,grp_out,options)<0)
    goto error;

   if (H5Gclose(grp_out)<0)
    goto error;
   if (H5Gclose(grp_in)<0)
    goto error;


   break;

/*-------------------------------------------------------------------------
 * H5G_DATASET
 *-------------------------------------------------------------------------
 */
  case H5G_DATASET:

   if ((dset_in=H5Dopen(fidin,travt->objs[i].name))<0)
    goto error;
   if ((space_id=H5Dget_space(dset_in))<0)
    goto error;
   if ((ftype_id=H5Dget_type (dset_in))<0)
    goto error;
   if ((dcpl_id=H5Dget_create_plist(dset_in))<0)
    goto error;
			if ((dcpl_out = H5Pcopy (dcpl_id))<0)
				goto error;
   if ( (rank=H5Sget_simple_extent_ndims(space_id))<0)
    goto error;
   HDmemset(dims, 0, sizeof dims);
   if ( H5Sget_simple_extent_dims(space_id,dims,NULL)<0)
    goto error;
   nelmts=1;
   for (j=0; j<rank; j++)
    nelmts*=dims[j];

		 if ((mtype_id=h5tools_get_native_type(ftype_id))<0)
    goto error;

   if ((msize=H5Tget_size(mtype_id))==0)
    goto error;

/*-------------------------------------------------------------------------
 * check for external files
 *-------------------------------------------------------------------------
 */
   if ((next=H5Pget_external_count (dcpl_id))<0)
    goto error;

   if (next) 
    fprintf(stderr," <warning: %s has external files, ignoring read...>\n",
    travt->objs[i].name );

/*-------------------------------------------------------------------------
 * check if the dataset creation property list has filters that
 * are not registered in the current configuration
 * 1) the external filters GZIP and SZIP might not be available
 * 2) the internal filters might be turned off
 *-------------------------------------------------------------------------
 */
   if (next==0 && h5tools_canreadf((travt->objs[i].name),dcpl_id)==1)
   {
    apply_s=1;
    apply_f=1;

/*-------------------------------------------------------------------------
 * references are a special case
 * we cannot just copy the buffers, but instead we recreate the reference
 * in a second traversal of the output file
 *-------------------------------------------------------------------------
 */
   if ( (H5T_REFERENCE!=H5Tget_class(mtype_id)))
   {
    /* get the storage size of the input dataset */
    dsize_in=H5Dget_storage_size(dset_in);

  /*-------------------------------------------------------------------------
   * read 
   *-------------------------------------------------------------------------
   */
    if (nelmts)
    {
     buf=(void *) HDmalloc((unsigned)(nelmts*msize));
     if ( buf==NULL){
      printf( "cannot read into memory\n" );
      goto error;
     }
     if (H5Dread(dset_in,mtype_id,H5S_ALL,H5S_ALL,H5P_DEFAULT,buf)<0)
      goto error;

     /*-------------------------------------------------------------------------
      * check for datasets too small 
      *-------------------------------------------------------------------------
      */
     
     if (nelmts*msize < options->threshold )
      apply_s=0;
     
    /*-------------------------------------------------------------------------
     * apply the filter
     *-------------------------------------------------------------------------
     */
     if (apply_s){
      if (apply_filters(travt->objs[i].name,rank,dims,dcpl_out,mtype_id,options)<0)
       goto error;
     }

    }/*nelmts*/

    /*-------------------------------------------------------------------------
     * create;
					* disable error checking in case the dataset cannot be created with the
					* modified dcpl; in that case use the original instead
     *-------------------------------------------------------------------------
     */

				H5E_BEGIN_TRY {
					 dset_out=H5Dcreate(fidout,travt->objs[i].name,mtype_id,space_id,dcpl_out);
				} H5E_END_TRY;

    
				if (dset_out==FAIL)
				{
     if ((dset_out=H5Dcreate(fidout,travt->objs[i].name,mtype_id,space_id,dcpl_id))<0)
						goto error;

     apply_f=0;
				}

				/*-------------------------------------------------------------------------
     * write dataset
     *-------------------------------------------------------------------------
     */

    if (dsize_in && nelmts) {
     if (H5Dwrite(dset_out,mtype_id,H5S_ALL,H5S_ALL,H5P_DEFAULT,buf)<0)
      goto error;
    }

   /*-------------------------------------------------------------------------
    * amount of compression used
    *-------------------------------------------------------------------------
    */
    if (options->verbose) {
     if (apply_s && apply_f)
     {
      /* get the storage size of the input dataset */
      dsize_out=H5Dget_storage_size(dset_out);
      PER((hssize_t)dsize_in,(hssize_t)dsize_out);
      print_dataset_info(dcpl_out,travt->objs[i].name,per*(float)100.0);
     }
     else
      print_dataset_info(dcpl_id,travt->objs[i].name,0);
    }
   
    if (apply_s==0 && options->verbose)
     printf(" <warning: filter not applied to %s. dataset smaller than %d bytes>\n",
       travt->objs[i].name,
       (int)options->threshold);

    if (apply_f==0 && options->verbose)
						printf(" <warning: could not apply the filter to %s>\n", 
      travt->objs[i].name);

    /*-------------------------------------------------------------------------
     * copy attrs
     *-------------------------------------------------------------------------
     */
    if (copy_attr(dset_in,dset_out,options)<0)
     goto error;

    /*close */
    if (H5Dclose(dset_out)<0)
     goto error;

    if (buf)
     free(buf);

   }/*H5T_STD_REF_OBJ*/
   }/*can_read*/


/*-------------------------------------------------------------------------
 * close
 *-------------------------------------------------------------------------
 */
   if (H5Tclose(ftype_id)<0)
    goto error;
   if (H5Tclose(mtype_id)<0)
    goto error;
   if (H5Pclose(dcpl_id)<0)
    goto error;
			if (H5Pclose(dcpl_out)<0)
    goto error;
   if (H5Sclose(space_id)<0)
    goto error;
   if (H5Dclose(dset_in)<0)
    goto error;

   break;

/*-------------------------------------------------------------------------
 * H5G_TYPE
 *-------------------------------------------------------------------------
 */
  case H5G_TYPE:

   if ((type_in = H5Topen (fidin,travt->objs[i].name))<0)
    goto error;

   if ((type_out = H5Tcopy(type_in))<0)
    goto error;

   if ((H5Tcommit(fidout,travt->objs[i].name,type_out))<0)
    goto error;

/*-------------------------------------------------------------------------
 * copy attrs
 *-------------------------------------------------------------------------
 */
   if (copy_attr(type_in,type_out,options)<0)
    goto error;

   if (H5Tclose(type_in)<0)
    goto error;
   if (H5Tclose(type_out)<0)
    goto error;

   if (options->verbose)
    printf(FORMAT_OBJ,"type",travt->objs[i].name );

   break;


/*-------------------------------------------------------------------------
 * H5G_LINK
 *-------------------------------------------------------------------------
 */

  case H5G_LINK:
   {
    H5G_stat_t  statbuf;
    char        *targbuf=NULL;

    if (H5Gget_objinfo(fidin,travt->objs[i].name,FALSE,&statbuf)<0)
     goto error;

    targbuf = malloc(statbuf.u.slink.linklen);

    if (H5Gget_linkval(fidin,travt->objs[i].name,statbuf.u.slink.linklen,targbuf)<0)
     goto error;

    if (H5Glink(fidout,
     H5G_LINK_SOFT,
     targbuf,        /* current name of object */
     travt->objs[i].name   /* new name of object */
     )<0)
     goto error;

    free(targbuf);

    if (options->verbose)
     printf(FORMAT_OBJ,"link",travt->objs[i].name );

   }
   break;

  default:
   goto error;
  }
 }

/*-------------------------------------------------------------------------
 * the root is a special case, we get an ID for the root group
 * and copy its attributes using that ID
 * it must be done last, because the attributes might contain references to
 * objects in the object list
 *-------------------------------------------------------------------------
 */

 if ((grp_out = H5Gopen(fidout,"/"))<0)
  goto error;

 if ((grp_in  = H5Gopen(fidin,"/"))<0)
  goto error;

 if (copy_attr(grp_in,grp_out,options)<0)
  goto error;

 if (H5Gclose(grp_out)<0)
  goto error;
 if (H5Gclose(grp_in)<0)
  goto error;

 return 0;

error:
 H5E_BEGIN_TRY {
  H5Gclose(grp_in);
  H5Gclose(grp_out);
  H5Pclose(dcpl_id);
  H5Sclose(space_id);
  H5Dclose(dset_in);
  H5Dclose(dset_out);
  H5Tclose(ftype_id);
  H5Tclose(mtype_id);
  H5Tclose(type_in);
  H5Tclose(type_out);
  if (buf)
   free(buf);
 } H5E_END_TRY;
 return -1;

}


/*-------------------------------------------------------------------------
 * Function: copy_attr
 *
 * Purpose: copy attributes located in LOC_IN, which is obtained either from
 * loc_id = H5Gopen( fid, name);
 * loc_id = H5Dopen( fid, name);
 * loc_id = H5Topen( fid, name);
 *
 * Return: 0, ok, -1 no
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: October, 28, 2003
 *
 *-------------------------------------------------------------------------
 */

int copy_attr(hid_t loc_in,
              hid_t loc_out,
              pack_opt_t *options
              )
{
 hid_t      attr_id;      /* attr ID */
 hid_t      attr_out;     /* attr ID */
 hid_t      space_id;     /* space ID */
 hid_t      ftype_id;     /* file data type ID */
 hid_t      mtype_id;     /* memory data type ID */
 size_t     msize;        /* memory size of type */
 void       *buf=NULL;    /* data buffer */
 hsize_t    nelmts;       /* number of elements in dataset */
 int        rank;         /* rank of dataset */
 hsize_t    dims[H5S_MAX_RANK];/* dimensions of dataset */
 char       name[255];
 int        n, j;
 unsigned   u;

 if ((n = H5Aget_num_attrs(loc_in))<0)
  goto error;

 for ( u = 0; u < (unsigned)n; u++)
 {

  /* set data buffer to NULL each iteration
     we might not use it in the case of references
   */
   buf=NULL;
/*-------------------------------------------------------------------------
 * open
 *-------------------------------------------------------------------------
 */
  /* open attribute */
  if ((attr_id = H5Aopen_idx(loc_in, u))<0)
   goto error;

  /* get name */
  if (H5Aget_name( attr_id, 255, name )<0)
   goto error;

  /* get the file datatype  */
  if ((ftype_id = H5Aget_type( attr_id )) < 0 )
   goto error;

  /* get the dataspace handle  */
  if ((space_id = H5Aget_space( attr_id )) < 0 )
   goto error;

  /* get dimensions  */
  if ( (rank = H5Sget_simple_extent_dims(space_id, dims, NULL)) < 0 )
   goto error;

  nelmts=1;
  for (j=0; j<rank; j++)
   nelmts*=dims[j];

  if ((mtype_id=h5tools_get_native_type(ftype_id))<0)
   goto error;

  if ((msize=H5Tget_size(mtype_id))==0)
   goto error;

/*-------------------------------------------------------------------------
 * object references are a special case
 * we cannot just copy the buffers, but instead we recreate the reference
 * this is done on a second sweep of the file that just copies
 * the referenced objects
 *-------------------------------------------------------------------------
 */
  if ( ! H5Tequal(mtype_id, H5T_STD_REF_OBJ))
  {


 /*-------------------------------------------------------------------------
  * read to memory
  *-------------------------------------------------------------------------
  */

   buf=(void *) HDmalloc((unsigned)(nelmts*msize));
   if ( buf==NULL){
    printf( "cannot read into memory\n" );
    goto error;
   }
   if (H5Aread(attr_id,mtype_id,buf)<0)
    goto error;

   /*-------------------------------------------------------------------------
    * copy
    *-------------------------------------------------------------------------
    */

   if ((attr_out=H5Acreate(loc_out,name,ftype_id,space_id,H5P_DEFAULT))<0)
    goto error;
   if(H5Awrite(attr_out,mtype_id,buf)<0)
    goto error;

   /*close*/
   if (H5Aclose(attr_out)<0)
    goto error;


   if (buf)
    free(buf);


  } /*H5T_STD_REF_OBJ*/


  if (options->verbose)
   printf(FORMAT_OBJ_ATTR, "attr", name);

/*-------------------------------------------------------------------------
 * close
 *-------------------------------------------------------------------------
 */

  if (H5Tclose(ftype_id)<0) goto error;
  if (H5Tclose(mtype_id)<0) goto error;
  if (H5Sclose(space_id)<0) goto error;
  if (H5Aclose(attr_id)<0) goto error;

 } /* u */

  return 0;

error:
 H5E_BEGIN_TRY {
  H5Tclose(ftype_id);
  H5Tclose(mtype_id);
  H5Sclose(space_id);
  H5Aclose(attr_id);
  H5Aclose(attr_out);
  if (buf)
    free(buf);
 } H5E_END_TRY;
 return -1;
}




