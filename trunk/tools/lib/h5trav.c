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


#include "h5trav.h"
#include "H5private.h"

/* functions for traversal */
static int traverse( hid_t loc_id,
              const char *group_name,
              trav_table_t *table,
              trav_info_t *info,
              int *idx,
              int print);

static hssize_t get_nnames( hid_t loc_id,
                   const char *group_name );

herr_t get_name_type( hid_t loc_id,
                      const char *group_name,
                      int idx,
                      char **name,
                      H5G_obj_t *type );

/*-------------------------------------------------------------------------
 * Function: h5trav_getinfo
 *
 * Purpose: get an array of "trav_info_t" , containing the name and type of
 *  objects in the file
 *
 * Return: number of object names in file
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: November 6, 2002
 *
 *-------------------------------------------------------------------------
 */

int h5trav_getinfo(hid_t file_id,
                   trav_info_t *info,
                   int print )
{

 trav_table_t  *table=NULL;
 int           nnames=0;

 /* init table */
 trav_table_init( &table );

 /* iterate starting on the root group */
 if (( nnames = traverse( file_id, "/", table, info, &nnames, print )) < 0 )
  return -1;

 /* free table */
 trav_table_free( table );

 return nnames;

}


/*-------------------------------------------------------------------------
 * Function: h5trav_gettable
 *
 * Purpose: get the trav_table_t struct
 *
 * Return: 0, -1 on error
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: December 17, 2003
 *
 *-------------------------------------------------------------------------
 */

int h5trav_gettable(hid_t fid, trav_table_t *travt)
{
 int nnames=0;

 /* iterate starting on the root group */
 if (( nnames = traverse(fid,"/",travt,NULL,&nnames,0))<0)
  return -1;

 return 0;

}


/*-------------------------------------------------------------------------
 * Function: h5trav_getindex
 *
 * Purpose: get index of OBJ in list
 *
 * Return: index, -1 if not found
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: May 9, 2003
 *
 *-------------------------------------------------------------------------
 */

int h5trav_getindex( const char *obj, int nobjs, trav_info_t *info )
{
 char *pdest;
 int  result;
 int  i;

 for ( i = 0; i < nobjs; i++)
 {
  if ( strcmp(obj,info[i].name)==0 )
   return i;

  pdest  = strstr( info[i].name, obj );
  result = (int)(pdest - info[i].name);

  /* found at position 1, meaning without '/' */
  if( pdest != NULL && result==1 )
   return i;
 }
 return -1;
}



/*-------------------------------------------------------------------------
 * Function: h5trav_freeinfo
 *
 * Purpose: free info memory
 *
 *-------------------------------------------------------------------------
 */

void h5trav_freeinfo( trav_info_t *info, int nobjs )
{
 int i;
	if ( info )
	{
		for ( i = 0; i < nobjs; i++)
		{
			if (info[i].name)
		 	HDfree( info[i].name );
		}
		HDfree(info);
	}
}


/*-------------------------------------------------------------------------
 * Function: get_nnames
 *
 * Purpose:  Counts the number of names in the group GROUP_NAME
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: October 10, 2002
 *
 * Return:
 *     Success: The return value of the first operator that
 *              returns non-zero, or zero if all members were
 *              processed with no operator returning non-zero.
 *
 *     Failure: Negative if something goes wrong within the
 *              library, or the negative value returned by one
 *              of the operators.
 *
 *-------------------------------------------------------------------------
 */

static hssize_t get_nnames( hid_t loc_id, const char *group_name )
{
    hid_t gid;
    hsize_t nobjs = 0;

    /* Open the group */
    if((gid = H5Gopen(loc_id, group_name)) < 0)
        return(-1);

    /* Retrieve the number of objects in it */
    if(H5Gget_num_objs(gid, &nobjs) < 0)
        return(-1);

    /* Close the group */
    if(H5Gclose(gid) < 0)
        return(-1);

    return((hssize_t)nobjs);
}


/*-------------------------------------------------------------------------
 * Function: opget_info
 *
 * Purpose: operator function
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: October 10, 2002
 *
 * Comments:
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static herr_t opget_info( hid_t loc_id, const char *name, void *op_data)
{

 H5G_stat_t statbuf;

 if (H5Gget_objinfo( loc_id, name, FALSE, &statbuf) < 0 )
  return -1;

 ((trav_info_t *)op_data)->type = statbuf.type;
 ((trav_info_t *)op_data)->name = (char *)HDstrdup(name);

 /* Define 1 for return. This will cause the iterator to stop */
 return 1;
}


/*-------------------------------------------------------------------------
 * Function: get_name_type
 *
 * Purpose:
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: October 10, 2002
 *
 * Return:
 *     Success: The return value of the first operator that
 *              returns non-zero, or zero if all members were
 *              processed with no operator returning non-zero.
 *
 *     Failure: Negative if something goes wrong within the
 *              library, or the negative value returned by one
 *              of the operators.
 *
 *-------------------------------------------------------------------------
 */

herr_t get_name_type( hid_t loc_id,
                      const char *group_name,
                      int idx,
                      char **name,
                      H5G_obj_t *type )
{

 trav_info_t info;

 if (H5Giterate( loc_id, group_name, &idx, opget_info, (void *)&info) < 0 )
  return -1;

 *name = info.name;
 *type = info.type;

 return 0;
}

/*-------------------------------------------------------------------------
 * Function: traverse
 *
 * Purpose: recursive function that searches HDF5 objects in LOC_ID
 *
 * Return: number of objects found in LOC_ID
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: November 4, 2002
 *
 *-------------------------------------------------------------------------
 */

static int traverse( hid_t loc_id,
              const char *group_name,
              trav_table_t *table,
              trav_info_t *info,
              int *idx,
              int print)
{
 haddr_t       objno;              /* Compact form of object's location */
 char          *name=NULL;
 H5G_obj_t     type;
 int           n_names;
 char          *path=NULL;
 H5G_stat_t    statbuf;
 int           inserted_objs=0;
 int           i, j;

 /* get the number of names */
 if (( n_names = (int)get_nnames( loc_id, group_name )) < 0 )
  return -1;

 for ( i = 0; i < n_names; i++)
 {
  if (get_name_type( loc_id, group_name, i, &name, &type ) < 0 )
   return -1;

  /* allocate path buffer */
  path = (char*) HDmalloc(HDstrlen(group_name) + HDstrlen(name) + 2);

  /* initialize path */
  HDstrcpy( path, group_name );
  if ( HDstrcmp(group_name, "/") != 0 )
   HDstrcat( path, "/" );
  HDstrcat( path, name );

  /* disable error reporting */
  H5E_BEGIN_TRY {

  /* get info */
   H5Gget_objinfo( loc_id, path, FALSE, &statbuf);
  } H5E_END_TRY;
  objno = (haddr_t)statbuf.objno[0] | ((haddr_t)statbuf.objno[1] << (8 * sizeof(long)));

  /* add to array */
  if ( info )
  {
   info[*idx].name = (char *)HDstrdup(path);
   info[*idx].type = type;
   (*idx)++;
  }


  switch ( type )
  {

  /*-------------------------------------------------------------------------
   * H5G_GROUP
   *-------------------------------------------------------------------------
   */

  case H5G_GROUP:

    /* increment */
   inserted_objs++;

   /* nlink is number of hard links to object */
   if (statbuf.nlink > 0  && trav_table_search(objno, table ) == -1)
   {
    /* add object to table */
    trav_table_add(objno, path, H5G_GROUP, table );

    /* print it */
    if (print)
     printf(" %-10s %s\n", "group", path  );

    /* recurse with the absolute name */
    inserted_objs += traverse( loc_id, path, table, info, idx, print );
   }

     /* search table
       group with more than one link to it */
   if (statbuf.nlink > 1)
   {
    if ((j = trav_table_search(objno, table )) < 0 )
     return -1;

    trav_table_addlink(table,j,path);

    if ( table->objs[j].displayed == 0 )
    {
     table->objs[j].displayed = 1;
    }
    else
    {
     /* print it */
     if (print)
      printf(" %-10s %s %s %s\n", "group", path, "->", table->objs[j].name  );
    }

   }

   break;

  /*-------------------------------------------------------------------------
   * H5G_DATASET
   *-------------------------------------------------------------------------
   */

  case H5G_DATASET:

    /* increment */
   inserted_objs++;

   /* nlink is number of hard links to object */
   if (statbuf.nlink > 0  && trav_table_search(objno, table ) == -1)
   {
    /* add object to table */
    trav_table_add(objno, path, H5G_DATASET, table );

    /* print it */
    if (print)
     printf(" %-10s %s\n", "dataset", path  );
   }

   /* search table
       dataset with more than one link to it */
   if (statbuf.nlink > 1)
   {
    if ((j = trav_table_search(objno, table )) < 0 )
     return -1;

    trav_table_addlink(table,j,path);

    if ( table->objs[j].displayed == 0 )
    {
     table->objs[j].displayed = 1;
    }
    else
    {
     /* print it */
     if (print)
      printf(" %-10s %s %s %s\n", "dataset", path, "->", table->objs[j].name  );
    } /* displayed==1 */
   } /* nlink>1 */


   break;

  /*-------------------------------------------------------------------------
   * H5G_TYPE
   *-------------------------------------------------------------------------
   */

  case H5G_TYPE:

   /* increment */
   inserted_objs++;

   /* nlink is number of hard links to object */
   if (statbuf.nlink > 0  && trav_table_search(objno, table ) == -1)
   {
    /* add object to table */
    trav_table_add(objno, path, H5G_TYPE, table );

     /* print it */
    if (print)
     printf(" %-10s %s\n", "datatype", path  );
   }

   break;


  /*-------------------------------------------------------------------------
   * H5G_LINK
   *-------------------------------------------------------------------------
   */

  case H5G_LINK:
  {
    /* increment */
    inserted_objs++;

    /* add object to table */
    trav_table_add(HADDR_UNDEF, path, H5G_LINK, table );

    if (statbuf.linklen>0)
    {
     char *targbuf;

     targbuf = HDmalloc(statbuf.linklen);
     assert(targbuf);
     H5Gget_linkval(loc_id,path,statbuf.linklen,targbuf);
     if (print)
      printf(" %-10s %s -> %s\n", "link", path, targbuf);
     free(targbuf);
    }
    else
    {
     if (print)
      printf(" %-10s %s ->\n", "link", path);
    }
   }

   break;

  /*-------------------------------------------------------------------------
   * H5G_UDLINK
   *-------------------------------------------------------------------------
   */

  case H5G_UDLINK:
  {
    H5L_linkinfo_t linkbuf;

    /* increment */
    inserted_objs++;

    /* add object to table */
    trav_table_add(HADDR_UNDEF, path, H5G_UDLINK, table );

    /* Get type of link */
    H5E_BEGIN_TRY {

    /* get link class info */
    H5Lget_linkinfo( loc_id, path, &linkbuf, H5P_DEFAULT);
    } H5E_END_TRY;

    if(linkbuf.linkclass == H5L_LINK_EXTERNAL)
    {
      if (statbuf.linklen>0)
      {
      char *targbuf;
      char *objname;

      targbuf = HDmalloc(statbuf.linklen);
      assert(targbuf);
      H5Gget_linkval(loc_id,path,statbuf.linklen,targbuf);
      H5Lunpack_elink_val(targbuf, NULL, &objname);
      if (print)
        printf(" %-10s %s -> %s %s\n", "ext link", path, targbuf, objname);
      free(targbuf);
      }
      else
      {
      if (print)
        printf(" %-10s %s ->\n", "udlink", path);
      }
    }
    else  /* Unknown user-defined type */
    {
      if (print)
        printf(" %-10s %s ->\n", "UD link type", path);
    }
  }
  break;


  default:
    HDfprintf(stderr, "traverse: Unknown object %d!\n", type); /* JAMES */
    return (-1);
   break;

  }

  /*-------------------------------------------------------------------------
   * end switch
   *-------------------------------------------------------------------------
   */

  if ( name )
   HDfree( name );

  if ( path )
   HDfree( path );

 } /* i */

 return inserted_objs;
}




/*-------------------------------------------------------------------------
 * Function: h5trav_printinfo
 *
 * Purpose: print list of names in file
 *
 * Return: void
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: May 9, 2003
 *
 *-------------------------------------------------------------------------
 */
void h5trav_printinfo(int nobjs, trav_info_t *travi)
{
 int i;
 for ( i = 0; i < nobjs; i++)
 {
  switch ( travi[i].type )
  {
  case H5G_GROUP:
   printf(" %-10s %s\n", "group", travi[i].name  );
   break;
  case H5G_DATASET:
   printf(" %-10s %s\n", "dataset", travi[i].name );
   break;
  case H5G_TYPE:
   printf(" %-10s %s\n", "datatype", travi[i].name );
   break;
  case H5G_LINK:
   printf(" %-10s %s\n", "link", travi[i].name );
   break;
  case H5G_UDLINK:
   printf(" %-10s %s\n", "User defined link", travi[i].name );
   break;
  default:
   printf(" %-10s %s\n", "User defined object", travi[i].name );
   break;
  }
 }
}



/*-------------------------------------------------------------------------
 * Function: h5trav_printtable
 *
 * Purpose: print list of objects in file
 *
 * Return: void
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: May 9, 2003
 *
 *-------------------------------------------------------------------------
 */
void h5trav_printtable(trav_table_t *table)
{
 int i, j;

 for ( i = 0; i < table->nobjs; i++)
 {
  switch ( table->objs[i].type )
  {
  case H5G_GROUP:
   printf(" %-10s %s\n", "group", table->objs[i].name  );
   break;
  case H5G_DATASET:
   printf(" %-10s %s\n", "dataset", table->objs[i].name );
   break;
  case H5G_TYPE:
   printf(" %-10s %s\n", "datatype", table->objs[i].name );
   break;
  case H5G_LINK:
   printf(" %-10s %s\n", "link", table->objs[i].name );
   break;
  default:
   printf(" %-10s %s\n", "User defined object", table->objs[i].name );
   break;
  }

  if (table->objs[i].nlinks)
  {
   for ( j=0; j<table->objs[i].nlinks; j++)
   {
    printf(" %-10s %s\n", "    hardlink", table->objs[i].links[j].new_name );
   }
  }

 }
}


/*-------------------------------------------------------------------------
 * Function: h5trav_getindext
 *
 * Purpose: get index of NAME in list
 *
 * Return: index, -1 if not found
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: December 18, 2003
 *
 *-------------------------------------------------------------------------
 */

int h5trav_getindext(const char *name, trav_table_t *table)
{
 char *pdest;
 int  result;
 int  i, j;

 for ( i = 0; i < table->nobjs; i++)
 {
  if ( strcmp(name,table->objs[i].name)==0 )
   return i;

  pdest  = strstr( table->objs[i].name, name );
  result = (int)(pdest - table->objs[i].name);

  /* found at position 1, meaning without '/' */
  if( pdest != NULL && result==1 && strlen(table->objs[i].name)-1==strlen(name))
   return i;

  /* search also in the list of links */
  if (table->objs[i].nlinks)
  {
   for ( j=0; j<table->objs[i].nlinks; j++)
   {
    if ( strcmp(name,table->objs[i].links[j].new_name)==0 )
     return i;

    pdest  = strstr( table->objs[i].links[j].new_name, name );
    result = (int)(pdest - table->objs[i].links[j].new_name);

    /* found at position 1, meaning without '/' */
    if( pdest != NULL && result==1 )
     return i;

   }
  }

 }
 return -1;
}

