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
#include "h5repack.h"



/*-------------------------------------------------------------------------
 * Function: options_table_init
 *
 * Purpose: init options table
 *
 * Return: 0, ok, -1, fail
 *
 *-------------------------------------------------------------------------
 */

int options_table_init( pack_opttbl_t **tbl )
{
 int i, j;
 pack_opttbl_t* table = (pack_opttbl_t*) malloc(sizeof(pack_opttbl_t));
 if (table==NULL) {
  printf("Error: not enough memory for options table\n");
  return -1;
 }
 
 table->size   = 30;
 table->nelems = 0;
 table->objs   = (pack_info_t*) malloc(table->size * sizeof(pack_info_t));
 if (table->objs==NULL) {
  printf("Error: not enough memory for options table\n");
  return -1;
 }
 
 for ( i=0; i<table->size; i++) {
   strcpy(table->objs[i].path,"\0");
   table->objs[i].filter.filtn  = -1;
   for ( j=0; j<CDVALUES; j++) 
    table->objs[i].filter.cd_values[j] = -1;
   table->objs[i].chunk.rank = -1;
   table->objs[i].refobj_id = -1;
   table->objs[i].layout = -1;
  }
 
 *tbl = table;
 return 0;
}

/*-------------------------------------------------------------------------
 * Function: options_table_free
 *
 * Purpose: free table memory
 *
 * Return: 0
 *
 *-------------------------------------------------------------------------
 */

int options_table_free( pack_opttbl_t *table )
{
 free(table->objs);
 free(table);
 return 0;
}

/*-------------------------------------------------------------------------
 * Function: options_add_layout
 *
 * Purpose: add a layout option to the option list
 *
 * Return: 0, ok, -1, fail
 *
 *-------------------------------------------------------------------------
 */


int options_add_layout( obj_list_t *obj_list,
                        int n_objs,
                        pack_info_t *pack,
                        pack_opttbl_t *table )
{
 int i, j, k, I, added=0, found=0;
 
 if (table->nelems+n_objs >= table->size) {
  table->size += n_objs;
  table->objs = (pack_info_t*)realloc(table->objs, table->size * sizeof(pack_info_t));
  if (table->objs==NULL) {
   printf("Error: not enough memory for options table\n");
   return -1;
  }
  for (i = table->nelems; i < table->size; i++) {
   strcpy(table->objs[i].path,"\0");
   table->objs[i].filter.filtn  = -1;
   for ( j=0; j<CDVALUES; j++) 
    table->objs[i].filter.cd_values[j] = -1;
   table->objs[i].chunk.rank = -1;
   table->objs[i].refobj_id = -1;
   table->objs[i].layout = -1;
  }
 }

 
 /* search if this object is already in the table; "path" is the key */
 if (table->nelems>0)
 {
  /* go tru the supplied list of names */
  for (j = 0; j < n_objs; j++) 
  {
   /* linear table search */
   for (i = 0; i < table->nelems; i++) 
   {
    /*already on the table */
    if (strcmp(obj_list[j].obj,table->objs[i].path)==0)
    {
     /* already chunk info inserted for this one; exit */
     if (table->objs[i].chunk.rank>0)
     {
      printf("Input Error: chunk information already inserted for <%s>\n",obj_list[j].obj);
      exit(1);
     }
     /* insert the layout info */
     else
     {
      table->objs[i].layout = pack->layout;
      if (H5D_CHUNKED==pack->layout) {
       table->objs[i].chunk.rank = pack->chunk.rank;
       for (k = 0; k < pack->chunk.rank; k++) 
        table->objs[i].chunk.chunk_lengths[k] = pack->chunk.chunk_lengths[k];
      }
      found=1;
      break;
     }
    } /* if */
   } /* i */
   
   if (found==0)
   {
    /* keep the grow in a temp var */
    I = table->nelems + added;  
    added++;
    strcpy(table->objs[I].path,obj_list[j].obj);
    table->objs[I].layout = pack->layout;
    if (H5D_CHUNKED==pack->layout) {
     table->objs[I].chunk.rank = pack->chunk.rank;
     for (k = 0; k < pack->chunk.rank; k++) 
      table->objs[I].chunk.chunk_lengths[k] = pack->chunk.chunk_lengths[k];
    }
   }
  } /* j */ 
 }
 
 /* first time insertion */
 else
 {
  /* go tru the supplied list of names */
  for (j = 0; j < n_objs; j++) 
  {
   I = table->nelems + added;  
   added++;
   strcpy(table->objs[I].path,obj_list[j].obj);
   table->objs[I].layout = pack->layout;
   if (H5D_CHUNKED==pack->layout) {
    table->objs[I].chunk.rank = pack->chunk.rank;
    for (k = 0; k < pack->chunk.rank; k++) 
     table->objs[I].chunk.chunk_lengths[k] = pack->chunk.chunk_lengths[k];
   }
  }
 }
 
 table->nelems+= added;
 
 return 0;
}



/*-------------------------------------------------------------------------
 * Function: options_add_filter
 *
 * Purpose: add a compression -f option to the option list
 *
 * Return: 0, ok, -1, fail
 *
 *-------------------------------------------------------------------------
 */

int options_add_filter(obj_list_t *obj_list,
                       int n_objs,
                       filter_info_t filt,
                       pack_opttbl_t *table )
{
 
 int i, j, I, added=0, found=0;
 
 if (table->nelems+n_objs >= table->size) {
  table->size += n_objs;
  table->objs = (pack_info_t*)realloc(table->objs, table->size * sizeof(pack_info_t));
  if (table->objs==NULL) {
   printf("Error: not enough memory for options table\n");
   return -1;
  }
  for (i = table->nelems; i < table->size; i++) {
   strcpy(table->objs[i].path,"\0");
   table->objs[i].filter.filtn  = -1;
   for ( j=0; j<CDVALUES; j++) 
    table->objs[i].filter.cd_values[j] = -1;
   table->objs[i].chunk.rank = -1;
   table->objs[i].refobj_id = -1;
   table->objs[i].layout = -1;
  }
 }
 
 /* search if this object is already in the table; "path" is the key */
 if (table->nelems>0)
 {
  /* go tru the supplied list of names */
  for (j = 0; j < n_objs; j++) 
  {
   /* linear table search */
   for (i = 0; i < table->nelems; i++) 
   {
    /*already on the table */
    if (strcmp(obj_list[j].obj,table->objs[i].path)==0)
    {
     /* insert */
     
     table->objs[i].filter = filt;
     found=1;
     break;
    } /* if */
   } /* i */
   
   if (found==0)
   {
    /* keep the grow in a temp var */
    I = table->nelems + added;  
    added++;
    strcpy(table->objs[I].path,obj_list[j].obj);
    table->objs[I].filter = filt;
   }
  } /* j */ 
 }
 
 /* first time insertion */
 else
 {
  /* go tru the supplied list of names */
  for (j = 0; j < n_objs; j++) 
  {
   I = table->nelems + added;  
   added++;
   strcpy(table->objs[I].path,obj_list[j].obj);
   table->objs[I].filter = filt;
  }
 }
 
 table->nelems+= added;
 
 return 0;
}

/*-------------------------------------------------------------------------
 * Function: options_get_object
 *
 * Purpose: get object from table; "path" is the key
 *
 * Return: pack_info_t* OBJECT or NULL if not found; PATH is the key
 *
 *-------------------------------------------------------------------------
 */

pack_info_t* options_get_object( const char *path,
                                 pack_opttbl_t *table )
{
 int i;
 
 for ( i = 0; i < table->nelems; i++) 
 {
  /* found it */
  if (strcmp(table->objs[i].path,path)==0)
  {
   return (&table->objs[i]);
  }
 }
 
 return NULL;
}




