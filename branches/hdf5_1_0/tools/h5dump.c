#include "h5dump.h"
#include <stdio.h>
#include "H5private.h"

int indent = 0;
int ischar=0;
static int display_data = 1;
static int status = 0;
static int unamedtype = 0;  /* shared data type with no name */

typedef struct shared_obj_t{
unsigned long objno[2];
char objname[1024];
int displayed;
} shared_obj_t;

typedef struct table_t{
int size;
int nobjs;
shared_obj_t *objs;
} table_t;

static int prefix_len = 1024;
static char *prefix;
static table_t group_table, dset_table, type_table;

static void dump_group (hid_t , const char* );
static void dump_dataset (hid_t, const  char*);
static void dump_data (hid_t, int);
static void dump_named_datatype (hid_t , const char *);
static int search_obj (table_t, unsigned long *);

/*-------------------------------------------------------------------------
 * Function:    usage
 *
 * Purpose:     Print the usage message about dumper
 *
 * Return:      void
 *
 * Programmer:  Ruey-Hsia Li
 *
 * Modifications:
 *
 *-----------------------------------------------------------------------*/
static void
usage(void)
{
fprintf(stderr, 
"\nUsage of HDF5 Dumper:\n\n  \
h5dump [-h] [-bb] [-header] [-a <names>] [-d <names>] [-g <names>]\n  \
       [-l <names>] [-t <names>] <file>\n\n\
  -h            Print information on this command.\n\
  -bb           Display the conent of the boot block. The default is not to display.\n\
  -header       Display header only; no data is displayed.\n\
  -a <names>    Display the specified attribute(s).\n\
  -d <names>    Display the specified dataset(s).\n\
  -g <names>    Display the specified group(s) and all the members.\n\
  -l <names>    Displays the value(s) of the specified soft link(s).\n\
  -t <names>    Display the specified named data type(s).\n\
  \n\
  <names> is one or more appropriate object names.\n\n");
}


/*-------------------------------------------------------------------------
 * Function:    indentation
 *
 * Purpose:     Print spaces for indentation
 *
 * Return:      void
 *
 * Programmer:  Ruey-Hsia Li
 *
 * Modifications:
 *
 *-----------------------------------------------------------------------*/
static void indentation(int x) {

  while (x>0) { printf(" "); x--; }

}


/*-------------------------------------------------------------------------
 * Function:    print_datatype
 *
 * Purpose:     print the data type.
 *
 * Return:      void
 *
 * Programmer:  Ruey-Hsia Li
 *
 * Modifications:
 *
 *-----------------------------------------------------------------------*/
static void
print_datatype(hid_t type) {
char *fname ;
hid_t nmembers, mtype;
int i, j, ndims, perm[H5DUMP_MAX_NDIMS];
size_t dims[H5DUMP_MAX_NDIMS];

H5G_stat_t statbuf;

    switch (H5Tget_class(type)) {

    case H5T_INTEGER:

        if (H5Tequal(type, H5T_STD_I8BE))
            printf( "H5T_STD_I8BE");
        else if (H5Tequal(type, H5T_STD_I8LE))
            printf( "H5T_STD_I8LE");
        else if (H5Tequal(type, H5T_STD_I16BE))
            printf( "H5T_STD_I16BE");
        else if (H5Tequal(type, H5T_STD_I16LE))
            printf( "H5T_STD_I16LE");
        else if (H5Tequal(type, H5T_STD_I32BE))
            printf( "H5T_STD_I32BE");
        else if (H5Tequal(type, H5T_STD_I32LE))
            printf( "H5T_STD_I32LE");
        else if (H5Tequal(type, H5T_STD_I64BE))
            printf( "H5T_STD_I64BE");
        else if (H5Tequal(type, H5T_STD_I64LE))
            printf( "H5T_STD_I64LE");
        else if (H5Tequal(type, H5T_STD_U8BE))
            printf( "H5T_STD_U8BE");
        else if (H5Tequal(type, H5T_STD_U8LE))
            printf( "H5T_STD_U8LE");
        else if (H5Tequal(type, H5T_STD_U16BE))
            printf( "H5T_STD_U16BE");
        else if (H5Tequal(type, H5T_STD_U16LE))
            printf( "H5T_STD_U16LE");
        else if (H5Tequal(type, H5T_STD_U32BE))
            printf( "H5T_STD_U32BE");
        else if (H5Tequal(type, H5T_STD_U32LE))
            printf( "H5T_STD_U32LE");
        else if (H5Tequal(type, H5T_STD_U64BE))
            printf( "H5T_STD_U64BE");
        else if (H5Tequal(type, H5T_STD_U64LE))
            printf( "H5T_STD_U64LE");
        else if (H5Tequal(type, H5T_NATIVE_CHAR)) 
            printf( "H5T_NATIVE_CHAR");
        else if (H5Tequal(type, H5T_NATIVE_UCHAR))
            printf( "H5T_NATIVE_UCHAR");
        else if (H5Tequal(type, H5T_NATIVE_SHORT))
            printf( "H5T_NATIVE_SHORT");
        else if (H5Tequal(type, H5T_NATIVE_USHORT))
            printf( "H5T_NATIVE_USHORT");
        else if (H5Tequal(type, H5T_NATIVE_INT))
            printf( "H5T_NATIVE_INT");
        else if (H5Tequal(type, H5T_NATIVE_UINT))
            printf( "H5T_NATIVE_UINT");
        else if (H5Tequal(type, H5T_NATIVE_LONG))
            printf( "H5T_NATIVE_LONG");
        else if (H5Tequal(type, H5T_NATIVE_ULONG))
            printf( "H5T_NATIVE_ULONG");
        else if (H5Tequal(type, H5T_NATIVE_LLONG))
            printf( "H5T_NATIVE_LLONG");
        else if (H5Tequal(type, H5T_NATIVE_ULLONG))
            printf( "H5T_NATIVE_ULLONG");
        else {
            printf( "undefined integer");
            status = 1;
        }
        break;

    case H5T_FLOAT: 
        if (H5Tequal(type, H5T_IEEE_F32BE))
            printf( "H5T_IEEE_F32BE");
        else if (H5Tequal(type, H5T_IEEE_F32LE))
            printf( "H5T_IEEE_F32LE");
        else if (H5Tequal(type, H5T_IEEE_F64BE))
            printf( "H5T_IEEE_F64BE");
        else if (H5Tequal(type, H5T_IEEE_F64LE))
            printf( "H5T_IEEE_F64LE");
        else if (H5Tequal(type, H5T_NATIVE_FLOAT))
            printf( "H5T_NATIVE_FLOAT");
        else if (H5Tequal(type, H5T_NATIVE_DOUBLE))
            printf( "H5T_NATIVE_DOUBLE");
        else if (H5Tequal(type, H5T_NATIVE_LDOUBLE))
            printf( "H5T_NATIVE_LDOUBLE");
        else {
            printf( "undefined float");
            status = 1;
        }
        break;

    case H5T_TIME: 
        printf( "H5T_TIME: not yet implemented");

    case H5T_STRING: 

        if (H5Tequal(type,H5T_C_S1))
            printf( "H5T_C_S1");
        else if (H5Tequal(type,H5T_FORTRAN_S1))
            printf( "H5T_FORTRAN_S1");
        else {
            printf( "undefined string");
            status = 1;
        }
        break;

    case H5T_BITFIELD: 

        if (H5Tequal(type, H5T_STD_B8BE))
            printf( "H5T_STD_B8BE");
        else if (H5Tequal(type, H5T_STD_B8LE))
            printf( "H5T_STD_B8LE");
        else if (H5Tequal(type, H5T_STD_B16BE))
            printf( "H5T_STD_B16BE");
        else if (H5Tequal(type, H5T_STD_B16LE))
            printf( "H5T_STD_B16LE");
        else if (H5Tequal(type, H5T_STD_B32BE))
            printf( "H5T_STD_B32BE");
        else if (H5Tequal(type, H5T_STD_B32LE))
            printf( "H5T_STD_B32LE");
        else if (H5Tequal(type, H5T_STD_B64BE))
            printf( "H5T_STD_B64BE");
        else if (H5Tequal(type, H5T_STD_B64LE))
            printf( "H5T_STD_B64LE");
        else {
            printf( "undefined bitfield");
            status = 1;
        }
        break;

    case H5T_OPAQUE: 
        printf( "H5T_OPAQUE: not yet implemented");
        break;

    case H5T_COMPOUND: 

        if (H5Tcommitted(type) > 0) {
            H5Gget_objinfo(type, ".", TRUE, &statbuf);

            i = search_obj (type_table, statbuf.objno);

            indentation (indent+col);
            if (i >= 0) {
                if (!type_table.objs[i].displayed) /* unamed data type */
                    printf("%s %s \"#%lu:%lu\" %s\n", HARDLINK, BEGIN,
                                                      type_table.objs[i].objno[0],
                                                      type_table.objs[i].objno[1], END);
                else
                    printf("%s %s \"%s\" %s\n", HARDLINK, BEGIN,type_table.objs[i].objname, END);
            } else {
                printf("h5dump error: unknown committed type.\n");
                status = 1;
            }

        } else {

            nmembers = H5Tget_nmembers(type);
 
            for (i = 0; i < nmembers; i++) {

                 fname = H5Tget_member_name(type, i);

                 mtype = H5Tget_member_type(type, i);

                 ndims = H5Tget_member_dims(type, i, dims, perm);

                 indentation (indent+col);

                 print_datatype(mtype);

                 printf (" %s", fname);

                 if (ndims != 1 || dims[0] != 1) {
                     for (j = 0; j < ndims; j++) 
                          printf("[%d]",dims[j]);
                 }

                 printf (";\n");

                 free (fname);
            }
        }

        break;

    default:
        printf( "unknown data type");
        status = 1;
        break;
    }

}


/*-------------------------------------------------------------------------
 * Function:    dump_bb
 *
 * Purpose:     Dump the boot block
 *
 * Return:      void
 *
 * Programmer:  Ruey-Hsia Li
 *
 * Modifications:
 *
 *-----------------------------------------------------------------------*/
static void
dump_bb(void) {

    printf ("%s %s boot block not yet implemented %s\n", BOOT_BLOCK, BEGIN, END);

}


/*-------------------------------------------------------------------------
 * Function:    dump_datatype
 *
 * Purpose:     Dump the data type. Data type can be HDF5 predefined
 *              atomic data type or committed/transient data type.
 *
 * Return:      void 
 *
 * Programmer:  Ruey-Hsia Li
 *
 * Modifications:
 *
 *-----------------------------------------------------------------------*/
static void
dump_datatype (hid_t type) {

    indent += col;
    indentation (indent);
    if (H5Tget_class(type) == H5T_COMPOUND) {
        printf ("%s %s\n", DATATYPE, BEGIN);
        print_datatype(type);
        indentation (indent);
        printf ("%s\n", END);
    } else {
        printf ("%s %s \"", DATATYPE, BEGIN);
        print_datatype(type);
        printf ("\" %s\n", END);
    }
    indent -= col;
}


/*-------------------------------------------------------------------------
 * Function:    dump_dataspace
 *
 * Purpose:     Dump the data space. Data space can be named data space,
 *              array, or others.
 *
 * Return:      void    
 *
 * Programmer:  Ruey-Hsia Li
 *
 * Modifications:
 *
 *-----------------------------------------------------------------------*/
static void
dump_dataspace (hid_t space)
{
    hsize_t size[H5DUMP_MAX_NDIMS];
    hsize_t maxsize[H5DUMP_MAX_NDIMS]; 
    int ndims = H5Sget_simple_extent_dims(space, size, maxsize);
    int i;

    indentation (indent+col);

    printf("%s ", DATASPACE);

    if (H5Sis_simple(space)) {

        if (ndims == 0) /* scalar space */
            HDfprintf (stdout, "%s %s ( 0 ) ( 0 ) %s\n",BEGIN, ARRAY, END);
        else {
            HDfprintf (stdout, "%s %s ( %Hu",BEGIN, ARRAY, size[0]);
            for (i = 1; i < ndims; i++) 
                HDfprintf (stdout, ", %Hu", size[i]);
            printf(" ) ");
           
           if (maxsize[0]==H5S_UNLIMITED)
               HDfprintf (stdout, "( %s", "H5S_UNLIMITED");
            else
               HDfprintf (stdout, "( %Hu", maxsize[0]);

            for (i = 1; i < ndims; i++) 
                 if (maxsize[i]==H5S_UNLIMITED)
                     HDfprintf (stdout, ", %s", "H5S_UNLIMITED");
                 else
                     HDfprintf (stdout, ", %Hu", maxsize[i]);

            printf(" ) %s\n", END);
        }

    } else
        printf("%s not yet implemented %s\n", BEGIN, END);

}


/*-------------------------------------------------------------------------
 * Function:    dump_attr
 *
 * Purpose:     dump the attribute
 *
 * Return:      Success:        SUCCEED
 *
 *              Failure:        FAIL
 *
 * Programmer:  Ruey-Hsia Li
 *
 * Modifications:
 *
 *-----------------------------------------------------------------------*/
static herr_t
dump_attr (hid_t attr, const char *attr_name, void __unused__ *op_data)
{
hid_t  attr_id, type, space;

    indentation(indent);
    begin_obj (ATTRIBUTE, attr_name); 

    if ((attr_id = H5Aopen_name (attr, attr_name))>= 0) {

        type = H5Aget_type(attr_id);
        space = H5Aget_space(attr_id);
        dump_datatype(type);
        dump_dataspace(space);
        if (display_data) dump_data(attr_id, ATTRIBUTE_DATA);
        H5Tclose(type);
        H5Sclose(space);
	H5Aclose (attr_id);
        indentation (indent);
        end_obj();

    } else {
        indentation (indent+col);
        printf("h5dump error: unable to open attribute.\n");
        indentation (indent);
        end_obj();
        status = 1;
        return FAIL;
    }


    return SUCCEED;

}

/*-------------------------------------------------------------------------
 * Function:    dump_selected_attr
 *
 * Purpose:     dump the selected attribute
 *
 * Return:      Success:        SUCCEED
 *
 *              Failure:        FAIL
 *
 * Programmer:  Ruey-Hsia Li
 *
 * Modifications:
 *
 *-----------------------------------------------------------------------*/
static herr_t
dump_selected_attr (hid_t loc_id, char *name)
{
int j;
char *obj_name, *attr_name;
hid_t  oid, attr_id, type, space;
H5G_stat_t statbuf;


    j = strlen(name)-1;
    obj_name = malloc ((j+2) * sizeof(char));

    /* find the last / */
    while (name[j] != '/' && j >=0) j--;
    /* object name */
    if (j == -1) strcpy(obj_name, "/");
    else strncpy(obj_name, name, j+1);
 
    attr_name = name+j+1;

    begin_obj (ATTRIBUTE, name);

    H5Gget_objinfo(loc_id, obj_name, FALSE, &statbuf);
    switch (statbuf.type) {
    case H5G_GROUP:
         if ((oid = H5Gopen (loc_id, obj_name))<0) {
             indentation (col);
             fprintf (stdout, "h5dump error: unable to open %s\n", obj_name);
             end_obj();
             status = 1;
             return FAIL;
         }
         break;
    case H5G_DATASET:
         if ((oid = H5Dopen (loc_id, obj_name))<0) {
             indentation (col);
             fprintf (stdout, "h5dump error: unable to open %s\n", obj_name);
             end_obj();
             status = 1;
             return FAIL;
         }
         break;
    case H5G_TYPE:
         if ((oid =  H5Topen(loc_id, obj_name)) < 0 ) {
             indentation (col);
             fprintf (stdout, "h5dump error: unable to open %s\n", obj_name);
             end_obj();
             status = 1;
             return FAIL;
         } 
         break;
    default:
         indentation (col);
         fprintf (stdout, "h5dump error: unable to open %s\n", obj_name);
         end_obj();
         status = 1;
         return FAIL;
    }

    if ((attr_id = H5Aopen_name (oid, attr_name))>= 0) {

        type = H5Aget_type(attr_id);
        space = H5Aget_space(attr_id);
        dump_datatype(type);
        dump_dataspace(space);
        if (display_data) dump_data(attr_id, ATTRIBUTE_DATA);
        H5Tclose(type);
        H5Sclose(space);
	H5Aclose (attr_id);
        end_obj();

    } else {
        indentation (col);
        printf("h5dump error: unable to open attribute.\n");
        end_obj();
        status = 1;
        return FAIL;
    }

    switch (statbuf.type) {
    case H5G_GROUP:
         if (H5Gclose (oid) < 0) {
         status = 1;
         return FAIL;
         } 
         break;
          
    case H5G_DATASET:
         if (H5Dclose (oid) < 0 ) {
             status = 1;
             return FAIL;
         } 
         break;

    case H5G_TYPE:
         if (H5Tclose(oid) < 0 ) {
             status = 1;
             return FAIL;
         } 
         break;
    default:
         status = 1;
         return FAIL;
    }

    free(obj_name);
    return SUCCEED;
}


/*-------------------------------------------------------------------------
 * Function:    dump all
 *
 * Purpose:     Dump everything in the specified object
 *
 * Return:      Success:        SUCCEED
 *
 *              Failure:        FAIL
 *
 * Programmer:  Ruey-Hsia Li
 *
 * Modifications:
 *
 *-----------------------------------------------------------------------*/
static herr_t
dump_all (hid_t group, const char *name, void __unused__ *op_data)
{
hid_t obj;
char *buf, *tmp;
H5G_stat_t statbuf;
int i;


    H5Gget_objinfo(group, name, FALSE, &statbuf);

    tmp = (char *) malloc ((strlen(prefix)+strlen(name)+2) * sizeof(char));
    strcpy(tmp, prefix);

    switch (statbuf.type) {
    case H5G_LINK:
        indentation (indent);

        buf = malloc (statbuf.linklen*sizeof(char));

        begin_obj(SOFTLINK, name);
        indentation (indent+col);
        if (H5Gget_linkval (group, name, statbuf.linklen, buf)>=0) 
            printf ("LINKTARGET \"%s\"\n", buf);
        else {
            printf ("h5dump error: unable to get link value.\n");
            status = 1;
        }

        indentation (indent);
        end_obj();
        free (buf);
        break;

    case H5G_GROUP:
         if ((obj=H5Gopen (group, name))>=0) {
             strcat(strcat(prefix,"/"), name);
             dump_group (obj, name);
             strcpy(prefix, tmp); 
             H5Gclose (obj);
         } else {
             printf ("h5dump error: unable to dump group %s\n",name);
             status = 1;
         }

         break;

    case H5G_DATASET:

         if ((obj=H5Dopen (group, name))>=0) {              

             /* hard link */
             H5Gget_objinfo(obj, ".", TRUE, &statbuf);
             if (statbuf.nlink > 1) {
                 i = search_obj (dset_table, statbuf.objno);
                 if (i < 0) {
                     indentation (indent);
                     begin_obj(DATASET, name);
                     indentation (indent+col);
                     printf("h5dump error: internal error\n");
                     indentation (indent);
                     end_obj();
                     status = 1;
                     goto done;
                 } else if (dset_table.objs[i].displayed) {
                     indentation (indent);
                     begin_obj(DATASET, name);
                     indentation (indent+col);
                     printf("%s %s \"%s\" %s\n",HARDLINK, BEGIN, 
                                                dset_table.objs[i].objname,END);
                     indentation (indent);
                     end_obj();
                     goto done;
                 } else {
                     dset_table.objs[i].displayed = 1;
                     strcat(tmp,"/");
                     strcat(tmp,name); 
                     strcpy(dset_table.objs[i].objname, tmp);
                 }
             }
             dump_dataset (obj, name);
             H5Dclose (obj);
         } else {
             printf ("h5dump error: unable to dump dataset %s\n",name);
             status = 1;
         }

         break;

    case H5G_TYPE:
         if ((obj=H5Topen (group, name)) >= 0) {
             dump_named_datatype (obj, name);
             H5Tclose(obj);
         } else {
             printf ("h5dump error: unable to dump data type %s\n",name);
             status = 1;
         }
         break;

    default:
         printf ("h5dump error: unknown object %s\n", name);
         status = 1; 
         return FAIL;

    }

done:
    free(tmp);
    return SUCCEED;

}


/*-------------------------------------------------------------------------
 * Function:    dump_named_datatype
 *
 * Purpose:     Dump named data type
 *
 * Return:      void
 *
 * Programmer:  Ruey-Hsia Li
 *
 * Modifications: Comments: not yet implemented.
 *
 *-----------------------------------------------------------------------*/
static void
dump_named_datatype (hid_t type, const char *name) {
char *fname ;
hid_t nmembers, mtype;
int i, j, ndims, perm[H5DUMP_MAX_NDIMS];
size_t dims[H5DUMP_MAX_NDIMS];

    indentation (indent);
    begin_obj(DATATYPE, name);

    nmembers = H5Tget_nmembers(type);
 
    for (i = 0; i < nmembers; i++) {

         fname = H5Tget_member_name(type, i);

         mtype = H5Tget_member_type(type, i);

         ndims = H5Tget_member_dims(type, i, dims, perm);

         indentation (indent+col);

         print_datatype(mtype);

         printf (" %s", fname);
  
         if (ndims != 1 || dims[0] != 1) {
             for (j = 0; j < ndims; j++) 
                  printf("[%d]",dims[j]);
         }

         printf (";\n");

         free (fname);
    }
        
    indentation (indent);
    end_obj();
}


/*-------------------------------------------------------------------------
 * Function:    dump_group
 *
 * Purpose:     Dump everything within the specified group
 *
 * Return:      void
 *
 * Programmer:  Ruey-Hsia Li
 *
 * Modifications:
 *
 *-----------------------------------------------------------------------*/
static void
dump_group (hid_t gid, const char *name) {
H5G_stat_t statbuf;
hid_t dset, type;
char typename[1024], *tmp;
int i;

    tmp = (char *) malloc ((strlen(prefix)+strlen(name)+2) * sizeof(char));
    strcpy(tmp, prefix);


    indentation (indent);
    begin_obj(GROUP, name);
    indent += col;

    if (!strcmp(name,"/") && unamedtype) { /* dump unamed type in root group */
        for (i = 0; i < type_table.nobjs; i++)
             if (!type_table.objs[i].displayed) {
                 dset = H5Dopen (gid, type_table.objs[i].objname);
                 type = H5Dget_type (dset);
                 sprintf(typename,"#%lu:%lu", type_table.objs[i].objno[0],
                                              type_table.objs[i].objno[1]);
                 dump_named_datatype (type, typename);
                 H5Tclose(type);
                 H5Dclose(dset);
             }
    }

    H5Gget_objinfo(gid, ".", TRUE, &statbuf);
    if (statbuf.nlink > 1) { 

        i = search_obj (group_table, statbuf.objno);
        if (i < 0) {

            indentation (indent);
            printf("h5dump error: internal error\n");
            status = 1;

        } else if (group_table.objs[i].displayed) {

            indentation (indent);
            printf("%s %s \"%s\" %s\n",HARDLINK, BEGIN, group_table.objs[i].objname,END); 

        } else {

            strcpy(group_table.objs[i].objname, prefix);
            group_table.objs[i].displayed=1;
            H5Aiterate (gid, NULL, dump_attr, NULL);
            H5Giterate (gid, ".", NULL, dump_all, NULL);

        } 
           
    } else {

       H5Aiterate (gid, NULL, dump_attr, NULL);
       H5Giterate (gid, ".", NULL, dump_all, NULL);

    }

    indent -= col;
    indentation (indent);
    end_obj();
    
    free(tmp);

}


/*-------------------------------------------------------------------------
 * Function:    dump_dataset
 *
 * Purpose:     Dump the specified data set
 *
 * Return:      void
 *
 * Programmer:  Ruey-Hsia Li
 *
 * Modifications:
 *
 *-----------------------------------------------------------------------*/
static void
dump_dataset (hid_t did, const char *name) {
hid_t  type, space;

    indentation (indent);
    begin_obj(DATASET, name);

    type = H5Dget_type (did);
    space = H5Dget_space (did);
    dump_datatype(type);
    dump_dataspace(space);

    if (display_data)
    switch (H5Tget_class(type)) {
    case H5T_INTEGER:
         dump_data(did, DATASET_DATA);
         break;
    case H5T_FLOAT:
         dump_data(did, DATASET_DATA);
         break;
    case H5T_TIME:
         indent += col;
         indentation (indent);
         indent -= col;
         printf("DATA{ not yet implemented.}\n");
         break;
    case H5T_STRING:
         indent += col;
         indentation (indent);
         indent -= col;
         printf("DATA{ not yet implemented.}\n");
         break;
    case H5T_BITFIELD:
         indent += col;
         indentation (indent);
         indent -= col;
         printf("DATA{ not yet implemented.}\n");
         break;
    case H5T_OPAQUE:
         indent += col;
         indentation (indent);
         indent -= col;
         printf("DATA{ not yet implemented.}\n");
         break;
    case H5T_COMPOUND:
/*
         dump_data(did, DATASET_DATA);
*/
         indent += col;
         indentation (indent);
         indent -= col;
         printf("DATA{ not yet implemented.}\n");
         break;
    default: break;
    }

    indent += col;
    H5Aiterate (did, NULL, dump_attr, NULL);
    indent -= col;

    H5Tclose(type);
    H5Sclose(space);

    indentation (indent);
    end_obj();
    
}


/*-------------------------------------------------------------------------
 * Function:    init_table
 *
 * Purpose:     allocate and initialize tables for shared groups, datasets, 
 *              and committed types
 *
 * Return:      void
 *
 * Programmer:  Ruey-Hsia Li
 *
 * Modifications:
 *
 *-----------------------------------------------------------------------*/
static void
init_table (void){
int i;

    group_table.size = dset_table.size = type_table.size = 20;
    group_table.nobjs = dset_table.nobjs = type_table.nobjs = 0;

    group_table.objs = (shared_obj_t*) malloc(group_table.size*sizeof(shared_obj_t));
    dset_table.objs = (shared_obj_t*) malloc(dset_table.size*sizeof(shared_obj_t));
    type_table.objs = (shared_obj_t*) malloc(type_table.size*sizeof(shared_obj_t));

    for (i = 0; i < group_table.size; i++) {
         group_table.objs[i].objno[0] = group_table.objs[i].objno[1] = 0;
         group_table.objs[i].displayed = 0;
    }

    for (i = 0; i < dset_table.size; i++) {
         dset_table.objs[i].objno[0] = dset_table.objs[i].objno[1] = 0;
         dset_table.objs[i].displayed = 0;
    }

    for (i = 0; i < type_table.size; i++) {
         dset_table.objs[i].objno[0] = dset_table.objs[i].objno[1] = 0;
         dset_table.objs[i].displayed = 0;
    }

    prefix = (char *) malloc(prefix_len * sizeof (char));
    strcpy(prefix, "");
}


/*-------------------------------------------------------------------------
 * Function:    search_obj
 *
 * Purpose:     search the object specified by objno in the table
 *
 * Return:      an integer, the location of the object
 *              -1   if object is not found
 *
 *
 * Programmer:  Ruey-Hsia Li
 *
 * Modifications:
 *
 *-----------------------------------------------------------------------*/
static int 
search_obj (table_t table, unsigned long *objno) {
int i=0, found=0;

    while (i < table.nobjs && !found) 
           if (table.objs[i].objno[0] == *(objno) &&
               table.objs[i].objno[1] == *(objno+1) )  found = 1;
           else     i++; 
  
    if (!found) return -1;
    else return i;

}


/*-------------------------------------------------------------------------
 * Function:    add_obj
 *
 * Purpose:     add a shared object to the table
 *              realloc the table if necessary
 *
 * Return:      void
 *
 * Programmer:  Ruey-Hsia Li
 *
 * Modifications:
 *
 *-----------------------------------------------------------------------*/
static void
add_obj (table_t *table, unsigned long *objno, char *objname) {
int i;

    if (table->nobjs == table->size) {
        table->size *= 2;
        table->objs = realloc (table->objs, table->size*sizeof(shared_obj_t));
        for (i = table->nobjs; i < table->size; i++) {
             table->objs[i].objno[0] = table->objs[i].objno[1] = 0;
             table->objs[i].displayed = 0;
        }
    }

    i = table->nobjs++;
    table->objs[i].objno[0] = *objno;
    table->objs[i].objno[1] = *(objno+1);
    strcpy (table->objs[i].objname, objname);

}


/*-------------------------------------------------------------------------
 * Function:    dump_tables
 *
 * Purpose:     display the contents of tables for debugging purposes
 *
 * Return:      void
 *
 * Programmer:  Ruey-Hsia Li
 *
 * Modifications:
 *
 *-----------------------------------------------------------------------*/
/*
static void 
dump_tables(void) {
int i;

    printf("group_table: # of entries = %d\n", group_table.nobjs);
    for ( i = 0; i < group_table.nobjs; i++)
        printf ("%ul %ul %s\n %d", group_table.objs[i].objno[0],
                                   group_table.objs[i].objno[1],
                                   group_table.objs[i].objname,
                                   group_table.objs[i].displayed);

    printf("\ndset_table: # of entries = %d\n", dset_table.nobjs);
    for ( i = 0; i < dset_table.nobjs; i++)
        printf ("%ul %ul %s %d\n", dset_table.objs[i].objno[0],
                                   dset_table.objs[i].objno[1],
                                   dset_table.objs[i].objname,
                                   dset_table.objs[i].displayed);
   
    printf("\ntype_table: # of entries = %d\n", type_table.nobjs);
    for ( i = 0; i < type_table.nobjs; i++)
        printf ("%ul %ul %s %d\n", type_table.objs[i].objno[0],
                                   type_table.objs[i].objno[1],
                                   type_table.objs[i].objname,
                                   type_table.objs[i].displayed);
}
*/


/*-------------------------------------------------------------------------
 * Function:   Find_shared_objs 
 *
 * Purpose:    Find shared objects, committed types and store them in tables
 *
 * Return:      Success:        SUCCEED
 *
 *              Failure:        FAIL
 *
 * Programmer:  Ruey-Hsia Li
 *
 * Modifications:
 *
 *-----------------------------------------------------------------------*/
static herr_t
find_shared_objs(hid_t group, const char *name, void __unused__ *op_data)
{
hid_t obj, type;
H5G_stat_t statbuf;
char *tmp;
int i;

    H5Gget_objinfo(group, name, TRUE, &statbuf);

    tmp = (char *) malloc ((strlen(prefix)+strlen(name)+2) * sizeof(char));

    strcpy(tmp, prefix); 

    switch (statbuf.type) {

    case H5G_GROUP:
        if ((obj=H5Gopen (group, name))>=0) {

            if (prefix_len < (int)(strlen(prefix) + strlen(name) + 2)) {
                prefix_len *= 2;
                prefix = realloc (prefix, prefix_len * sizeof(char));
            } 
            strcat(strcat(prefix,"/"), name);

            if (statbuf.nlink > 1) {
                if (search_obj (group_table,  statbuf.objno) < 0) {
                    add_obj (&group_table, statbuf.objno, prefix); 
                    H5Giterate (obj, ".", NULL, find_shared_objs, NULL);
                }
            } else 
                H5Giterate (obj, ".", NULL, find_shared_objs, NULL);

            strcpy(prefix, tmp);
            H5Gclose (obj);

        } else 
            status = 1;

        break;

    case H5G_DATASET:

        strcat(tmp,"/");
        strcat(tmp,name); /* absolute name of the data set */
        if (statbuf.nlink > 1  && 
            search_obj (dset_table, statbuf.objno) < 0)
            add_obj (&dset_table, statbuf.objno, tmp);

        if ((obj=H5Dopen (group, name))>=0) {              
             type = H5Dget_type (obj);
             if (H5Tcommitted(type) > 0 ) {
                 H5Gget_objinfo(type, ".", TRUE, &statbuf);
                 if (search_obj (type_table, statbuf.objno) < 0) {
                     add_obj (&type_table, statbuf.objno, tmp) ;
                     type_table.objs[type_table.nobjs-1].displayed = 0;
                 }
             }
             H5Tclose(type);
             H5Dclose (obj);
        } else
             status = 1;
            
        break;

    case H5G_TYPE:
         strcat(tmp,"/");
         strcat(tmp,name); /* absolute name of the type */
         i = search_obj (type_table, statbuf.objno);
         if (i < 0) {
             add_obj (&type_table, statbuf.objno, tmp) ;
             type_table.objs[type_table.nobjs-1].displayed = 1; /* named data type */
         } else {
             strcpy (type_table.objs[i].objname, tmp);
             type_table.objs[i].displayed = 1; /* named data type */
         }
         break;

    default:
        break;
    }

    free (tmp);

    return SUCCEED;
}


/*-------------------------------------------------------------------------
 * Function:    dump_data
 *
 * Purpose:     Dump attribute or dataset data
 *
 * Return:      void
 *
 * Programmer:  Ruey-Hsia Li
 *
 * Modifications:
 *
 *-----------------------------------------------------------------------*/
static void
dump_data (hid_t obj_id, int obj_data) {

    hid_t               f_type ;
    size_t              size ;
    h5dump_t            info;

    indent += col;
    indentation (indent);
    printf("%s %s\n", DATA, BEGIN);

    if (obj_data == DATASET_DATA)
        f_type = H5Dget_type(obj_id);
    else
        f_type = H5Aget_type(obj_id);

    size = H5Tget_size(f_type);

    /* Set to all default values and then override */
    memset(&info, 0, sizeof info);
    info.idx_fmt = "        (%s) ";
    info.line_ncols = 70 - indent;

    /*
     * If the dataset is a 1-byte integer type then format it as an ASCI
     * character string instead of integers.
     */
    if (1==size && H5T_INTEGER==H5Tget_class(f_type)) {
        info.elmt_suf1 = "";
        info.elmt_suf2 = "";
        info.idx_fmt = "        (%s) \"";
/*
        info.line_suf = "\"";
*/
        info.line_suf = "";
        ischar = 1;
    }

   
    /*
     * Print all the values.
     */

    if (h5dump1(stdout, &info, obj_id, -1, obj_data)<0) {
        indentation(indent+col);
        printf("Unable to print data.\n");
    }

    if (1==size && H5T_INTEGER==H5Tget_class(f_type)) 
        ischar = 0;

    indentation(indent);
    printf("%s\n", END);
    indent -= col;

    H5Tclose(f_type);

}


/*-------------------------------------------------------------------------
 * Function:    main
 *
 * Purpose:     HDF5 dumper
 *
 * Return:      Success:	0
 *              Failure:	1
 *
 * Programmer:  Ruey-Hsia Li
 *
 * Modifications:
 *
 *-----------------------------------------------------------------------*/
int
main(int argc, char *argv[]) {
hid_t fid, gid, dsetid, typeid;
hid_t plist=H5P_DEFAULT;
const char *fname = NULL;
int i, index, curr_arg, display_bb=0, display_all=1;
int nopts=0, *opts;
char *buf, name[128], name1[128];
H5G_stat_t statbuf;
void __unused__ *op_data;
void *edata;
hid_t (*func)(void*);

/* Disable error reporting */
H5Eget_auto (&func, &edata);
H5Eset_auto (NULL, NULL);

    if (argc < 2 ) {
        usage();
        exit(1);
    }

    opts = malloc((argc/2) * sizeof (int));
    opts[0] = -1;
    /* parse command line options */
    for (curr_arg = 1; curr_arg < argc; curr_arg++) 

         if (argv[curr_arg][0] == '-') {

             opts[nopts++] = curr_arg;

             if (!strcmp(argv[curr_arg],"-h")) {
                 usage();
                 free(opts);
                 exit(0);

             } else if (!strcmp(argv[curr_arg],"-bb"))

                 display_bb = 1;

             else if (!strcmp(argv[curr_arg],"-header")) 

                 display_data=0;

             else if (strcmp(argv[curr_arg],"-a") &&
                      strcmp(argv[curr_arg],"-d") &&
                      strcmp(argv[curr_arg],"-g") &&
                      strcmp(argv[curr_arg],"-l") &&
                      strcmp(argv[curr_arg],"-t")) {

                 fprintf(stderr, "h5dump error: illegal option %s \n", 
                         argv[curr_arg]);
                 usage();
                 free(opts);
                 exit(1);
             } else display_all = 0;
         } 

    /* check names */
    if (argc == 2) {
        if (opts[0] == 1) { /* argv[1] is an option */
            fprintf(stderr, "h5dump error: no <names> or no <file>\n");
            usage();
            free(opts);
            exit(1);
        }
    } else {
        for (i = 0; i < nopts-1; i++) {
             if (opts[i+1]-opts[i] == 1) {
                if (strcmp(argv[opts[i]], "-bb") &&
                    strcmp(argv[opts[i]], "-header") ) {
                    fprintf(stderr,"h5dump error: no <names> after option %s\n",
                            argv[opts[i]]);
                    usage();
                    free(opts);
                    exit(1);
                } 
             }
        }
        if (argc - opts[nopts-1] == 1) {
            fprintf(stderr,"h5dump error: no <file>\n");
            usage();
            free(opts);
            exit(1);
        } 
        if (argc - opts[nopts-1] == 2) {
            if (strcmp(argv[opts[i]], "-bb") &&
                strcmp(argv[opts[i]], "-header") ) {
                fprintf (stderr, "h5dump error: no <file> or no <names> after option %s\n", argv[opts[i]]);
                usage();
                free(opts);
                exit(1);
            }
        }
    } 


    if (argv[argc-1][0] == '\\') fname = &argv[argc-1][1];
    else fname = argv[argc-1];

    if ((fid = H5Fopen (fname, H5F_ACC_RDONLY, plist)) < 0) {
         fprintf (stderr, "h5dump error: unable to open file %s \n", fname);
         free(opts);
         exit(1);
    }

    /* allocate and initialize internal data structure */
    init_table();

    /* find all shared objects */
    H5Giterate (fid, "/", NULL, find_shared_objs, NULL);
    strcpy(prefix, "");

    /* assign names to unamed shared data type */
    for ( i = 0; i < type_table.nobjs; i++)
          if (type_table.objs[i].displayed == 0) unamedtype = 1;

/*
    #ifdef H5DUMP_DEBUG
        dump_tables();
    #endif
*/

    if (status) {
        printf("internal error! \n");
        goto done;
    }

    begin_obj("HDF5", fname);

    if (display_bb) dump_bb();
 
    if (display_all) {

        if ((gid = H5Gopen (fid, "/")) < 0 ) {
             fprintf(stdout, "h5dump error: unable to open root group\n");
             status = 1;
        } else 
             dump_group(gid, "/");

        if (H5Gclose (gid) < 0) {
            fprintf(stdout, "h5dump error: unable to close root group\n");
            status = 1;
        }

    } else

      for (i = 0; i < nopts; i++) {
           if (!strcmp(argv[opts[i]],"-a")) { 

               for (curr_arg = opts[i]+1; 
                    curr_arg < ((i+1)==nopts?(argc-1):opts[i+1]); 
                    curr_arg++) 

                    dump_selected_attr (fid, argv[curr_arg]);

           } else if (!strcmp(argv[opts[i]],"-d")) {
                for (curr_arg = opts[i]+1; 
                     curr_arg < ((i+1)==nopts?(argc-1):opts[i+1]); 
                     curr_arg++) {

                     if ((dsetid = H5Dopen (fid, argv[curr_arg]))<0) {
                         begin_obj (DATASET, argv[curr_arg]); 
                         indentation (col);
                         fprintf (stdout, "h5dump error: unable to open %s\n", 
                                  argv[curr_arg]);
                         end_obj();
                         status = 1;
                     } else {
                         H5Gget_objinfo(dsetid, ".", TRUE, &statbuf);
                         if (statbuf.nlink > 1) {
                             index = search_obj (dset_table, statbuf.objno);
                             if (index >= 0) {
                                 if (dset_table.objs[index].displayed) {
                                     begin_obj(DATASET, argv[curr_arg]);
                                     indentation (indent+col);
                                     printf("%s %s \"%s\" %s\n",HARDLINK, BEGIN, 
                                                       dset_table.objs[index].objname,END);
                                     indentation (indent);
                                     end_obj();
                                 } else {
                                     strcpy(dset_table.objs[index].objname, argv[curr_arg]);
                                     dset_table.objs[index].displayed = 1;
                                     dump_dataset(dsetid, argv[curr_arg]);
                                 }
                             } else status = 1;
                         } else
                             dump_dataset(dsetid, argv[curr_arg]);
                         if (H5Dclose(dsetid)<1) status = 1;
                     }
          
                }



           } else if (!strcmp(argv[opts[i]],"-g")) {

                for (curr_arg = opts[i]+1; 
                     curr_arg < ((i+1)==nopts?(argc-1):opts[i+1]); 
                     curr_arg++) {
                     if ((gid = H5Gopen (fid, argv[curr_arg])) < 0) {
                         begin_obj (GROUP, argv[curr_arg]); 
                         indentation (col);
                         fprintf (stdout, "h5dump error: unable to open %s\n", 
                                  argv[curr_arg]);
                         end_obj();
                         status = 1;
                     } else {
                         H5Gget_objinfo(gid, ".", TRUE, &statbuf);
                         strcpy(prefix, argv[curr_arg]);
                         dump_group(gid, argv[curr_arg]);
                         if (H5Gclose (gid) < 0) status = 1;
                     }
                }

           } else if (!strcmp(argv[opts[i]],"-l")) {

                for (curr_arg = opts[i]+1; 
                     curr_arg < ((i+1)==nopts?(argc-1):opts[i+1]); 
                     curr_arg++) {


                     if (H5Gget_objinfo(fid, argv[curr_arg], FALSE, &statbuf) < 0) {
                         begin_obj(SOFTLINK, argv[curr_arg]);
                         indentation (col);
                         fprintf(stdout, "h5dump error: unable to get obj info from %s\n", argv[curr_arg]);
                         end_obj();
                         status = 1;

                     } else if (statbuf.type == H5G_LINK) {

                         buf = malloc(statbuf.linklen*sizeof(char));
                         begin_obj(SOFTLINK, argv[curr_arg]);
                         indentation (col);
                         if (H5Gget_linkval (fid, argv[curr_arg], statbuf.linklen, buf)>=0) 
                             printf ("LINKTARGET \"%s\"\n", buf);
                         else {
                             fprintf (stdout, "h5dump error: unable to get link value\n");
                             status = 1;
                         }
                         end_obj();
                         free(buf);

                     } else {
                         begin_obj(SOFTLINK, argv[curr_arg]);
                         indentation (col);
                         fprintf(stdout, "h5dump error: %s is not a link\n", argv[curr_arg]);
                         end_obj();
                         status = 1;
                     }

                 }

           } else if (!strcmp(argv[opts[i]],"-t")) {


                for (curr_arg = opts[i]+1; 
                     curr_arg < ((i+1)==nopts?(argc-1):opts[i+1]); 
                     curr_arg++) {

                     if ((typeid=H5Topen (fid, argv[curr_arg])) < 0) {

                         /* check if argv[curr_arg] is unamed data type */
                         index = 0;
                         while (index < type_table.nobjs ) {
                             if (!type_table.objs[index].displayed) { /* unamed data type */
                                 sprintf(name,"#%lu:%lu\n", type_table.objs[index].objno[0], 
                                                            type_table.objs[index].objno[1]);
                                 sprintf(name1,"/#%lu:%lu\n", type_table.objs[index].objno[0], 
                                                            type_table.objs[index].objno[1]);
                                 if (!strncmp(name, argv[curr_arg], strlen(argv[curr_arg])) || 
                                     !strncmp(name1, argv[curr_arg], strlen(argv[curr_arg]))) {
                                      break;
                                  }
                             } 
                             index++;
                         }
                         if (index ==  type_table.nobjs) {  /* unknown type */
                              begin_obj (DATATYPE, argv[curr_arg]); 
                              indentation (col);
                              fprintf (stdout, "h5dump error: unable to open %s\n", 
                                       argv[curr_arg]);
                              end_obj();
                              status = 1;
                         } else {
                              dsetid = H5Dopen (fid, type_table.objs[index].objname) ;
                              typeid = H5Dget_type (dsetid);
                              dump_named_datatype (typeid, argv[curr_arg]);
                              H5Tclose(typeid);
                              H5Dclose(dsetid);
                         }

                     } else {
                         dump_named_datatype (typeid, argv[curr_arg]);
                         if (H5Tclose(typeid) < 0) status = 1;
                     }
                }
           }
      }

    end_obj();

done:

    H5Eset_auto (func, edata);
    free(opts);
    if (H5Fclose (fid) < 0) status = 1;

    free (group_table.objs);
    free (dset_table.objs);
    free (type_table.objs);
    free (prefix);

    return status;

}
