
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

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <assert.h>
#include "hdf5.h"
#include "h5trav.h"


#if 0
#define H5DIFF_DEBUG
#endif

#define FFORMAT "%-15.10g %-15.10g %-15.10g\n"
#define IFORMAT "%-15d %-15d %-15d\n"
#define LIFORMAT "%-15ld %-15ld %-15ld\n"
/* with -p option */
#define FPFORMAT "%-15.10g %-15.10g %-15.10g %-14.10g\n"
#define IPFORMAT "%-15d %-15d %-15d %-14d\n"
#define LPIFORMAT "%-15ld %-15ld %-15ld %-14ld\n"
#define SPACES  "          "


typedef struct options_t
{
 int    r;       /* report only what objects differ */
 int    d;       /* delta */
 double delta;   /* delta value */
 int    p;       /* relative error */
 double percent; /* relative error value */
 int    n;       /* count */
 int    count;   /* count value */
} options_t;

/*-------------------------------------------------------------------------
 * prototypes
 *-------------------------------------------------------------------------
 */

int diff_dataset( hid_t file1_id, hid_t file2_id, const char *obj1_name, 
                  const char *obj2_name, options_t options );
void diff( hid_t file1_id, const char *obj1_name, hid_t file2_id, const char *obj2_name, 
           options_t options, int type );
void compare( hid_t file1_id, const char *file1_name, const char *obj1_name, 
              int nobjects1, info_t *info1,
              hid_t file2_id, const char *file2_name, const char *obj2_name, 
              int nobjects2, info_t *info2,
              options_t options );
void match( hid_t file1_id, const char *file1_name, int nobjects1, info_t *info1,
            hid_t file2_id, const char *file2_name, int nobjects2, info_t *info2,
            options_t options );
int array_diff( void *buf1, void *buf2, hsize_t tot_cnt, int rank, hsize_t *dims, 
                options_t options, const char *obj1, const char *obj2,
                hid_t m_type );

/*-------------------------------------------------------------------------
 * utility functions
 *-------------------------------------------------------------------------
 */

void list( const char *filename, int nobjects, info_t *info );
void print_pos( int *ph, int p, unsigned int curr_pos, int *acc, 
                int *pos, int rank, const char *obj1, const char *obj2 );
hid_t fixtype( hid_t f_type );
void print_datatype(hid_t type);
int check_n_input( const char* );
int check_f_input( const char* );
int get_index( const char *obj, int nobjects, info_t *info );
int compare_object( char *obj1, char *obj2 );
void usage(void);
const char* basename(const char *name);
const char* get_type(int type);
const char* get_class(H5T_class_t tclass);
void print_dims( int r, hsize_t *d );
void print_sizes( const char *obj1, const char *obj2,
                  hid_t f_type1, hid_t f_type2,
                  hid_t m_type1, hid_t m_type2 );


/*-------------------------------------------------------------------------
 * Function: usage
 *
 * Purpose: print a usage message  
 *
 * Return: void
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: May 9, 2003
 *
 *-------------------------------------------------------------------------
 */
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
 printf("[-r]              Print only what objects differ, not the differences\n");
 printf("[-n count]        Print difference up to count number\n");
 printf("[-d delta]        Print difference when it is greater than limit delta\n");
 printf("[-p relative]     Print difference when it is greater than a relative error\n");
 printf("\n");
 printf("Items in [] are optional\n");
 printf("[obj1] and [obj1] are HDF5 objects (datasets, groups or datatypes)\n");
 printf("The 'count' value must be a positive integer\n");
 printf("The 'delta' and 'relative' values must be positive numbers\n");
 printf("The -d compare criteria is |a - b| > delta\n");
 printf("The -p compare criteria is |1 - b/a| > relative\n");
 printf("\n");
 printf("Examples:\n");
 printf("\n");
 printf("1) h5diff file1 file2 /a/b /a/c\n");
 printf("\n");
 printf("   Compares object '/a/b' in file1 with '/a/c' in file2\n");
 printf("\n");
 printf("2) h5diff file1 file2 /a/b\n");
 printf("\n");
 printf("   Compares object '/a/b' in both files\n");
 printf("\n");
 printf("3) h5diff file1 file2\n");
 printf("\n");
 printf("   Compares all objects in both files\n");
 printf("\n");
 exit(0);
}


/*-------------------------------------------------------------------------
 * Function: main
 *
 * Purpose: H5diff main program
 *
 * Return: 
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

int main(int argc, const char *argv[])
{
 int        i;
 const char *s = NULL;
 hid_t      file1_id, file2_id; 
 int        nobjects1, nobjects2;
 info_t     *info1=NULL;
 info_t     *info2=NULL;
 options_t  options = {0,0,0,0,0,0,0};
 void       *edata;
 hid_t      (*func)(void*);
 const char *file1_name;
 const char *file2_name;
 const char *obj1_name  = NULL;
 const char *obj2_name  = NULL;

/*-------------------------------------------------------------------------
 * print the command line options
 *-------------------------------------------------------------------------
 */

 printf("$h5diff");
 for (i=1; i<argc ; i++) 
 {
  printf(" %s", argv[i] );
 }
 printf("\n");

 
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
  file1_name = argv[1];
  file2_name = argv[2];
 }

/*-------------------------------------------------------------------------
 * open the files first; if they are not valid, no point in continuing
 *-------------------------------------------------------------------------
 */

 /* disable error reporting */
 H5Eget_auto(&func, &edata);
 H5Eset_auto(NULL, NULL);
 
 /* Open the files */
 if ((file1_id=H5Fopen(file1_name,H5F_ACC_RDONLY,H5P_DEFAULT))<0 )
 {
  printf("h5diff: %s: No such file or directory\n", file1_name );
  exit(1);
 }
 if ((file2_id=H5Fopen(file2_name,H5F_ACC_RDONLY,H5P_DEFAULT))<0 )
 {
  printf("h5diff: %s: No such file or directory\n", file2_name );
  exit(1);
 }
 /* enable error reporting */
 H5Eset_auto(func, edata);

 
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
    case 'r': 
     options.r = 1;
     break;
    case 'd': 
     /* if it is not another option */
     if ( '-' != argv[i+1][0] )
     {
      options.d=1;
      if ( check_f_input(argv[i+1])==-1)
      {
       printf("<-d %s> is not a valid option\n", argv[i+1] );
       usage();
      }
      options.delta = atof(argv[i+1]);
     }
     else
     {
      printf("<-d %s> is not a valid option\n", argv[i+1] );
      usage();
     }
     break;
    case 'p': 
     if ( '-' !=argv[i+1][0] )
     {
      options.p=1;
      if ( check_f_input(argv[i+1])==-1)
      {
       printf("<-p %s> is not a valid option\n", argv[i+1] );
       usage();
      }
      options.percent = atof(argv[i+1]);
     }
     break;
    case 'n': 
     if ( '-' !=argv[i+1][0] )
     {
      options.n=1;
      if ( check_n_input(argv[i+1])==-1)
      {
       printf("<-n %s> is not a valid option\n", argv[i+1] );
       usage();
      }
      options.count = atoi(argv[i+1]);
     }
     break;
    } /*switch*/
   } /*for*/ 
  } /*if*/
  
  else /* not single-letter switches */
   
  {
   /* check if it is not a -d, -p parameter */
   if ( '-' !=argv[i-1][0] )
   {
    if ( obj1_name==NULL )
     obj1_name = argv[i];
    if ( obj2_name==NULL )
    {
     /* check if we have a second object name */
     if ( i+1<argc && '-' !=argv[i+1][0] )
      /* yes */
      obj2_name = argv[i+1];
     else
      /* no */
      obj2_name = obj1_name;
    }
   }
  }
  
 }/*for*/
 




/*-------------------------------------------------------------------------
 * get the number of objects in the files
 *-------------------------------------------------------------------------
 */

 nobjects1 = H5get_object_info( file1_id, NULL );
 nobjects2 = H5get_object_info( file2_id, NULL );

/*-------------------------------------------------------------------------
 * get the list of objects in the files
 *-------------------------------------------------------------------------
 */

 info1 = (info_t*) malloc( nobjects1 * sizeof(info_t));
 info2 = (info_t*) malloc( nobjects2 * sizeof(info_t));
 if (info1==NULL || info2==NULL)
  return 0;

 H5get_object_info( file1_id, info1 );
 H5get_object_info( file2_id, info2 );

/*-------------------------------------------------------------------------
 * object name was supplied
 *-------------------------------------------------------------------------
 */
 
 if ( obj1_name )
 {
  compare(file1_id,file1_name,obj1_name,nobjects1,info1,
          file2_id,file2_name,obj2_name,nobjects2,info2,options);
 }

/*-------------------------------------------------------------------------
 * compare all
 *-------------------------------------------------------------------------
 */

 else 
 {
  match(file1_id,file1_name,nobjects1,info1,
        file2_id,file2_name,nobjects2,info2,options);
 }
 
 /* close */
 assert( (H5Fclose(file1_id)) >=0);
 assert( (H5Fclose(file2_id)) >=0);
 
 info_free(info1,nobjects1);
 info_free(info2,nobjects2);
 printf("\n");
 return 0;
 
}


/*-------------------------------------------------------------------------
 * Function: check_n_input
 *
 * Purpose: check for valid input
 *
 * Return: 
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
 * Purpose: check for valid input
 *
 * Return: 
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

int check_f_input( const char *str )
{
 unsigned i;
 char c;

 /* '0' values not allowed */
 if ( strlen(str)==1 && str[0]=='0' )
  return -1;

 for ( i = 0; i < strlen(str); i++)
 {
  c = str[i];
  if ( c < 48 || c > 57  ) /* ascii values between 0 and 9 */
   if  ( c!= 46) /* . */
   return -1;
 }
 return 1;
}

/*-------------------------------------------------------------------------
 * Function: list
 *
 * Purpose: print list of objects in file
 *
 * Return: 
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

void list( const char *filename, int nobjects, info_t *info )
{
 int i;

 printf("File <%s>: # of entries = %d\n", filename, nobjects );
 for ( i = 0; i < nobjects; i++)
 {
  switch ( info[i].type )
  {
  case H5G_GROUP:
   printf("%s %20s\n", info[i].name, "group" );
   break;
  case H5G_DATASET:
   printf("%s %20s\n", info[i].name, "dataset" );
   break;
  case H5G_TYPE:
   printf("%s %20s\n", info[i].name, "datatype" );
   break;
  case H5G_LINK:
   printf("%s %20s\n", info[i].name, "link" );
   break;
  default:
   printf("%s %20s\n", info[i].name, "User defined object" );
   break;
  }
 }

}


/*-------------------------------------------------------------------------
 * Function: compare_object
 *
 * Purpose: do the compare criteria
 *
 * Return: 
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

int compare_object( char *obj1, char *obj2 )
{
 int cmp;
 cmp = strcmp(obj1,obj2);
 return cmp;

}


/*-------------------------------------------------------------------------
 * Function: match
 *
 * Purpose: Find commom objects; the algorithm used for this search is the 
 *  cosequential match algorithm and is described in 
 *  Folk, Michael; Zoellick, Bill. (1992). File Structures. Addison-Wesley.
 *
 * Return: 
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

void match( hid_t file1_id, const char *file1_name, int nobjects1, info_t *info1,
            hid_t file2_id, const char *file2_name, int nobjects2, info_t *info2,
            options_t options )
{
 int  cmp;
 int  more_names_exist = (nobjects1>0 && nobjects2>0) ? 1 : 0;
 int  curr1=0;
 int  curr2=0;
 int  i;
 /*build a common list */
 table_t  *table=NULL;
 unsigned long infile[2]; 
 char c1, c2;

/*-------------------------------------------------------------------------
 * build the list
 *-------------------------------------------------------------------------
 */
 table_init( &table );

 
 while ( more_names_exist )
 {
  cmp = compare_object( info1[curr1].name, info2[curr2].name );
  if ( cmp == 0 )
  {
   infile[0]=1; infile[1]=1;
   table_add(infile, info1[curr1].name, info1[curr1].type, table );

   curr1++;
   curr2++;
  }
  else if ( cmp < 0 )
  {
   infile[0]=1; infile[1]=0;
   table_add(infile, info1[curr1].name, info1[curr1].type, table );
   curr1++;
  }
  else 
  {
   infile[0]=0; infile[1]=1;
   table_add(infile, info2[curr2].name, info2[curr2].type, table );
   curr2++;
  }

  more_names_exist = (curr1<nobjects1 && curr2<nobjects2) ? 1 : 0;

 
 } /* end while */

 /* list1 did not end */
 if (curr1<nobjects1)
 {
  while ( curr1<nobjects1 )
  {
   infile[0]=1; infile[1]=0;
   table_add(infile, info1[curr1].name, info1[curr1].type, table );
   curr1++;
  }
 }

 /* list2 did not end */
 if (curr2<nobjects2)
 {
  while ( curr2<nobjects2 )
  {
   infile[0]=0; infile[1]=1;
   table_add(infile, info2[curr2].name, info2[curr2].type, table );
   curr2++;
  }
 }

/*-------------------------------------------------------------------------
 * print the list
 *-------------------------------------------------------------------------
 */

 printf("file1     file2\n");
 printf("---------------------------------------\n");
 for (i = 0; i < table->nobjs; i++)
 {
  c1 = (table->objs[i].objno[0]) ? 'x' : ' ';
  c2 = (table->objs[i].objno[1]) ? 'x' : ' ';
  printf("%5c %6c    %-15s\n", c1, c2, table->objs[i].objname);
 }
 printf("\n");
  

/*-------------------------------------------------------------------------
 * do the diff for common objects
 *-------------------------------------------------------------------------
 */

 for (i = 0; i < table->nobjs; i++)
 {
  if ( table->objs[i].objno[0]==1 && table->objs[i].objno[1]==1 )
   diff( file1_id, table->objs[i].objname, file2_id, table->objs[i].objname, options, 
    table->objs[i].type );
 }

 /* free table */
 table_free(table);
}



/*-------------------------------------------------------------------------
 * Function: get_index
 *
 * Purpose: get index in list
 *
 * Return: 
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

int get_index( const char *obj, int nobjects, info_t *info )
{
 char *pdest;
 int  result;
 int  i;

 for ( i = 0; i < nobjects; i++) 
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
 * Function: compare
 *
 * Purpose: get objects form list, and check for the same type
 *
 * Return: 
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


void compare( hid_t file1_id, const char *file1_name, const char *obj1_name, 
              int nobjects1, info_t *info1,
              hid_t file2_id, const char *file2_name, const char *obj2_name, 
              int nobjects2, info_t *info2,
              options_t options )
{

 int f1=0, f2=0;

 int i = get_index( obj1_name, nobjects1, info1 );
 int j = get_index( obj2_name, nobjects2, info2 );

 if ( i == -1 )
 {
  printf( "Object <%s> could not be found in <%s>\n", obj1_name, file1_name );
  f1=1;
 }
 if ( j == -1 )
 {
  printf( "Object <%s> could not be found in <%s>\n", obj2_name, file2_name );
  f2=1;
 }
 if ( f1 || f2 )
  return;

  /* use the name with "/" first, as obtained by iterator function */
 obj1_name=info1[i].name;
 obj2_name=info2[j].name;

 /* objects are not the same type */
 if ( info1[i].type != info2[j].type )
 {
  printf("Comparison not supported\n");
  printf("<%s> is of type %s and <%s> is of type %s\n", 
   obj1_name, get_type(info1[i].type), 
   obj2_name, get_type(info2[j].type) );
  return;
 }
  
 diff( file1_id, obj1_name, file2_id, obj2_name, options, info1[i].type );
 
}


/*-------------------------------------------------------------------------
 * Function: diff
 *
 * Purpose: switch between types and choose the diff function
 *
 * Return: 
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


void diff( hid_t file1_id, const char *obj1_name, hid_t file2_id, const char *obj2_name, 
           options_t options, int type )
{
 switch ( type )
 {
 case H5G_DATASET:
  diff_dataset(file1_id,file2_id,obj1_name,obj2_name,options);
  break;
  
 default:
  printf("Comparison not supported\n");
  printf("<%s> is of type %s and <%s> is of type %s\n", 
   obj1_name, get_type(type), 
   obj2_name, get_type(type) );
  break;
 } 
 
 printf("\n");
}

/*-------------------------------------------------------------------------
 * Function: diff_dataset
 *
 * Purpose: check for comparable datasets and read into a compatible 
 *  memory type
 *
 * Return: Success: 0, Failure: -1
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

int diff_dataset( hid_t file1_id, hid_t file2_id, const char *obj1_name, 
                  const char *obj2_name, options_t options )
{

 hid_t   dset1_id  =-1;
 hid_t   dset2_id  =-1;
 hid_t   space1_id =-1;
 hid_t   space2_id =-1;
 hid_t   f_type1=-1, f_type2=-1; /* file data type */ 
 hid_t   m_type1=-1, m_type2=-1; /* memory data type */
 size_t  f_size1, f_size2;       /* size of type in file */
 size_t  m_size1, m_size2;       /* size of type in memory */
 int     rank1, rank2; 
 void    *buf1=NULL, *buf2=NULL;
 hsize_t tot_cnt1, tot_cnt2;
 hsize_t dims1[32], dims2[32], maxdim1[32], maxdim2[32];
 H5T_class_t tclass1;
 H5T_class_t tclass2;
 int     i;
 int     nfound;
 void    *edata;
 hid_t   (*func)(void*);
 const char *name1=NULL; /* relative names */
 const char *name2=NULL;
 int maxdim_diff=0;
 int dim_diff=0;

 /* disable error reporting */
 H5Eget_auto(&func, &edata);
 H5Eset_auto(NULL, NULL);

/*-------------------------------------------------------------------------
 * open the handles
 *-------------------------------------------------------------------------
 */

 /* Open the datasets */
 if ( (dset1_id = H5Dopen(file1_id,obj1_name)) < 0 )
 {
  printf("Cannot open dataset <%s>\n", obj1_name );
  goto out;
 }

 if ( (dset2_id = H5Dopen(file2_id,obj2_name)) < 0 )
 {
  printf("Cannot open dataset <%s>\n", obj2_name );
  goto out;
 }
 /* enable error reporting */
 H5Eset_auto(func, edata);

  /* Get the dataspace handle */
 if ( (space1_id = H5Dget_space(dset1_id)) < 0 )
  goto out;

 /* Get rank */
 if ( (rank1 = H5Sget_simple_extent_ndims(space1_id)) < 0 )
  goto out;

 /* Get the dataspace handle */
 if ( (space2_id = H5Dget_space(dset2_id)) < 0 )
  goto out;

 /* Get rank */
 if ( (rank2 = H5Sget_simple_extent_ndims(space2_id)) < 0 )
  goto out;

 /* Get dimensions */
 if ( H5Sget_simple_extent_dims(space1_id,dims1,maxdim1) < 0 )
  goto out;

 /* Get dimensions */
 if ( H5Sget_simple_extent_dims(space2_id,dims2,maxdim2) < 0 )
  goto out;

/*-------------------------------------------------------------------------
 * Get the file data type 
 *-------------------------------------------------------------------------
 */
 
 /* Get the data type */
 if ( (f_type1 = H5Dget_type(dset1_id)) < 0 )
  goto out;

 /* Get the data type */
 if ( (f_type2 = H5Dget_type(dset2_id)) < 0 )
  goto out;


/*-------------------------------------------------------------------------
 * check for the same class 
 *-------------------------------------------------------------------------
 */

 if ((tclass1=H5Tget_class(f_type1))<0) 
  goto out;

 if ((tclass2=H5Tget_class(f_type2))<0) 
  goto out;

 if ( tclass1 != tclass2 )
 {
  printf("Comparison not supported\n");
  printf("<%s> is of class %s and <%s> is of class %s\n", 
   obj1_name, get_class(tclass1), 
   obj2_name, get_class(tclass2) );
  goto out;
 }

/*-------------------------------------------------------------------------
 * check for non supported classes
 *-------------------------------------------------------------------------
 */

 assert(tclass1==tclass2);
 switch (tclass1) 
 {
 case H5T_TIME:
 case H5T_STRING:
 case H5T_BITFIELD:
 case H5T_OPAQUE:
 case H5T_COMPOUND:
 case H5T_REFERENCE:
 case H5T_ENUM:
 case H5T_VLEN:
 case H5T_ARRAY:
  printf("Comparison not supported\n");
  printf("<%s> is of class %s and <%s> is of class %s\n", 
   obj1_name, get_class(tclass1), 
   obj2_name, get_class(tclass2) );
  goto out;
 default:
  break;
 }



/*-------------------------------------------------------------------------
 * check for the same rank
 *-------------------------------------------------------------------------
 */
 
 if ( rank1 != rank2 )
 {
  printf("Comparison not supported\n");
  printf("<%s> has rank %d, dimensions ", obj1_name, rank1);
  print_dims(rank1,dims1);
  printf(", max dimensions ");
  print_dims(rank1,maxdim1);
  printf("\n" );
  printf("<%s> has rank %d, dimensions ", obj2_name, rank2);
  print_dims(rank2,dims2);
  printf(", max dimensions ");
  print_dims(rank2,maxdim2);
  goto out;
 }

/*-------------------------------------------------------------------------
 * check for different dimensions
 *-------------------------------------------------------------------------
 */
 
 assert(rank1==rank2);
 for ( i=0; i<rank1; i++) 
 {
  if ( maxdim1[i] != maxdim2[i] )
   maxdim_diff=1;
  if ( dims1[i] != dims2[i] )
   dim_diff=1;
 }

/*-------------------------------------------------------------------------
 * current dimensions
 *-------------------------------------------------------------------------
 */

 if (dim_diff==1)
 {
  printf("Comparison not supported\n");
  printf("<%s> has rank %d, dimensions ", obj1_name, rank1);
  print_dims(rank1,dims1);
  printf(", max dimensions ");
  print_dims(rank1,maxdim1);
  printf("\n" );
  printf("<%s> has rank %d, dimensions ", obj2_name, rank2);
  print_dims(rank2,dims2);
  printf(", max dimensions ");
  print_dims(rank2,maxdim2);
  goto out;
 }

/*-------------------------------------------------------------------------
 * maximum dimensions; just give a warning
 *-------------------------------------------------------------------------
 */
 if (maxdim_diff==1)
 {
  printf( "Warning: Different maximum dimensions\n");
  printf("<%s> has max dimensions ", obj1_name);
  print_dims(rank1,maxdim1);
  printf("\n");
  printf("<%s> has max dimensions ", obj2_name);
  print_dims(rank2,maxdim2);
  printf("\n");
 }
  
/*-------------------------------------------------------------------------
 * get number of elements
 *-------------------------------------------------------------------------
 */

 tot_cnt1 = 1;
 for (i = 0; i < rank1; i++) 
 {
  tot_cnt1 *= dims1[i];
 }
 
 tot_cnt2 = 1;
 for (i = 0; i < rank2; i++) 
 {
  tot_cnt2 *= dims2[i];
 }

 assert(tot_cnt1==tot_cnt2);

/*-------------------------------------------------------------------------
 * check for equal file datatype; warning only
 *-------------------------------------------------------------------------
 */

 if ( (H5Tequal(f_type1, f_type2)==0) ) 
 {
  printf("Warning: Different storage datatype\n");
  printf("<%s> has file datatype ", obj1_name);
  print_datatype(f_type1);
  printf("\n");
  printf("<%s> has file datatype ", obj2_name);
  print_datatype(f_type2);
  printf("\n");
 }

/*-------------------------------------------------------------------------
 * memory type and sizes
 *-------------------------------------------------------------------------
 */

 m_type1 = fixtype( f_type1 );
 m_type2 = fixtype( f_type2 );

 f_size1 = H5Tget_size( f_type1 );
 f_size2 = H5Tget_size( f_type2 );
 m_size1 = H5Tget_size( m_type1 );
 m_size2 = H5Tget_size( m_type2 );

#if defined (H5DIFF_DEBUG)
 print_sizes(obj1_name,obj2_name,f_type1,f_type2,m_type1,m_type2);
#endif 


/*-------------------------------------------------------------------------
 * "upgrade" the smaller memory size 
 *-------------------------------------------------------------------------
 */

 if ( m_size1 != m_size2 )
 {
  if ( m_size1 < m_size2 )
  {
   assert( (H5Tclose(m_type1)) >=0);
   m_type1 = fixtype( f_type2 );
   m_size1 = H5Tget_size( m_type1 );
  }
  else
  {
   assert( (H5Tclose(m_type2)) >=0);
   m_type2 = fixtype( f_type1 );
   m_size2 = H5Tget_size( m_type2 );
  }
#if defined (H5DIFF_DEBUG)
  printf("WARNING: Size was upgraded\n");
  print_sizes(obj1_name,obj2_name,f_type1,f_type2,m_type1,m_type2);
#endif 
 }
 assert(m_size1==m_size2);

 buf1 = (void *) malloc((unsigned) (tot_cnt1*m_size1));
 buf2 = (void *) malloc((unsigned) (tot_cnt2*m_size2));

 if ( buf1 == NULL || buf2 == NULL )
 {
  printf( "cannot read into memory\n" );
  goto out;
 }

/*-------------------------------------------------------------------------
 * read
 *-------------------------------------------------------------------------
 */

 if ( H5Dread(dset1_id,m_type1,H5S_ALL,H5S_ALL,H5P_DEFAULT,buf1) < 0 )
  goto out;

 if ( H5Dread(dset2_id,m_type2,H5S_ALL,H5S_ALL,H5P_DEFAULT,buf2) < 0 )
  goto out;

/*-------------------------------------------------------------------------
 * array compare
 *-------------------------------------------------------------------------
 */
 printf( "Comparing <%s> with <%s>\n", obj1_name, obj2_name );
 name1=basename(obj1_name);
 name2=basename(obj2_name);
 nfound = array_diff(buf1,buf2,tot_cnt1,rank1,dims1,options,name1,name2,m_type1);
 printf("%d differences found\n", nfound );

/*-------------------------------------------------------------------------
 * close
 *-------------------------------------------------------------------------
 */

out:

 if ( buf1) free(buf1);
 if ( buf2) free(buf2);
 
 /* Close */
 if ( dset1_id!=-1 )  assert( (H5Dclose(dset1_id)) >=0);
 if ( dset2_id!=-1 )  assert( (H5Dclose(dset2_id)) >=0);
 if ( space1_id!=-1 ) assert( (H5Sclose(space1_id)) >=0);
 if ( space2_id!=-1 ) assert( (H5Sclose(space2_id)) >=0);
 if ( f_type1!=-1 )   assert( (H5Tclose(f_type1)) >=0);
 if ( f_type2!=-1 )   assert( (H5Tclose(f_type2)) >=0);
 if ( m_type1!=-1 )   assert( (H5Tclose(m_type1)) >=0);
 if ( m_type2!=-1 )   assert( (H5Tclose(m_type2)) >=0);
 
 return 0;

}




/*-------------------------------------------------------------------------
 * Function: array_diff
 *
 * Purpose: compare array
 *
 * Return: 
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
 
int array_diff( void *buf1, void *buf2, hsize_t tot_cnt, int rank, hsize_t *dims, 
                options_t options, const char *obj1, const char *obj2,
                hid_t m_type )
{
 H5T_class_t type_class;
 size_t      type_size;
 int         nfound=0; /* number of differences found */
 int         ph=1;     /* print header  */
 int         acc[32];  /* accumulator and matrix position */
 int         pos[32];
 unsigned    i; 
 int         j;
 char        *_buf1 = (char*)buf1;
 char        *_buf2 = (char*)buf2;
 
 /* some temporary store */
 double      temp1_double;
 double      temp2_double;
 float       temp1_float;
 float       temp2_float;
 long        temp1_long;
 long        temp2_long;
 int         temp1_int;
 int         temp2_int;
 short       temp1_short;
 short       temp2_short;
 char        temp1_char;
 char        temp2_char;


 acc[rank-1]=1;
 for(j=(rank-2); j>=0; j--)
 {
  acc[j]=acc[j+1]*(int)dims[j+1];
 }

 /* Get the class. */
 type_class = H5Tget_class( m_type );

 /* Get the size. */
 type_size = H5Tget_size( m_type );

 
 switch(type_class)
 {
 default:
  return -1;
  
 case H5T_INTEGER:

/*-------------------------------------------------------------------------
 * H5T_NATIVE_SCHAR
 *-------------------------------------------------------------------------
 */
  
  if (H5Tequal(m_type, H5T_NATIVE_SCHAR)) 
  {
   assert(type_size==sizeof(char));
   for ( i = 0; i < tot_cnt; i++)
   {
    memcpy(&temp1_char, _buf1, sizeof(char));
    memcpy(&temp2_char, _buf2, sizeof(char));
    /* -d and !-p */
    if (options.d && !options.p)
    {
     if (abs(temp1_char-temp2_char) > options.delta)
     {
      if (options.n && nfound>=options.count)
       return nfound;
      if ( options.r==0 ) 
      {
       print_pos(&ph,0,i,acc,pos,rank,obj1,obj2);
       printf(SPACES);
       printf(IFORMAT,temp1_char,temp2_char,abs(temp1_char-temp2_char));
      }
      nfound++;
     }
    }
    /* !-d and -p */
    else if (!options.d && options.p)
    {
     if ( temp1_char!=0 && abs(1-temp2_char/temp1_char) > options.percent )
     {
      if (options.n && nfound>=options.count)
       return nfound;
      if ( options.r==0 ) 
      {
       print_pos(&ph,1,i,acc,pos,rank,obj1,obj2);
       printf(SPACES);
       printf(IPFORMAT,temp1_char,temp2_char,abs(temp1_char-temp2_char),
        abs(1-temp2_char/temp1_char));
      }
      nfound++;
     }
    }
    /* -d and -p */
    else if ( options.d && options.p)
    {
     if ( temp1_char!=0 && abs(1-temp2_char/temp1_char) > options.percent && 
      abs(temp1_char-temp2_char) > options.delta )
     {
      if (options.n && nfound>=options.count)
       return nfound;
      if ( options.r==0 ) 
      {
       print_pos(&ph,1,i,acc,pos,rank,obj1,obj2);
       printf(SPACES);
       printf(IPFORMAT,temp1_char,temp2_char,abs(temp1_char-temp2_char),
        abs(1-temp2_char/temp1_char));
      }
      nfound++;
     }
    }
    else if (temp1_char != temp2_char)
    {
     if (options.n && nfound>=options.count)
      return nfound;
     if ( options.r==0 ) 
     {
      print_pos(&ph,0,i,acc,pos,rank,obj1,obj2);
      printf(SPACES);
      printf(IFORMAT,temp1_char,temp2_char,abs(temp1_char-temp2_char));
     }
     nfound++;
    }
    
    _buf1+=sizeof(char);
    _buf2+=sizeof(char);
   }/* i */
   
  } /*H5T_NATIVE_SCHAR*/


/*-------------------------------------------------------------------------
 * H5T_NATIVE_SHORT
 *-------------------------------------------------------------------------
 */
  
  else if (H5Tequal(m_type, H5T_NATIVE_SHORT)) 
  {
   assert(type_size==sizeof(short));
   for ( i = 0; i < tot_cnt; i++)
   {
    memcpy(&temp1_short, _buf1, sizeof(short));
    memcpy(&temp2_short, _buf2, sizeof(short));
    /* -d and !-p */
    if (options.d && !options.p)
    {
     if (abs(temp1_short-temp2_short) > options.delta)
     {
      if (options.n && nfound>=options.count)
       return nfound;
      if ( options.r==0 ) 
      {
       print_pos(&ph,0,i,acc,pos,rank,obj1,obj2);
       printf(SPACES);
       printf(IFORMAT,temp1_short,temp2_short,abs(temp1_short-temp2_short));
      }
      nfound++;
     }
    }
    /* !-d and -p */
    else if (!options.d && options.p)
    {
     if ( temp1_short!=0 && abs(1-temp2_short/temp1_short) > options.percent )
     {
      if (options.n && nfound>=options.count)
       return nfound;
      if ( options.r==0 ) 
      {
       print_pos(&ph,1,i,acc,pos,rank,obj1,obj2);
       printf(SPACES);
       printf(IPFORMAT,temp1_short,temp2_short,abs(temp1_short-temp2_short),
        abs(1-temp2_short/temp1_short));
      }
      nfound++;
     }
    }
    /* -d and -p */
    else if ( options.d && options.p)
    {
     if ( temp1_short!=0 && abs(1-temp2_short/temp1_short) > options.percent && 
      abs(temp1_short-temp2_short) > options.delta )
     {
      if (options.n && nfound>=options.count)
       return nfound;
      if ( options.r==0 ) 
      {
       print_pos(&ph,1,i,acc,pos,rank,obj1,obj2);
       printf(SPACES);
       printf(IPFORMAT,temp1_short,temp2_short,abs(temp1_short-temp2_short),
        abs(1-temp2_short/temp1_short));
      }
      nfound++;
     }
    }
    else if (temp1_short != temp2_short)
    {
     if (options.n && nfound>=options.count)
      return nfound;
     if ( options.r==0 ) 
     {
      print_pos(&ph,0,i,acc,pos,rank,obj1,obj2);
      printf(SPACES);
      printf(IFORMAT,temp1_short,temp2_short,abs(temp1_short-temp2_short));
     }
     nfound++;
    }
    
    _buf1+=sizeof(short);
    _buf2+=sizeof(short);
   }/* i */
   
  } /*H5T_NATIVE_SHORT*/


/*-------------------------------------------------------------------------
 * H5T_NATIVE_INT
 *-------------------------------------------------------------------------
 */
  
  else if (H5Tequal(m_type, H5T_NATIVE_INT)) 
  {
   assert(type_size==sizeof(int));
   for ( i = 0; i < tot_cnt; i++)
   {
    memcpy(&temp1_int, _buf1, sizeof(int));
    memcpy(&temp2_int, _buf2, sizeof(int));
    /* -d and !-p */
    if (options.d && !options.p)
    {
     if (abs(temp1_int-temp2_int) > options.delta)
     {
      if (options.n && nfound>=options.count)
       return nfound;
      if ( options.r==0 ) 
      {
       print_pos(&ph,0,i,acc,pos,rank,obj1,obj2);
       printf(SPACES);
       printf(IFORMAT,temp1_int,temp2_int,abs(temp1_int-temp2_int));
      }
      nfound++;
     }
    }
    /* !-d and -p */
    else if (!options.d && options.p)
    {
     if ( temp1_int!=0 && abs(1-temp2_int/temp1_int) > options.percent )
     {
      if (options.n && nfound>=options.count)
       return nfound;
      if ( options.r==0 ) 
      {
       print_pos(&ph,1,i,acc,pos,rank,obj1,obj2);
       printf(SPACES);
       printf(IPFORMAT,temp1_int,temp2_int,abs(temp1_int-temp2_int),
        abs(1-temp2_int/temp1_int));
      }
      nfound++;
     }
    }
    /* -d and -p */
    else if ( options.d && options.p)
    {
     if ( temp1_int!=0 && abs(1-temp2_int/temp1_int) > options.percent && 
      abs(temp1_int-temp2_int) > options.delta )
     {
      if (options.n && nfound>=options.count)
       return nfound;
      if ( options.r==0 ) 
      {
       print_pos(&ph,1,i,acc,pos,rank,obj1,obj2);
       printf(SPACES);
       printf(IPFORMAT,temp1_int,temp2_int,abs(temp1_int-temp2_int),
        abs(1-temp2_int/temp1_int));
      }
      nfound++;
     }
    }
    else if (temp1_int != temp2_int)
    {
     if (options.n && nfound>=options.count)
      return nfound;
     if ( options.r==0 ) 
     {
      print_pos(&ph,0,i,acc,pos,rank,obj1,obj2);
      printf(SPACES);
      printf(IFORMAT,temp1_int,temp2_int,abs(temp1_int-temp2_int));
     }
     nfound++;
    }
    
    _buf1+=sizeof(int);
    _buf2+=sizeof(int);
   }/* i */
   
  } /*H5T_NATIVE_INT*/

/*-------------------------------------------------------------------------
 * H5T_NATIVE_LONG
 *-------------------------------------------------------------------------
 */
  
  else if (H5Tequal(m_type, H5T_NATIVE_LONG)) 
  {
   assert(type_size==sizeof(long));
   for ( i = 0; i < tot_cnt; i++)
   {
    memcpy(&temp1_long, _buf1, sizeof(long));
    memcpy(&temp2_long, _buf2, sizeof(long));
    /* -d and !-p */
    if (options.d && !options.p)
    {
     if (labs(temp1_long-temp2_long) > (long)options.delta)
     {
      if (options.n && nfound>=options.count)
       return nfound;
      if ( options.r==0 ) 
      {
       print_pos(&ph,0,i,acc,pos,rank,obj1,obj2);
       printf(SPACES);
       printf(LIFORMAT,temp1_long,temp2_long,labs(temp1_long-temp2_long));
      }
      nfound++;
     }
    }
    /* !-d and -p */
    else if (!options.d && options.p)
    {
     if ( temp1_long!=0 && labs(1-temp2_long/temp1_long) > (long)options.percent )
     {
      if (options.n && nfound>=options.count)
       return nfound;
      if ( options.r==0 ) 
      {
       print_pos(&ph,1,i,acc,pos,rank,obj1,obj2);
       printf(SPACES);
       printf(LPIFORMAT,temp1_long,temp2_long,labs(temp1_long-temp2_long),
        labs(1-temp2_long/temp1_long));
      }
      nfound++;
     }
    }
    /* -d and -p */
    else if ( options.d && options.p)
    {
     if ( temp1_long!=0 && labs(1-temp2_long/temp1_long) > (long)options.percent && 
      labs(temp1_long-temp2_long) > (long)options.delta )
     {
      if (options.n && nfound>=options.count)
       return nfound;
      if ( options.r==0 ) 
      {
       print_pos(&ph,1,i,acc,pos,rank,obj1,obj2);
       printf(SPACES);
       printf(LPIFORMAT,temp1_long,temp2_long,labs(temp1_long-temp2_long),
        labs(1-temp2_long/temp1_long));
      }
      nfound++;
     }
    }
    else if (temp1_long != temp2_long)
    {
     if (options.n && nfound>=options.count)
      return nfound;
     if ( options.r==0 ) 
     {
      print_pos(&ph,0,i,acc,pos,rank,obj1,obj2);
      printf(SPACES);
      printf(LIFORMAT,temp1_long,temp2_long,labs(temp1_long-temp2_long));
     }
     nfound++;
    }
    
    _buf1+=sizeof(long);
    _buf2+=sizeof(long);
   }/* i */
   
  } /*H5T_NATIVE_LONG*/


  break; /*H5T_INTEGER*/


  case H5T_FLOAT:


/*-------------------------------------------------------------------------
 * H5T_NATIVE_FLOAT
 *-------------------------------------------------------------------------
 */
  
  if (H5Tequal(m_type, H5T_NATIVE_FLOAT)) 
  {
   assert(type_size==sizeof(float));
   for ( i = 0; i < tot_cnt; i++)
   {
    memcpy(&temp1_float, _buf1, sizeof(float));
    memcpy(&temp2_float, _buf2, sizeof(float));
    /* -d and !-p */
    if (options.d && !options.p)
    {
     if (fabs(temp1_float-temp2_float) > options.delta)
     {
      if (options.n && nfound>=options.count)
       return nfound;
      if ( options.r==0 ) 
      {
       print_pos(&ph,0,i,acc,pos,rank,obj1,obj2);
       printf(SPACES);
       printf(FFORMAT,temp1_float,temp2_float,fabs(temp1_float-temp2_float));
      }
      nfound++;
     }
    }
    /* !-d and -p */
    else if (!options.d && options.p)
    {
     if ( temp1_float!=0 && fabs(1-temp2_float/temp1_float) > options.percent )
     {
      if (options.n && nfound>=options.count)
       return nfound;
      if ( options.r==0 ) 
      {
       print_pos(&ph,1,i,acc,pos,rank,obj1,obj2);
       printf(SPACES);
       printf(FPFORMAT,temp1_float,temp2_float,fabs(temp1_float-temp2_float),
        fabs(1-temp2_float/temp1_float));
      }
      nfound++;
     }
    }
    /* -d and -p */
    else if ( options.d && options.p)
    {
     if ( temp1_float!=0 && fabs(1-temp2_float/temp1_float) > options.percent && 
      fabs(temp1_float-temp2_float) > options.delta )
     {
      if (options.n && nfound>=options.count)
       return nfound;
      if ( options.r==0 ) 
      {
       print_pos(&ph,1,i,acc,pos,rank,obj1,obj2);
       printf(SPACES);
       printf(FPFORMAT,temp1_float,temp2_float,fabs(temp1_float-temp2_float),
        fabs(1-temp2_float/temp1_float));
      }
      nfound++;
     }
    }
    else if (temp1_float != temp2_float)
    {
     if (options.n && nfound>=options.count)
      return nfound;
     if ( options.r==0 ) 
     {
      print_pos(&ph,0,i,acc,pos,rank,obj1,obj2);
      printf(SPACES);
      printf(FFORMAT,temp1_float,temp2_float,fabs(temp1_float-temp2_float));
     }
     nfound++;
    }
    
    _buf1+=sizeof(float);
    _buf2+=sizeof(float);
   }/* i */
   
  } /*H5T_NATIVE_FLOAT*/


  else if (H5Tequal(m_type, H5T_NATIVE_DOUBLE)) 
  {
   assert(type_size==sizeof(double));
   for ( i = 0; i < tot_cnt; i++)
   {
    memcpy(&temp1_double, _buf1, sizeof(double));
    memcpy(&temp2_double, _buf2, sizeof(double));
    /* -d and !-p */
    if (options.d && !options.p)
    {
     if (fabs(temp1_double-temp2_double) > options.delta)
     {
      if (options.n && nfound>=options.count)
       return nfound;
      if ( options.r==0 ) 
      {
       print_pos(&ph,0,i,acc,pos,rank,obj1,obj2);
       printf(SPACES);
       printf(FFORMAT,temp1_double,temp2_double,fabs(temp1_double-temp2_double));
      }
      nfound++;
     }
    }
    /* !-d and -p */
    else if (!options.d && options.p)
    {
     if ( temp1_double!=0 && fabs(1-temp2_double/temp1_double) > options.percent )
     {
      if (options.n && nfound>=options.count)
       return nfound;
      if ( options.r==0 ) 
      {
       print_pos(&ph,1,i,acc,pos,rank,obj1,obj2);
       printf(SPACES);
       printf(FPFORMAT,temp1_double,temp2_double,fabs(temp1_double-temp2_double),
        fabs(1-temp2_double/temp1_double));
      }
      nfound++;
     }
    }
    /* -d and -p */
    else if ( options.d && options.p)
    {
     if ( temp1_double!=0 && fabs(1-temp2_double/temp1_double) > options.percent && 
      fabs(temp1_double-temp2_double) > options.delta )
     {
      if (options.n && nfound>=options.count)
       return nfound;
      if ( options.r==0 ) 
      {
       print_pos(&ph,1,i,acc,pos,rank,obj1,obj2);
       printf(SPACES);
       printf(FPFORMAT,temp1_double,temp2_double,fabs(temp1_double-temp2_double),
        fabs(1-temp2_double/temp1_double));
      }
      nfound++;
     }
    }
    else if (temp1_double != temp2_double)
    {
     if (options.n && nfound>=options.count)
      return nfound;
     if ( options.r==0 ) 
     {
      print_pos(&ph,0,i,acc,pos,rank,obj1,obj2);
      printf(SPACES);
      printf(FFORMAT,temp1_double,temp2_double,fabs(temp1_double-temp2_double));
     }
     nfound++;
    }
    
    _buf1+=sizeof(double);
    _buf2+=sizeof(double);
   }/* i */
   
  } /*H5T_NATIVE_DOUBLE*/


  break; /*H5T_FLOAT*/

  
 } /*switch*/
   
 
 return nfound;
}






/*-------------------------------------------------------------------------
 * Function: fixtype
 *
 * Purpose: Given a file data type choose a memory data type which is
 *  appropriate 
 *
 * Return: Memory data type
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: May 9, 2003
 *
 * Comments: Adapted from h5tools_fixtype
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
hid_t fixtype(hid_t f_type)
{
 hid_t   m_type = -1;
 size_t  size;
 
 size = H5Tget_size(f_type);
 
 switch (H5Tget_class(f_type)) 
 {
 default:
  return -1;
 case H5T_INTEGER:
/*
 * Use the smallest native integer type of the same sign as the file
 * such that the memory type is at least as large as the file type.
 * If there is no memory type large enough then use the largest
 * memory type available.
 */
  if (size <= sizeof(char)) 
  {
   m_type = H5Tcopy(H5T_NATIVE_SCHAR);
   #if defined (H5DIFF_DEBUG)
   printf("using memory type H5T_NATIVE_SCHAR\n");
   #endif
  } 
  else if (size <= sizeof(short)) 
  {
   m_type = H5Tcopy(H5T_NATIVE_SHORT);
   #if defined (H5DIFF_DEBUG) 
    printf("using memory type H5T_NATIVE_SHORT\n");  
   #endif
  } 
  else if (size <= sizeof(int)) 
  {
   m_type = H5Tcopy(H5T_NATIVE_INT);
   #if defined (H5DIFF_DEBUG)  
    printf("using memory type H5T_NATIVE_INT\n");
   #endif
  } 
  else if (size <= sizeof(long)) 
  {
   m_type = H5Tcopy(H5T_NATIVE_LONG);
   #if defined (H5DIFF_DEBUG) 
    printf("using memory type H5T_NATIVE_LONG\n");
   #endif
  } 
  else 
  {
   m_type = H5Tcopy(H5T_NATIVE_LLONG);
   #if defined (H5DIFF_DEBUG)
    printf("using memory type H5T_NATIVE_LLONG\n");
   #endif
  }
  
  H5Tset_sign(m_type, H5Tget_sign(f_type));
  break;
  
 case H5T_FLOAT:
/*
 * Use the smallest native floating point type available such that
 * its size is at least as large as the file type.  If there is not
 * native type large enough then use the largest native type.
 */
  if (size <= sizeof(float)) 
  {
   m_type = H5Tcopy(H5T_NATIVE_FLOAT);
   #if defined (H5DIFF_DEBUG)
    printf("using memory type H5T_NATIVE_FLOAT\n");
   #endif
  } 
  else if (size <= sizeof(double)) 
  {
   m_type = H5Tcopy(H5T_NATIVE_DOUBLE);
   #if defined (H5DIFF_DEBUG) 
    printf("using memory type H5T_NATIVE_DOUBLE\n");
   #endif
  } 
  else 
  {
   m_type = H5Tcopy(H5T_NATIVE_LDOUBLE);
   #if defined (H5DIFF_DEBUG)
    printf("using memory type H5T_NATIVE_LDOUBLE\n");
   #endif
  }
  break;
  
 }
  
 return m_type;
}





/*-------------------------------------------------------------------------
 * Function: print_pos
 *
 * Purpose: convert an array index position to matrix notation
 *
 * Return: pos matrix array
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

void print_pos( int *ph, int p, unsigned int curr_pos, int *acc, 
                int *pos, int rank, const char *obj1, const char *obj2 )
{
 int i;

 /* print header */
 if ( *ph==1 )
 {
  *ph=0;
  if (p)
  {
   printf("%-15s %-15s %-15s %-15s %-15s\n", "position", obj1, obj2, "difference", 
    "relative");
   printf("------------------------------------------------------------------------\n");
  }
  else
  {
   printf("%-15s %-15s %-15s %-20s\n", "position", obj1, obj2, "difference");
   printf("------------------------------------------------------------\n");
  }
 }

 for ( i = 0; i < rank; i++)
  pos[i]=0;

 for ( i = 0; i < rank; i++)
 {
  pos[i] = curr_pos/acc[i];
  curr_pos -= acc[i]*pos[i];
 }
 assert( curr_pos == 0 );

 printf("[ " );  
 for ( i = 0; i < rank; i++)
 {
  printf("%d ", pos[i]  );
 }
 printf("]" );
}

/*-------------------------------------------------------------------------
 * Function: print_dims
 *
 * Purpose: print dimensions
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: May 9, 2003
 *
 * Comments: 
 *
 *-------------------------------------------------------------------------
 */
void print_dims( int r, hsize_t *d )
{
 int i;
 printf("[ " );  
 for ( i=0; i<r; i++ ) 
  printf("%d ",(int)d[i]  );
 printf("] " );
}


/*-------------------------------------------------------------------------
 * Function: print_datatype
 *
 * Purpose: Print name of datatype 
 *
 * Return: 
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: May 9, 2003
 *
 * Comments: Adapted from h5dump for H5T_INTEGER and H5T_FLOAT classes only
 *
 *-------------------------------------------------------------------------
 */

void print_datatype(hid_t type)
{
 switch (H5Tget_class(type)) 
 {
 default:
  return;
 case H5T_INTEGER:
  if (H5Tequal(type, H5T_STD_I8BE)) {
   printf("H5T_STD_I8BE");
  } else if (H5Tequal(type, H5T_STD_I8LE)) {
   printf("H5T_STD_I8LE");
  } else if (H5Tequal(type, H5T_STD_I16BE)) {
   printf("H5T_STD_I16BE");
  } else if (H5Tequal(type, H5T_STD_I16LE)) {
   printf("H5T_STD_I16LE");
  } else if (H5Tequal(type, H5T_STD_I32BE)) {
   printf("H5T_STD_I32BE");
  } else if (H5Tequal(type, H5T_STD_I32LE)) {
   printf("H5T_STD_I32LE");
  } else if (H5Tequal(type, H5T_STD_I64BE)) {
   printf("H5T_STD_I64BE");
  } else if (H5Tequal(type, H5T_STD_I64LE)) {
   printf("H5T_STD_I64LE");
  } else if (H5Tequal(type, H5T_STD_U8BE)) {
   printf("H5T_STD_U8BE");
  } else if (H5Tequal(type, H5T_STD_U8LE)) {
   printf("H5T_STD_U8LE");
  } else if (H5Tequal(type, H5T_STD_U16BE)) {
   printf("H5T_STD_U16BE");
  } else if (H5Tequal(type, H5T_STD_U16LE)) {
   printf("H5T_STD_U16LE");
  } else if (H5Tequal(type, H5T_STD_U32BE)) {
   printf("H5T_STD_U32BE");
  } else if (H5Tequal(type, H5T_STD_U32LE)) {
   printf("H5T_STD_U32LE");
  } else if (H5Tequal(type, H5T_STD_U64BE)) {
   printf("H5T_STD_U64BE");
  } else if (H5Tequal(type, H5T_STD_U64LE)) {
   printf("H5T_STD_U64LE");
  } else if (H5Tequal(type, H5T_NATIVE_SCHAR)) {
   printf("H5T_NATIVE_SCHAR");
  } else if (H5Tequal(type, H5T_NATIVE_UCHAR)) {
   printf("H5T_NATIVE_UCHAR");
  } else if (H5Tequal(type, H5T_NATIVE_SHORT)) {
   printf("H5T_NATIVE_SHORT");
  } else if (H5Tequal(type, H5T_NATIVE_USHORT)) {
   printf("H5T_NATIVE_USHORT");
  } else if (H5Tequal(type, H5T_NATIVE_INT)) {
   printf("H5T_NATIVE_INT");
  } else if (H5Tequal(type, H5T_NATIVE_UINT)) {
   printf("H5T_NATIVE_UINT");
  } else if (H5Tequal(type, H5T_NATIVE_LONG)) {
   printf("H5T_NATIVE_LONG");
  } else if (H5Tequal(type, H5T_NATIVE_ULONG)) {
   printf("H5T_NATIVE_ULONG");
  } else if (H5Tequal(type, H5T_NATIVE_LLONG)) {
   printf("H5T_NATIVE_LLONG");
  } else if (H5Tequal(type, H5T_NATIVE_ULLONG)) {
   printf("H5T_NATIVE_ULLONG");
  } else {
   printf("undefined integer");
  }
  break;
  
 case H5T_FLOAT:
  if (H5Tequal(type, H5T_IEEE_F32BE)) {
   printf("H5T_IEEE_F32BE");
  } else if (H5Tequal(type, H5T_IEEE_F32LE)) {
   printf("H5T_IEEE_F32LE");
  } else if (H5Tequal(type, H5T_IEEE_F64BE)) {
   printf("H5T_IEEE_F64BE");
  } else if (H5Tequal(type, H5T_IEEE_F64LE)) {
   printf("H5T_IEEE_F64LE");
  } else if (H5Tequal(type, H5T_NATIVE_FLOAT)) {
   printf("H5T_NATIVE_FLOAT");
  } else if (H5Tequal(type, H5T_NATIVE_DOUBLE)) {
   printf("H5T_NATIVE_DOUBLE");
  } else if (H5Tequal(type, H5T_NATIVE_LDOUBLE)) {
   printf("H5T_NATIVE_LDOUBLE");
  } else {
   printf("undefined float");
  }
  break;
   
 }/*switch*/
}



/*-------------------------------------------------------------------------
 * Function: basename
 *
 * Purpose: Returns a pointer to the last component absolute name 
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: May 9, 2003
 *
 * Comments: 
 *
 *-------------------------------------------------------------------------
 */

const char*
basename(const char *name)
{
 size_t i;
 
 /* Find the end of the base name */
 i = strlen(name);
 while (i>0 && '/'==name[i-1])
  --i;
 
 /* Skip backward over base name */
 while (i>0 && '/'!=name[i-1])
  --i;
 
 return(name+i);
}

/*-------------------------------------------------------------------------
 * Function: get_type
 *
 * Purpose: Returns the type as a string
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: May 9, 2003
 *
 * Comments: 
 *
 *-------------------------------------------------------------------------
 */

const char*
get_type(int type)
{
 switch (type)
 {
 case H5G_DATASET:
  return("H5G_DATASET");
 case H5G_GROUP:
  return("H5G_GROUP");
 case H5G_TYPE:
  return("H5G_TYPE");
 case H5G_LINK:
  return("H5G_LINK");
 default:
  return("user defined type");
 } 
}

/*-------------------------------------------------------------------------
 * Function: get_class
 *
 * Purpose: Returns the class as a string
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: May 9, 2003
 *
 * Comments: 
 *
 *-------------------------------------------------------------------------
 */

const char*
get_class(H5T_class_t tclass)
{
 switch (tclass) 
 {
 default:
  return("Invalid class");
 case H5T_TIME:
  return("H5T_TIME");
 case H5T_INTEGER:
  return("H5T_INTEGER");
 case H5T_FLOAT:
  return("H5T_FLOAT");
 case H5T_STRING:
  return("H5T_STRING");
 case H5T_BITFIELD:
  return("H5T_BITFIELD");
 case H5T_OPAQUE:
  return("H5T_OPAQUE");
 case H5T_COMPOUND:
  return("H5T_COMPOUND");
 case H5T_REFERENCE:
  return("H5T_REFERENCE");
 case H5T_ENUM:
  return("H5T_ENUM");
 case H5T_VLEN:
  return("H5T_VLEN");
 case H5T_ARRAY:
  return("H5T_ARRAY");
 }
}


/*-------------------------------------------------------------------------
 * Function: print_sizes
 *
 * Purpose: Print datatype sizes
 *
 *-------------------------------------------------------------------------
 */

void print_sizes( const char *obj1, const char *obj2,
                  hid_t f_type1, hid_t f_type2,
                  hid_t m_type1, hid_t m_type2 )      
{
 size_t  f_size1, f_size2;       /* size of type in file */
 size_t  m_size1, m_size2;       /* size of type in memory */

 f_size1 = H5Tget_size( f_type1 );
 f_size2 = H5Tget_size( f_type2 );
 m_size1 = H5Tget_size( m_type1 );
 m_size2 = H5Tget_size( m_type2 );

 printf("\n");
 printf("------------------\n");
 printf("sizeof(char)   %u\n", sizeof(char) );
 printf("sizeof(short)  %u\n", sizeof(short) );
 printf("sizeof(int)    %u\n", sizeof(int) );
 printf("sizeof(long)   %u\n", sizeof(long) );
 printf("<%s> ------------------\n", obj1);
 printf("type on file   ");
 print_datatype(f_type1);
 printf("\n");
 printf("size on file   %u\n", f_size1 );

 printf("type on memory ");
 print_datatype(m_type1);
 printf("\n");
 printf("size on memory %u\n", m_size1 );

 printf("<%s> ------------------\n", obj2);
 printf("type on file   ");
 print_datatype(f_type2);
 printf("\n");
 printf("size on file   %u\n", f_size2 );

 printf("type on memory ");
 print_datatype(m_type2);
 printf("\n");
 printf("size on memory %u\n", m_size2 );
 printf("\n");
}


