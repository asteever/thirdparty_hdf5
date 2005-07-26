/****************************************************************************
 * NCSA HDF                                                                 *
 * Scientific Data Technologies                                             *
 * National Center for Supercomputing Applications                          *
 * University of Illinois at Urbana-Champaign                               *
 * 605 E. Springfield, Champaign IL 61820                                   *
 *                                                                          *
 * For conditions of distribution and use, see the accompanying             *
 * hdf/COPYING f.                                                           *
 *                                                                          *
 ****************************************************************************/


#include "H5TB.h"

#include <stdlib.h>


/*-------------------------------------------------------------------------
 * Table API example
 *
 * H5TBmake_table 
 * H5TBread_table
 *
 *-------------------------------------------------------------------------
 */

#define NFIELDS  5
#define NRECORDS 8

int main( void )
{
 
 typedef struct Particle 
 {
  char   name[16];
  int    lati;
  int    longi;
  float  pressure;
  double temperature; 
 } Particle;

 Particle  dst_buf[NRECORDS];
 
 /* Calculate the size and the offsets of our struct members in memory */
 size_t dst_size =  sizeof( Particle );
 size_t dst_offset[NFIELDS] = { HOFFSET( Particle, name ),
                                HOFFSET( Particle, lati ),
                                HOFFSET( Particle, longi ),
                                HOFFSET( Particle, pressure ),
                                HOFFSET( Particle, temperature )};

 size_t dst_sizes[NFIELDS] = { sizeof( dst_buf[0].name),
                               sizeof( dst_buf[0].lati),
                               sizeof( dst_buf[0].longi),
                               sizeof( dst_buf[0].pressure),
                               sizeof( dst_buf[0].temperature)};
  
 
 /* Define an array of Particles */
	Particle  p_data[NRECORDS] = { {"zero",0,0, 0.0f, 0.0},
	{"one",10,10, 1.0f, 10.0},
	{"two",  20,20, 2.0f, 20.0},
	{"three",30,30, 3.0f, 30.0},
	{"four", 40,40, 4.0f, 40.0},
	{"five", 50,50, 5.0f, 50.0},
	{"six",  60,60, 6.0f, 60.0},
	{"seven",70,70, 7.0f, 70.0}
  };

  /* Define field information */
  const char *field_names[NFIELDS]  = 
		{ "Name","Latitude", "Longitude", "Pressure", "Temperature" };
  hid_t      field_type[NFIELDS];
  hid_t      string_type;
  hid_t      file_id;
  hsize_t    chunk_size = 10;
  int        *fill_data = NULL;
  int        compress  = 0;
  herr_t     status; 

		EXAMPLE("make a table");


  /* Initialize the field field_type */
  string_type = H5Tcopy( H5T_C_S1 );
  H5Tset_size( string_type, 16 );
  field_type[0] = string_type;
  field_type[1] = H5T_NATIVE_INT;
  field_type[2] = H5T_NATIVE_INT;
  field_type[3] = H5T_NATIVE_FLOAT;
  field_type[4] = H5T_NATIVE_DOUBLE;
  
  /* Create a new file using default properties. */
  file_id = H5Fcreate( "ex_table_01.h5", H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT );


/*-------------------------------------------------------------------------
 * H5TBmake_table 
 *-------------------------------------------------------------------------
 */

 status=H5TBmake_table( "Table Title", file_id, "Table1",(hsize_t) NFIELDS, (hsize_t)NRECORDS, dst_size, 
                       field_names, dst_offset, field_type, 
                       chunk_size, fill_data, compress, p_data  );
 

/*-------------------------------------------------------------------------
 * H5TBread_table 
 *-------------------------------------------------------------------------
 */

 status=H5TBread_table( file_id, "Table1", dst_size, dst_offset, dst_sizes, dst_buf );
   
/*-------------------------------------------------------------------------
 * end
 *-------------------------------------------------------------------------
 */
 
 /* Close the file. */
 H5Fclose( file_id );

	PASSED();

 return 0;

}


