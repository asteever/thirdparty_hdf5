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

#include <string>
#ifdef OLD_HEADER_FILENAME
#include <iostream.h>
#else
#include <iostream>
#endif

#include "H5Include.h"
#include "H5RefCounter.h"
#include "H5Exception.h"
#include "H5IdComponent.h"
#include "H5Idtemplates.h"
#include "H5PropList.h"
#include "H5Object.h"
#include "H5DataType.h"
#include "H5AtomType.h"
#include "H5PredType.h"
#include "H5private.h"

#ifndef H5_NO_NAMESPACE
namespace H5 {
#endif

//--------------------------------------------------------------------------
// Function:	DataType overloaded constructor
///\brief	Creates a datatype using an existing datatype's id
///\param	existing_id - IN: Id of the existing datatype
///\param	predefined  - IN: Indicates whether or not this datatype is
///		a predefined datatype; default to \c false
// Description
//		Constructor creates a copy of an existing DataType using 
//		its id.  The argument "predefined" is default to false; 
//		when a default datatype is created, this argument is set 
//		to true so H5Tclose will not be called on it later. - need 
//		a reassessment after changing to the new ref counting mech. 
//		- BMR 5/2004
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
DataType::DataType(const hid_t existing_id, bool predefined) : H5Object(existing_id), is_predtype(predefined) {}

//--------------------------------------------------------------------------
// Function:	DataType overloaded constructor
///\brief	Creates a object given its class and size
///\param	type_class - IN: Class of datatype to create
///\param	size       - IN: Number of bytes in the datatype to create
///\exception   H5::DataTypeIException
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
DataType::DataType( const H5T_class_t type_class, size_t size ) : H5Object(), is_predtype( false )
{
   // Call C routine to create the new datatype
   id = H5Tcreate( type_class, size );
   if( id <= 0 )
   {
      throw DataTypeIException("DataType constructor", "H5Tcreate failed");
   }
}

//--------------------------------------------------------------------------
// Function:	DataType default constructor
///\brief	Default constructor: Creates a stub datatype
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
DataType::DataType() : H5Object(), is_predtype( false ) {}

//--------------------------------------------------------------------------
// Function:	DataType copy constructor
///\brief	Copy constructor: makes a copy of the original DataType object.
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
DataType::DataType(const DataType& original) : H5Object(original)
{
   is_predtype = original.is_predtype; // copy data member from original
}

//--------------------------------------------------------------------------
// Function:	DataType::copy
///\brief	Copies an existing datatype to this datatype object
///\param	like_type - IN: Datatype to be copied
///\exception   H5::DataTypeIException
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
void DataType::copy( const DataType& like_type )
{
   // reset the identifier of this instance, H5Tclose will be called 
   // if needed 
    try {
        resetIdComponent( this ); }
    catch (Exception close_error) { // thrown by p_close
        throw DataTypeIException("DataType::copy", close_error.getDetailMsg());
    }

   // call C routine to copy the datatype
   id = H5Tcopy( like_type.getId() );

   // new reference counter for this id
   ref_count = new RefCounter;

   if( id <= 0 )
   {
      throw DataTypeIException("DataType::copy", "H5Tcopy failed");
   }
}

//--------------------------------------------------------------------------
// Function:	DataType::operator=
///\brief	Assignment operator
///\param	rhs - IN: Reference to the existing datatype
///\exception   H5::DataTypeIException
///
// Description
// 		Makes a copy of the type on the right hand side and stores 
//		the new id in the left hand side object.  
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
DataType& DataType::operator=( const DataType& rhs )
{
   copy(rhs);
   return(*this);
}

//--------------------------------------------------------------------------
// Function:	DataType::operator==
///\brief	Compares this DataType against the given one to determines 
///		whether the two objects refer to the same actual datatype.
///\param	compared_type - IN: Reference to the datatype to compare
///\return	true if the datatypes are equal, and false, otherwise.
///\exception	H5::DataTypeIException
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
bool DataType::operator==(const DataType& compared_type ) const
{
   // Call C routine H5Tequal to determines whether two datatype 
   // identifiers refer to the same datatype
   htri_t ret_value = H5Tequal( id, compared_type.getId() );
   if( ret_value > 0 )
      return true;
   else if( ret_value == 0 )
      return false;
   else
   {
      throw DataTypeIException("DataType::operator==", 
		"H5Tequal returns negative value");
   }
}

//--------------------------------------------------------------------------
// Function:	DataType::commit
///\brief	Commits a transient datatype to a file, creating a new 
///		named datatype
///\param	loc - IN: Either a file or a group
///\param	name - IN: Name of the datatype
///\exception   H5::DataTypeIException
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
void DataType::commit( H5Object& loc, const char* name ) const
{
   hid_t loc_id = loc.getId(); // get location id for C API

   // Call C routine to commit the transient datatype 
   herr_t ret_value = H5Tcommit( loc_id, name, id );
   if( ret_value < 0 )
   {
      throw DataTypeIException("DataType::commit", "H5Tcommit failed");
   }
}

//--------------------------------------------------------------------------
// Function:	DataType::commit
///\brief	This is an overloaded member function, provided for convenience.
///		It differs from the above function only in the type of the 
///		argument \a name.
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
void DataType::commit( H5Object& loc, const string& name ) const
{
   commit( loc, name.c_str() );
}

//--------------------------------------------------------------------------
// Function:	DataType::committed
///\brief	Determines whether a datatype is a named type or a 
///		transient type. 
///\return	true if the datatype is a named type, and false, otherwise.
///\exception   H5::DataTypeIException
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
bool DataType::committed() const
{
   // Call C function to determine if a datatype is a named one
   htri_t committed = H5Tcommitted( id );
   if( committed > 0 )
      return true;
   else if( committed == 0 )
      return false;
   else
   {
      throw DataTypeIException("DataType::committed", "H5Tcommitted return negative value");
   }
}

//--------------------------------------------------------------------------
// Function:	DataType::find
///\brief	Finds a conversion function that can handle a conversion 
///		from this datatype to the specified datatype, \a dest.
///\param	dest   - IN: Destination datatype
///\param	pcdata - IN: Pointer to type conversion data
///\return	Pointer to a suitable conversion function 
///\exception   H5::DataTypeIException
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
H5T_conv_t DataType::find( const DataType& dest, H5T_cdata_t **pcdata ) const
{
   // Call C routine to find the conversion function
   H5T_conv_t func = H5Tfind( id, dest.getId(), pcdata );
   if( func == NULL )
   {
      throw DataTypeIException("DataType::find", "H5Tfind returns a NULL function");
   }
   return( func );
}

//--------------------------------------------------------------------------
// Function:	DataType::convert
///\brief	Converts data from this datatype to the specified datatypes. 
///\param	dest       - IN: Destination datatype
///\param	nelmts     - IN: Size of array \a buf
///\param	buf        - IN/OUT: Array containing pre- and post-conversion 
///				values
///\param	background - IN: Optional backgroud buffer
///\param	plist      - IN: Dataset transfer property list
///\return	Pointer to a suitable conversion function 
///\exception   H5::DataTypeIException
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
void DataType::convert( const DataType& dest, hsize_t nelmts, void *buf, void *background, PropList& plist ) const
{
   // Get identifiers for C API
   hid_t dest_id = dest.getId();
   hid_t plist_id = plist.getId();

   // Call C routine H5Tconvert to convert the data
   herr_t ret_value;
   ret_value = H5Tconvert( id, dest_id, nelmts, buf, background, plist_id );
   if( ret_value < 0 )
   {
      throw DataTypeIException("DataType::convert", "H5Tconvert failed");
   }
}

//--------------------------------------------------------------------------
// Function:	DataType::lock
///\brief	Locks a datatype, making it read-only and non-destructible.
///\exception   H5::DataTypeIException
///\par Descrition
///		This is normally done by the library for predefined data 
///		types so the application doesn't inadvertently change or 
///		delete a predefined type.
///
///		Once a data type is locked it can never be unlocked unless
///		the entire library is closed.
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
void DataType::lock() const
{
   // Call C routine to lock the datatype
   herr_t ret_value = H5Tlock( id );
   if( ret_value < 0 )
   {
      throw DataTypeIException("DataType::lock", "H5Tlock failed");
   }
}

//--------------------------------------------------------------------------
// Function:	DataType::getClass
///\brief	Returns the datatype class identifier. 
///\return	Datatype class identifier
///\exception   H5::DataTypeIException
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
H5T_class_t DataType::getClass() const
{
   H5T_class_t type_class = H5Tget_class( id );

   // Return datatype class identifier if successful
   if( type_class == H5T_NO_CLASS )
   {
      throw DataTypeIException("DataType::getClass", 
		"H5Tget_class returns H5T_NO_CLASS");
   }
   return( type_class );
}

//--------------------------------------------------------------------------
// Function:	DataType::getSize
///\brief	Returns the size of a datatype. 
///\return	Datatype size in bytes
///\exception   H5::DataTypeIException
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
size_t DataType::getSize() const
{
   // Call C routine to get the datatype size
   size_t type_size = H5Tget_size( id );
   if( type_size <= 0 ) // valid data types are never zero size
   {
      throw DataTypeIException("DataType::getSize", 
		"H5Tget_size returns invalid datatype size");
   }
   return( type_size );
}

//--------------------------------------------------------------------------
// Function:	DataType::getSuper
///\brief	Returns the base datatype from which a datatype is derived. 
///\return	DataType object
///\exception   H5::DataTypeIException
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
DataType DataType::getSuper() const
{
   // Call C routine to get the base datatype from which the specified
   // datatype is derived. 
   hid_t base_type_id = H5Tget_super( id );

   // If H5Tget_super returns a valid datatype id, create and return
   // the base type, otherwise, raise exception
   if( base_type_id > 0 )
   {
      DataType base_type( base_type_id );
      return( base_type );
   }
   else
   {
      throw DataTypeIException("DataType::getSuper", "H5Tget_super failed");
   }
}

//--------------------------------------------------------------------------
// Function:	DataType::registerFunc
///\brief	Registers the specified conversion function. 
///\param	pers - IN: Conversion option
///			\li \c H5T_PERS_HARD for hard conversion functions
///			\li \c H5T_PERS_SOFT for soft conversion functions. 
///\param	name - IN: Name displayed in diagnostic output. 
///\param	dest - IN: Destination datatype.
///\param	func - IN: Function to convert between source and 
///		destination datatypes. 
///\exception   H5::DataTypeIException
///\par Description
///		For more information, please see:
/// http://hdf.ncsa.uiuc.edu/HDF5/doc/RM_H5T.html#Datatype-Register
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
void DataType::registerFunc( H5T_pers_t pers, const char* name, const DataType& dest, H5T_conv_t func ) const
{
   hid_t dest_id = dest.getId();  // get id of the destination datatype

   // Call C routine H5Tregister to register the conversion function
   herr_t ret_value = H5Tregister( pers, name, id, dest_id, func );
   if( ret_value < 0 )
   {
      throw DataTypeIException("DataType::registerFunc", "H5Tregister failed");
   }
}

//--------------------------------------------------------------------------
// Function:	DataType::registerFunc
///\brief	This is an overloaded member function, provided for convenience.
///		It differs from the above function only in the type of the 
///		argument \a name.
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
void DataType::registerFunc( H5T_pers_t pers, const string& name, const DataType& dest, H5T_conv_t func ) const
{
   registerFunc( pers, name.c_str(), dest, func );
}

//--------------------------------------------------------------------------
// Function:	DataType::unregister
///\brief	Removes a conversion function from all conversion paths. 
///\param	pers - IN: Conversion option
///			\li \c H5T_PERS_HARD for hard conversion functions
///			\li \c H5T_PERS_SOFT for soft conversion functions. 
///\param	name - IN: Name displayed in diagnostic output. 
///\param	dest - IN: Destination datatype.
///\param	func - IN: Function to convert between source and 
///		destination datatypes. 
///\exception   H5::DataTypeIException
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
void DataType::unregister( H5T_pers_t pers, const char* name, const DataType& dest, H5T_conv_t func ) const
{
   hid_t dest_id = dest.getId();  // get id of the dest datatype for C API

   // Call C routine H5Tunregister to remove the conversion function 
   herr_t ret_value = H5Tunregister( pers, name, id, dest_id, func );
   if( ret_value < 0 )
   {
      throw DataTypeIException("DataType::unregister", "H5Tunregister failed");
   }
}

//--------------------------------------------------------------------------
// Function:	DataType::unregister
///\brief	This is an overloaded member function, provided for convenience.
///		It differs from the above function only in the type of the 
///		argument \a name.
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
void DataType::unregister( H5T_pers_t pers, const string& name, const DataType& dest, H5T_conv_t func ) const
{
   unregister( pers, name.c_str(), dest, func );
}

//--------------------------------------------------------------------------
// Function:	DataType::setTag
///\brief	Tags an opaque datatype. 
///\param	tag - IN: Descriptive ASCII string with which the opaque 
///		datatype is to be tagged. 
///\exception   H5::DataTypeIException
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
void DataType::setTag( const char* tag ) const
{
   // Call C routine H5Tset_tag to tag an opaque datatype. 
   herr_t ret_value = H5Tset_tag( id, tag );
   if( ret_value < 0 )
   {
      throw DataTypeIException("DataType::setTag", "H5Tset_tag failed");
   }
}

//--------------------------------------------------------------------------
// Function:	DataType::setTag
///\brief	This is an overloaded member function, provided for convenience.
///		It differs from the above function only in the type of the 
///		argument \a name.
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
void DataType::setTag( const string& tag ) const
{
   setTag( tag.c_str());
}

//--------------------------------------------------------------------------
// Function:	DataType::setTag
///\brief	Gets the tag associated with an opaque datatype. 
///\return	Tag associated with the opaque datatype
///\exception   H5::DataTypeIException
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
string DataType::getTag() const
{
   char* tag_Cstr = H5Tget_tag( id );

   // if the tag C-string returned is not NULL, convert it to C++ string
   // and return it, otherwise, raise an exception
   if( tag_Cstr != NULL )
   {
      string tag = string(tag_Cstr); // convert C string to string object
	HDfree(tag_Cstr); // free the C string 
      return (tag); // return the tag 
   }
   else
   {
      throw DataTypeIException("DataType::getTag", 
		"H5Tget_tag returns NULL for tag");
   }
}

//--------------------------------------------------------------------------
// Function:	DataType::detectClass
///\brief	Checks whether a datatype contains (or is) a certain type of
///		datatype.
///\return	true if this datatype contains or is the specified type, 
///		and false, otherwise.
///\exception   H5::DataTypeIException
// Programmer	Binh-Minh Ribler - May, 2004
//--------------------------------------------------------------------------
bool DataType::detectClass(H5T_class_t cls) const
{
   htri_t ret_value = H5Tdetect_class(id, cls);
   if( ret_value > 0 )
      return true;
   else if( ret_value == 0 )
      return false;
   else
   {
      throw DataTypeIException("DataType::detectClass", 
		"H5Tdetect_class returns negative value");
   }
}

//--------------------------------------------------------------------------
// This private function calls the C API H5Tclose to close this datatype.
// Used by H5Object::p_reset.
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
void DataType::p_close() const
{
   // If this datatype is not a predefined type, call H5Tclose on it.
   if( is_predtype == false )
   {
      herr_t ret_value = H5Tclose( id );
      if( ret_value < 0 )
      {
         throw DataTypeIException(0, "H5Tclose failed");
      }
   }
}

//--------------------------------------------------------------------------
// Function:    DataType destructor
///\brief       Properly terminates access to this datatype.
// Programmer	Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
DataType::~DataType()
{  
   // The datatype id will be closed properly
    try {
        resetIdComponent( this ); }
    catch (Exception close_error) { // thrown by p_close
        cerr << "DataType::~DataType - " << close_error.getDetailMsg() << endl;
    }
}  

#ifndef H5_NO_NAMESPACE
} // end namespace
#endif
