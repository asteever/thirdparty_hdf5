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

/*****************************************************************************
   FILE
   tfile.cpp - HDF5 C++ testing the file I/O features

   EXTERNAL ROUTINES/VARIABLES:
     These routines are in the test directory of the C library:
        h5_fileaccess() -- in h5test.c, returns a file access template

 ***************************************************************************/

#ifdef OLD_HEADER_FILENAME
#include <iostream.h>
#else
#include <iostream>
#endif
#include <string>

#ifndef H5_NO_NAMESPACE
#ifndef H5_NO_STD
    using std::cerr;
    using std::endl;
#endif  // H5_NO_STD
#endif

#include "H5Cpp.h"	// C++ API header file

#ifndef H5_NO_NAMESPACE
    using namespace H5;
#endif

#include "h5cpputil.h"	// C++ utilility header file

const hsize_t F1_USERBLOCK_SIZE = (hsize_t)0;
const size_t F1_OFFSET_SIZE = sizeof(haddr_t);
const size_t F1_LENGTH_SIZE = sizeof(hsize_t);
const unsigned F1_SYM_LEAF_K  = 4;
const unsigned F1_SYM_INTERN_K = 16;
const H5std_string    FILE1("tfile1.h5");

const hsize_t F2_USERBLOCK_SIZE = (hsize_t)512;
const size_t F2_OFFSET_SIZE = 8;
const size_t F2_LENGTH_SIZE = 8;
const unsigned F2_SYM_LEAF_K  = 8;
const unsigned F2_SYM_INTERN_K = 32;
const H5std_string    FILE2("tfile2.h5");

const hsize_t F3_USERBLOCK_SIZE = (hsize_t)0;
const size_t F3_OFFSET_SIZE = F2_OFFSET_SIZE;
const size_t F3_LENGTH_SIZE = F2_LENGTH_SIZE;
const unsigned F3_SYM_LEAF_K  = F2_SYM_LEAF_K;
const unsigned F3_SYM_INTERN_K = F2_SYM_INTERN_K;
const H5std_string    FILE3("tfile3.h5");

const int KB =  1024;
const H5std_string    FILE4("tfile4.h5");


/*-------------------------------------------------------------------------
 * Function:    test_file_create
 *
 * Purpose:     Test file and template creations
 *
 * Return:      None
 *
 * Programmer:  Binh-Minh Ribler (use C version)
 *              January, 2001
 *
 * Modifications:
 *	January, 2005: C tests' macro VERIFY casts values to 'long' for all
 *		       cases.  Since there are no operator<< for 'long long'
 *		       or int64 in VS C++ ostream, I casted the hsize_t values
 *		       passed to verify_val to 'long' as well.  If problems
 *		       arises later, this will have to be specificly handled
 *		       with a special routine.
 *
 *-------------------------------------------------------------------------
 */
static void test_file_create()
{
    // Output message about test being performed
    SUBTEST("File Creation I/O");

    // Test create with various sequences of H5F_ACC_EXCL and
    // H5F_ACC_TRUNC flags

    // Create with H5F_ACC_EXCL
    // First ensure the file does not exist
    remove(FILE1.c_str());

    // Setting this to NULL for cleaning up in failure situations
    H5File* file1 = NULL;
    try {
	// Create file FILE1
	file1 = new H5File (FILE1, H5F_ACC_EXCL);

	// Try to create the same file with H5F_ACC_TRUNC. This should fail
	// because file1 is the same file and is currently open. Skip it on
	// OpenVMS because it creates another version of the file.
#ifndef H5_HAVE_FILE_VERSIONS
	try {
	    H5File file2 (FILE1, H5F_ACC_TRUNC);  // should throw E

	    // Should FAIL but didn't, so throw an invalid action exception
	    throw InvalidActionException("H5File constructor", "Attempted to create an existing file.");
	}
	catch( FileIException E ) // catch truncating existing file
	{} // do nothing, FAIL expected
#endif // H5_HAVE_FILE_VERSIONS

	// Close file1
	delete file1;
	file1 = NULL;

	// Try again with H5F_ACC_EXCL. This should fail because the file
	// already exists from the previous steps.
	try {
	    H5File file2(FILE1, H5F_ACC_EXCL);  // should throw E

	    // Should FAIL but didn't, so throw an invalid action exception
	    throw InvalidActionException("H5File constructor", "File already exists.");
	}
	catch( FileIException E ) // catching creating existing file
	{} // do nothing, FAIL expected

    	// Test create with H5F_ACC_TRUNC. This will truncate the existing file.
	file1 = new H5File (FILE1, H5F_ACC_TRUNC);

	// Try to create first file again. This should fail because file1
	// is the same file and is currently open. Skip it on OpenVMS because
	// it creates another version of the file.
#ifndef H5_HAVE_FILE_VERSIONS
    	try {
	    H5File file2 (FILE1, H5F_ACC_TRUNC);   // should throw E

	    // Should FAIL but didn't, so throw an invalid action exception
	    throw InvalidActionException("H5File constructor", "H5F_ACC_TRUNC attempt on an opened file.");
	}
	catch( FileIException E ) // catching truncating opened file
	{} // do nothing, FAIL expected

     	// Try with H5F_ACC_EXCL. This should fail too because the file already
     	// exists. Skip it on OpenVMS because it creates another version of the file.
    	try {
	    H5File file3 (FILE1, H5F_ACC_EXCL);  // should throw E

	    // Should FAIL but didn't, so throw an invalid action exception
	    throw InvalidActionException("H5File constructor", "H5F_ACC_EXCL attempt on an existing file.");
    	}
	catch( FileIException E ) // catching H5F_ACC_EXCL on existing file
	{} // do nothing, FAIL expected
#endif /*H5_HAVE_FILE_VERSIONS*/

    	// Get the file-creation template
	FileCreatPropList tmpl1 = file1->getCreatePlist();

	hsize_t ublock = tmpl1.getUserblock();
	verify_val((long)ublock, (long)F1_USERBLOCK_SIZE, "FileCreatPropList::getUserblock", __LINE__, __FILE__);

    	size_t  parm1, parm2;		// file-creation parameters
	tmpl1.getSizes( parm1, parm2);
	verify_val(parm1, F1_OFFSET_SIZE, "FileCreatPropList::getSizes", __LINE__, __FILE__);
	verify_val(parm2, F1_LENGTH_SIZE, "FileCreatPropList::getSizes", __LINE__, __FILE__);

        unsigned  iparm1,iparm2;        // file-creation parameters
        tmpl1.getSymk( iparm1, iparm2);
        verify_val(iparm1, F1_SYM_INTERN_K, "FileCreatPropList::getSymk", __LINE__, __FILE__);
        verify_val(iparm2, F1_SYM_LEAF_K, "FileCreatPropList::getSymk", __LINE__, __FILE__);

	// tmpl1 is automatically closed; if error occurs, it'll be
	// caught in the catch block

	// Close first file
	delete file1;
    }
    catch (InvalidActionException E)
    {
        cerr << " *FAILED*" << endl;
        cerr << "    <<<  " << E.getDetailMsg() << "  >>>" << endl << endl;
        if (file1 != NULL) // clean up
            delete file1;
    }
    // catch all other exceptions
    catch (Exception E)
    {
	issue_fail_msg("test_file_create()", __LINE__, __FILE__, E.getCDetailMsg());
        if (file1 != NULL) // clean up
            delete file1;
    }

    // Setting this to NULL for cleaning up in failure situations
    FileCreatPropList* tmpl1 = NULL;
    try
    {
    	// Create a new file with a non-standard file-creation template
	tmpl1 = new FileCreatPropList;

    	// Set the new file-creation parameters
	tmpl1->setUserblock (F2_USERBLOCK_SIZE);
	tmpl1->setSizes( F2_OFFSET_SIZE, F2_LENGTH_SIZE );
	tmpl1->setSymk( F2_SYM_INTERN_K, F2_SYM_LEAF_K );

     	// Try to create second file, with non-standard file-creation template
     	// params.
	H5File file2( FILE2, H5F_ACC_TRUNC, *tmpl1 );

    	// Release file-creation template
	delete tmpl1;
	tmpl1 = NULL;

	// Get the file-creation template
	tmpl1 = new FileCreatPropList (file2.getCreatePlist());

	// Get the file-creation parameters
	hsize_t ublock = tmpl1->getUserblock();
	verify_val((long)ublock, (long)F2_USERBLOCK_SIZE, "FileCreatPropList::getUserblock", __LINE__, __FILE__);

    	size_t  parm1, parm2;		// file-creation parameters
	tmpl1->getSizes( parm1, parm2);
	verify_val(parm1, F2_OFFSET_SIZE, "FileCreatPropList::getSizes", __LINE__, __FILE__);
	verify_val(parm2, F2_LENGTH_SIZE, "FileCreatPropList::getSizes", __LINE__, __FILE__);

        unsigned  iparm1,iparm2;	// file-creation parameters
        tmpl1->getSymk( iparm1, iparm2);
        verify_val(iparm1, F2_SYM_INTERN_K, "FileCreatPropList::getSymk", __LINE__, __FILE__);
        verify_val(iparm2, F2_SYM_LEAF_K, "FileCreatPropList::getSymk", __LINE__, __FILE__);

	// Clone the file-creation template
	FileCreatPropList tmpl2;
	tmpl2.copy (*tmpl1);

	// Release file-creation template
	delete tmpl1;
	tmpl1 = NULL;

	// Set the new file-creation parameter
	tmpl2.setUserblock( F3_USERBLOCK_SIZE );

	// Try to create second file, with non-standard file-creation template
	// params
	H5File file3( FILE3, H5F_ACC_TRUNC, tmpl2 );

	// Get the file-creation template
	tmpl1 = new FileCreatPropList (file3.getCreatePlist());

	// Get the file-creation parameters
	ublock = tmpl1->getUserblock();
	verify_val((long)ublock, (long)F3_USERBLOCK_SIZE, "FileCreatPropList::getUserblock", __LINE__, __FILE__);

	tmpl1->getSizes( parm1, parm2);
	verify_val(parm1, F3_OFFSET_SIZE, "FileCreatPropList::getSizes", __LINE__, __FILE__);
	verify_val(parm2, F3_LENGTH_SIZE, "FileCreatPropList::getSizes", __LINE__, __FILE__);

    	tmpl1->getSymk( iparm1, iparm2);
	verify_val(iparm1, F3_SYM_INTERN_K, "FileCreatPropList::getSymk", __LINE__, __FILE__);
	verify_val(iparm2, F3_SYM_LEAF_K, "FileCreatPropList::getSymk", __LINE__, __FILE__);

	// Release file-creation template
	delete tmpl1;
	PASSED();
    }
    // catch all exceptions
    catch (Exception E)
    {
	issue_fail_msg("test_file_create()", __LINE__, __FILE__, E.getCDetailMsg());
	if (tmpl1 != NULL)  // clean up
	    delete tmpl1;
    }
}   // test_file_create()


/*-------------------------------------------------------------------------
 * Function:    test_file_open
 *
 * Purpose:     Test file accesses
 *
 * Return:      None
 *
 * Programmer:  Binh-Minh Ribler (use C version)
 *              January, 2001
 *
 * Modifications:
 *	January, 2005: C tests' macro VERIFY casts values to 'long' for all
 *		       cases.  Since there are no operator<< for 'long long'
 *		       or int64 in VS C++ ostream, I casted the hsize_t values
 *		       passed to verify_val to 'long' as well.  If problems
 *		       arises later, this will have to be specificly handled
 *		       with a special routine.
 *
 *-------------------------------------------------------------------------
 */
static void test_file_open()
{
    // Output message about test being performed
    SUBTEST("File Opening I/O");

    try {

	// Open first file
	H5File file1 (FILE2, H5F_ACC_RDWR );

	// Get the file-creation template
	FileCreatPropList tmpl1 = file1.getCreatePlist();

	// Get the file-creation parameters
	hsize_t ublock = tmpl1.getUserblock();
	verify_val((long)ublock, (long)F2_USERBLOCK_SIZE, "FileCreatPropList::getUserblock", __LINE__, __FILE__);

    	size_t  parm1, parm2;		// file-creation parameters
	tmpl1.getSizes( parm1, parm2);
	verify_val(parm1, F2_OFFSET_SIZE, "FileCreatPropList::getSizes", __LINE__, __FILE__);
	verify_val(parm2, F2_LENGTH_SIZE, "FileCreatPropList::getSizes", __LINE__, __FILE__);

        unsigned  iparm1,iparm2;       // file-creation parameters
        tmpl1.getSymk( iparm1, iparm2);
        verify_val(iparm1, F2_SYM_INTERN_K, "FileCreatPropList::getSymk", __LINE__, __FILE__);
        verify_val(iparm2, F2_SYM_LEAF_K, "FileCreatPropList::getSymk", __LINE__, __FILE__);

	PASSED();
    }   // end of try block

    catch( Exception E ) {
        issue_fail_msg("test_file_open()", __LINE__, __FILE__, E.getCDetailMsg());
    }
}   // test_file_open()


/*-------------------------------------------------------------------------
 * Function:    test_file_size
 *
 * Purpose:     Test file size.
 *
 * Return:      None
 *
 * Programmer:  Raymond Lu
 *              June, 2004
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void test_file_size()
{
    // Output message about test being performed
    SUBTEST("File Size");

    hid_t	fapl_id;
    fapl_id = h5_fileaccess(); // in h5test.c, returns a file access template

    try {
        // Use the file access template id to create a file access prop.
        // list object to pass in H5File::H5File
        FileAccPropList fapl(fapl_id);

    	// Set to sec2 driver.  Do we want to test other file drivers?
        // They're not tested in C++.
        // File drivers seem not implemented.
	// fapl.setSec2();

        // Create a file
	H5File file4( FILE4, H5F_ACC_TRUNC, FileCreatPropList::DEFAULT, fapl);

        // Get file size
        hsize_t file_size = file4.getFileSize();

        // Check if file size is reasonable.  It's supposed to be 2KB now.
        if (file_size < 1*KB || file_size > 4*KB)
            issue_fail_msg("test_file_size()", __LINE__, __FILE__, "getFileSize() returned unreasonable value");

	// Get the amount of free space in the file
	hssize_t free_space = file4.getFreeSpace();

	// Check if it's reasonable.  It's 0 now.
	if (free_space < 0 || free_space > 4*KB)
	    issue_fail_msg("test_file_size()", __LINE__, __FILE__, "getFreeSpace returned unreasonable value");

	PASSED();
    }   // end of try block

    catch( Exception E ) {
        issue_fail_msg("test_file_size()", __LINE__, __FILE__, E.getCDetailMsg());
    }

    // use C test utility routine to close property list.
    H5Pclose(fapl_id);

}   // test_file_size()


/*-------------------------------------------------------------------------
 * Function:    test_file_name
 *
 * Purpose:     Test getting file's name.
 *
 * Return:      None
 *
 * Programmer:  Binh-Minh Ribler
 *              July, 2004
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
const int	RANK = 2;
const int	NX = 4;
const int	NY = 5;
const H5std_string	GROUPNAME ("group");
const H5std_string	DSETNAME ("dataset");
const H5std_string	DATTRNAME ("dataset attribute");
const H5std_string	FATTRNAME ("file attribute");
const H5std_string	DTYPENAME ("compound");

// Compound datatype
typedef struct s1_t {
    unsigned int a;
    float        b;
} s1_t;

static void test_file_name()
{
    // Output message about test being performed.
    SUBTEST("File Name");

    H5std_string file_name;
    try {
        // Create a file using default properties.
	H5File file4(FILE4, H5F_ACC_TRUNC);

        // Get file name from the file instance.
        file_name = file4.getFileName();
	verify_val(file_name, FILE4, "H5File::getFileName", __LINE__, __FILE__);

	// Create a group in the root group.
	Group group(file4.createGroup(GROUPNAME, 0));

	// Get and verify file name via a group.
	file_name = group.getFileName();
	verify_val(file_name, FILE4, "Group::getFileName", __LINE__, __FILE__);

	// Create the data space.
	hsize_t dims[RANK] = {NX, NY};
	DataSpace space(RANK, dims);

	// Create a new dataset.
	DataSet dataset(file4.createDataSet (DSETNAME, PredType::NATIVE_INT, space));

	// Get and verify file name via a dataset.
	file_name = dataset.getFileName();
	verify_val(file_name, FILE4, "DataSet::getFileName", __LINE__, __FILE__);

	// Create an attribute for the dataset.
	Attribute attr(dataset.createAttribute(DATTRNAME, PredType::NATIVE_INT, space));

	// Get and verify file name via an attribute.
	file_name = attr.getFileName();
	verify_val(file_name, FILE4, "Attribute::getFileName", __LINE__, __FILE__);

	// Create a compound datatype.
	CompType comp_type (sizeof(s1_t));

	// Insert fields.
	comp_type.insertMember("a", HOFFSET(s1_t, a), PredType::NATIVE_INT);
	comp_type.insertMember("b", HOFFSET(s1_t, b), PredType::NATIVE_FLOAT);

	// Save it on file.
	comp_type.commit(file4, DTYPENAME);

	// Get and verify file name via a committed datatype.
	comp_type.getFileName();
	verify_val(file_name, FILE4, "CompType::getFileName", __LINE__, __FILE__);
	PASSED();
    }   // end of try block

    catch (Exception E) {
        issue_fail_msg("test_file_name()", __LINE__, __FILE__, E.getCDetailMsg());
    }

}   // test_file_name()


#define NUM_OBJS	4
#define NUM_ATTRS	3
const int	RANK1 = 1;
const int	ATTR1_DIM1 = 3;
const H5std_string	FILE5("tfattrs.h5");
const H5std_string	FATTR1_NAME ("file attribute 1");
const H5std_string	FATTR2_NAME ("file attribute 2");
int fattr_data[ATTR1_DIM1]={512,-234,98123}; /* Test data for file attribute */
int dattr_data[ATTR1_DIM1]={256,-123,1000}; /* Test data for dataset attribute */
static void test_file_attribute()
{
    int rdata[ATTR1_DIM1];
    int i;

    // Output message about test being performed
    SUBTEST("File Attribute");

    H5std_string file_name;
    try {
        // Create a file using default properties.
	H5File file5(FILE5, H5F_ACC_TRUNC);

	// Create the data space
	hsize_t dims[RANK1] = {ATTR1_DIM1};
	DataSpace space(RANK1, dims);

	// Create two attributes for the file
	Attribute fattr1(file5.createAttribute(FATTR1_NAME, PredType::NATIVE_FLOAT, space));
	Attribute fattr2(file5.createAttribute(FATTR2_NAME, PredType::NATIVE_INT, space));

	fattr2.write(PredType::NATIVE_INT, fattr_data);

	try {
	    // Try to create the same attribute again (should fail)
	    Attribute fattr_dup(file5.createAttribute(FATTR2_NAME, PredType::NATIVE_INT, space));
	    // Should FAIL but didn't, so throw an invalid action exception
	    throw InvalidActionException("H5File createAttribute", "Attempted to create an existing attribute.");
	}
	catch( AttributeIException E ) // catch creating existing attribute
	{} // do nothing, FAIL expected

	// Create a new dataset
	DataSet dataset(file5.createDataSet (DSETNAME, PredType::NATIVE_INT, space));

	// Create an attribute for the dataset
	Attribute dattr(dataset.createAttribute(DATTRNAME, PredType::NATIVE_INT, space));

	// Write data to the second file attribute
	dattr.write(PredType::NATIVE_INT, dattr_data);

	// Test flushing out the data from the attribute object
        dattr.flush(H5F_SCOPE_GLOBAL);

	// Get and verify the number of all objects in the file
	// Current: 1 file, 2 file attr, 1 ds, and 1 ds attr.
	ssize_t num_objs = file5.getObjCount(H5F_OBJ_ALL);
	verify_val(num_objs, 5, "H5File::getObjCount", __LINE__, __FILE__);

	num_objs = file5.getObjCount(H5F_OBJ_GROUP);
	verify_val(num_objs, 0, "H5File::getObjCount(H5F_OBJ_GROUP)", __LINE__, __FILE__);
	num_objs = file5.getObjCount(H5F_OBJ_DATASET);
	verify_val(num_objs, 1, "H5File::getObjCount(H5F_OBJ_DATASET)", __LINE__, __FILE__);
	num_objs = file5.getObjCount(H5F_OBJ_ATTR);
	verify_val(num_objs, 3, "H5File::getObjCount(H5F_OBJ_ATTR)", __LINE__, __FILE__);
	num_objs = file5.getObjCount(H5F_OBJ_DATATYPE);
	verify_val(num_objs, 0, "H5File::getObjCount(H5F_OBJ_DATATYPE)", __LINE__, __FILE__);
	num_objs = file5.getObjCount(H5F_OBJ_FILE);
	verify_val(num_objs, 1, "H5File::getObjCount(H5F_OBJ_FILE)", __LINE__, __FILE__);
	
	// Get the file name using the attributes
	H5std_string fname = fattr1.getFileName();
	verify_val(fname, FILE5, "H5File::getFileName()", __LINE__, __FILE__);

	fname.clear();
	fname = dattr.getFileName();
	verify_val(fname, FILE5, "H5File::getFileName()", __LINE__, __FILE__);

	// Get the class of a file attribute's datatype
	H5T_class_t atclass = fattr1.getTypeClass();
	verify_val(atclass, H5T_FLOAT, "Attribute::getTypeClass()", __LINE__, __FILE__);

	// Get and verify the number of attributes attached to a file
	int n_attrs = file5.getNumAttrs();
	verify_val(n_attrs, 2, "H5File::getNumAttrs()", __LINE__, __FILE__);

	// Get and verify the number of attributes attached to a dataset
	n_attrs = 0;
	n_attrs = dataset.getNumAttrs();
	verify_val(n_attrs, 1, "DataSet::getNumAttrs()", __LINE__, __FILE__);

	// Read back attribute's data
	HDmemset(rdata, 0, sizeof(rdata));
        dattr.read(PredType::NATIVE_INT, rdata);
        /* Check results */
        for (i = 0; i < ATTR1_DIM1; i++) {
            if (rdata[i] != dattr_data[i]) {
                H5_FAILED();
		cerr << endl;
                cerr << "element [" << i << "] is " << rdata[i] <<
			"but should have been " << dattr_data[i] << endl;
                }
            }
	PASSED();
    }   // end of try block

    catch (Exception E) {
        issue_fail_msg("test_file_name()", __LINE__, __FILE__, E.getCDetailMsg());
    }
}   // test_file_attribute()

/*-------------------------------------------------------------------------
 * Function:    test_file
 *
 * Purpose:     Main file testing routine
 *
 * Return:      None
 *
 * Programmer:  Binh-Minh Ribler (use C version)
 *              January 2001
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
#ifdef __cplusplus
extern "C"
#endif
void test_file()
{
    // Output message about test being performed
    MESSAGE(5, ("Testing File I/O operations\n"));

    test_file_create();	// Test file creation (also creation templates)
    test_file_open();	// Test file opening
    test_file_size();	// Test file size
    test_file_name();	// Test getting file's name
    test_file_attribute();	// Test file attribute feature
}   // test_file()


/*-------------------------------------------------------------------------
 * Function:	cleanup_file
 *
 * Purpose:	Cleanup temporary test files
 *
 * Return:	none
 *
 * Programmer:  (use C version)
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
#ifdef __cplusplus
extern "C"
#endif
void cleanup_file()
{
    HDremove(FILE1.c_str());
    HDremove(FILE2.c_str());
    HDremove(FILE3.c_str());
    HDremove(FILE4.c_str());
}   // cleanup_file
