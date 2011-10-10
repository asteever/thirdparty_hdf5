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

/*
 * Programmer:  Scott Wegner <swegner@hdfgroup.org>
 *				Based on code by Robb Matzke
 *				May 24, 2007
 *
 * Purpose:	We would like to create a driver specifically for Windows
 *			to utilize the Win32 API, and reduce the maintenence demands
 *			for the other file drivers.  Our other motivation is that
 *			the Windows system calls of the existing sec2 driver differ
 *			from those on other platforms, and are not 64-bit compatible.
 *			From the start, this will have the structure very similar
 *			to our sec2 driver, but make system calls more similar to
 *			our stdio driver.
 */

#include "H5private.h"      /* Generic Functions        */
#include "H5Eprivate.h"     /* Error handling           */
#include "H5Fprivate.h"     /* File access              */
#include "H5FDprivate.h"    /* File drivers             */
#include "H5FDwindows.h"    /* Windows file driver      */
#include "H5FDsec2.h"       /* Windows file driver      */
#include "H5FLprivate.h"    /* Free Lists               */
#include "H5Iprivate.h"     /* IDs                      */
#include "H5MMprivate.h"    /* Memory management        */
#include "H5Pprivate.h"     /* Property lists           */

#ifdef H5_HAVE_WINDOWS


/*-------------------------------------------------------------------------
 * Function:	H5Pset_fapl_windows
 *
 * Purpose:	Modify the file access property list to use the H5FD_WINDOWS
 *		driver defined in this source file.  There are no driver
 *		specific properties.
 *
 * NOTE: The Windows VFD was merely a merge of the SEC2 and STDIO drivers
 *       so it has been retired.  Selecting the Windows VFD will actually
 *       set the SEC2 VFD (though for backwards compatibility, we'll keep
 *       the H5FD_WINDOWS symbol).
 *
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Scott Wegner
 *				Based on code by Robb Matzke
 *				Thursday, May 24 2007
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_fapl_windows(hid_t fapl_id)
{
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t ret_value;

    FUNC_ENTER_API(H5Pset_fapl_windows, FAIL)
    H5TRACE1("e", "i", fapl_id);

    if(NULL == (plist = H5P_object_verify(fapl_id, H5P_FILE_ACCESS)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file access property list")

    ret_value = H5P_set_driver(plist, H5FD_WINDOWS, NULL);

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pset_fapl_windows() */

#endif /* H5_HAVE_WINDOWS */

