/*  This file is part of the Pablo Performance Analysis Environment
// 
//           (R)
//  The Pablo    Performance Analysis Environment software is NOT in
//  the public domain.  However, it is freely available without fee for
//  education, research, and non-profit purposes.  By obtaining copies
//  of this and other files that comprise the Pablo Performance Analysis
//  Environment, you, the Licensee, agree to abide by the following
//  conditions and understandings with respect to the copyrighted software:
//  
//  1.  The software is copyrighted in the name of the Board of Trustees
//      of the University of Illinois (UI), and ownership of the software
//      remains with the UI. 
// 
//  2.  Permission to use, copy, and modify this software and its documentation
//      for education, research, and non-profit purposes is hereby granted
//      to Licensee, provided that the copyright notice, the original author's
//      names and unit identification, and this permission notice appear on
//      all such copies, and that no charge be made for such copies.  Any
//      entity desiring permission to incorporate this software into commercial
//      products should contact:
// 
//           Professor Daniel A. Reed                 reed@cs.uiuc.edu
//           University of Illinois
//           Department of Computer Science
//           2413 Digital Computer Laboratory
//           1304 West Springfield Avenue
//           Urbana, Illinois  61801
//           USA
// 
//  3.  Licensee may not use the name, logo, or any other symbol of the UI
//      nor the names of any of its employees nor any adaptation thereof in
//      advertizing or publicity pertaining to the software without specific
//      prior written approval of the UI.
// 
//  4.  THE UI MAKES NO REPRESENTATIONS ABOUT THE SUITABILITY OF THE
//      SOFTWARE FOR ANY PURPOSE.  IT IS PROVIDED "AS IS" WITHOUT EXPRESS
//      OR IMPLIED WARRANTY.
// 
//  5.  The UI shall not be liable for any damages suffered by Licensee from
//      the use of this software.
// 
//  6.  The software was developed under agreements between the UI and the
//      Federal Government which entitle the Government to certain rights.
// 
// *************************************************************************
// 
//  Developed by: The Pablo Research Group
//                University of Illinois at Urbana-Champaign
//                Department of Computer Science
//                1304 W. Springfield Avenue
//                Urbana, IL     61801
// 
//                http://www-pablo.cs.uiuc.edu
// 
//  Send comments to: pablo-feedback@guitar.cs.uiuc.edu
// 
//  Copyright (c) 1987-1998
//  The University of Illinois Board of Trustees.
//       All Rights Reserved.
// 
//  PABLO is a registered trademark of
//  The Board of Trustees of the University of Illinois
//  registered in the U.S. Patent and Trademark Office.
// 
//  Project Manager and Principal Investigator:
//       Daniel A. Reed (reed@cs.uiuc.edu)
// 
// Funded in part by the Defense Advanced Research Projects Agency under 
// DARPA contracts DABT63-94-C0049 (SIO Initiative), F30602-96-C-0161,
// and DABT63-96-C-0027 by the National Science Foundation under the PACI 
// program and grants NSF CDA 94-01124 and ASC 97-20202, and by the 
// Department of Energy under contracts DOE B-341494, W-7405-ENG-48, and 
// 1-B-333164.
//-------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------
 * File:  ProcIDs.h
 * Purpose: define IDs for identifying procedures in traces
 *-------------------------------------------------------------------------*/

#ifndef PROCIDS_H		/* avoid re-inclusion */
#define PROCIDS_H

extern int *procTrace;
/*
 * Define the event IDs that will be used for the various HDF events
 */
#include "ProcTrace.h"

#define H5_mask			ID_H5_c
#define H5A_mask		ID_H5A_c
#define H5AC_mask		ID_H5AC_c
#define H5B_mask		ID_H5B_c
#define H5D_mask		ID_H5D_c
#define H5E_mask		ID_H5E_c
#define H5F_mask		ID_H5F_c
#define H5F_arr_mask		ID_H5Farray_c
#define H5F_core_mask		ID_H5Fcore_c
#define H5F_family_mask		ID_H5Ffamily_c
#define H5F_istore_mask		ID_H5Fistore_c
#define H5F_low_mask		ID_H5Flow_c
#define H5F_mpio_mask		ID_H5Fmpio_c
#define H5F_sec2_mask		ID_H5Fsec2_c
#define H5F_split_mask		ID_H5Fsplit_c
#define H5F_stdio_mask		ID_H5Fstdio_c
#define H5G_mask		ID_H5G_c
#define H5G_ent_mask		ID_H5Gent_c
#define H5G_node_mask		ID_H5Gnode_c
#define H5G_stab_mask		ID_H5Gstab_c
#define H5HG_mask		ID_H5HG_c
#define H5HL_mask		ID_H5HL_c
#define H5I_mask		ID_H5I_c
#define H5MF_mask		ID_H5MF_c
#define H5MM_mask		ID_H5MM_c
#define H5O_mask		ID_H5O_c
#define H5O_attr_mask		ID_H5Oattr_c
#define H5O_pline_mask		ID_H5Ocomp_c
#define H5O_cont_mask		ID_H5Ocont_c
#define H5O_dtype_mask		ID_H5Odtype_c
#define H5O_efl_mask		ID_H5Oefl_c
#define H5O_fill_mask		ID_H5Ofill_c
#define H5O_layout_mask		ID_H5Olayout_c
#define H5O_mtime_mask		ID_H5Omtime_c
#define H5O_name_mask		ID_H5Oname_c
#define H5O_null_mask		ID_H5Onull_c
#define H5O_sdspace_mask	ID_H5Osdspace_c
#define H5O_shared_mask		ID_H5Oshared_c
#define H5O_stab_mask		ID_H5Ostab_c
#define H5P_mask		ID_H5P_c
#define H5R_mask		ID_H5R_c
#define H5RA_mask		ID_H5RA_c
#define H5S_mask		ID_H5S_c
#define H5S_all_mask		ID_H5Sall_c
#define H5S_hyper_mask		ID_H5Shyper_c
#define H5S_mpio_mask		ID_H5Smpio_c
#define H5S_none_mask		ID_H5Snone_c
#define H5S_point_mask		ID_H5Spoint_c
#define H5S_select_mask		ID_H5Sselect_c
#define H5T_mask		ID_H5T_c
#define H5TB_mask		ID_H5TB_c
#define H5Tbit_mask		ID_H5Tbit_c
#define H5T_conv_mask		ID_H5Tconv_c
#define H5T_init_mask		ID_H5Tinit_c
#define H5V_mask		ID_H5V_c
#define H5Z_mask		ID_H5Z_c

#define	ID_HDFprocName		9996
#define	ID_malloc		9997
#define	ID_free			9998
#define	ID_timeStamp		9999
#define	DUMMY_HDF		10000

#define BEGIN_HDF (DUMMY_HDF + 1)
#define END_HDF (ID_HDF_Last_Entry + DUMMY_HDF)
#define NumHDFProcs ( ID_HDF_Last_Entry )

#define BEGIN_MPIO            900800
#define END_MPIO              900899
/*======================================================================*/
/* Macros to tell if the ID is that of an HDF Entry or Exit             */
/*======================================================================*/
#define isBeginHDFEvent( ID ) ( BEGIN_HDF <= (ID) && (ID) <= END_HDF )
#define isEndHDFEvent( ID )   isBeginHDFEvent(-(ID))
#define isBeginMPIOEvent( ID ) \
            ( BEGIN_MPIO <= (ID) && (ID) <= END_MPIO && (ID)%2 == 0 )

#define isEndMPIOEvent( ID ) \
            ( BEGIN_MPIO <= (ID) && (ID) <= END_MPIO && (ID)%2 == 1 )
#define isBeginIOEvent( ID )  \
        ( IOerrorID < (ID) && (ID) <= fsetposEndID && (ID)%2 == 1 )
#define isEndIOEvent( ID ) \
        ( IOerrorID < (ID) && (ID) <= fsetposEndID && (ID)%2 == 0 )
#define ProcIndex( ID ) ( (ID) - BEGIN_HDF )
#define ProcIndexForHDFEntry( ID ) ( (ID) - BEGIN_HDF )
#define ProcIndexForHDFExit( ID )  ProcIndexForHDFEntry(-ID)
#define HDFIXtoEventID( ID ) ( (ID) + BEGIN_HDF )

#define TRACE_ON(mask, ID) \
if ( procTrace[mask] || procTrace[ID] ) startHDFtraceEvent( HDFIXtoEventID( ID ) )
#define TRACE_OFF(mask, ID ) \
if ( procTrace[mask] || procTrace[ID] ) endHDFtraceEvent(-HDFIXtoEventID(ID), 0, NULL, 0 )
 
#endif /* PROCIDS_H */
