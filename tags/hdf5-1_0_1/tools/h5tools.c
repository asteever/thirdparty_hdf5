/*
 * Copyright � 1998 NCSA
 *                  All rights reserved.
 *
 * Programmer:  Robb Matzke <matzke@llnl.gov>
 *              Thursday, July 23, 1998
 *
 * Purpose:	A library for displaying the values of a dataset in a human
 *		readable format.
 */
#include <assert.h>
#include <ctype.h>
#include <h5tools.h>
#include <hdf5.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*
 * The output functions need a temporary buffer to hold a piece of the
 * dataset while it's being printed.  This constant sets the limit on the
 * size of that temporary buffer in bytes.  For efficiency's sake, choose the
 * largest value suitable for your machine (for testing use a small value).
 */
#if 1
#define H5DUMP_BUFSIZE	(1024*1024)
#else
#define H5DUMP_BUFSIZE	(1024)
#endif

#define OPT(X,S)	((X)?(X):(S))
#define MIN(X,Y)	((X)<(Y)?(X):(Y))
#define NELMTS(X)	(sizeof(X)/sizeof(*X))
#define ALIGN(A,Z)	((((A)+(Z)-1)/(Z))*(Z))


/*-------------------------------------------------------------------------
 * Function:	h5dump_prefix
 *
 * Purpose:	Prints the prefix to show up at the begining of the line.
 *
 * Return:	void
 *
 * Programmer:	Robb Matzke
 *              Thursday, July 23, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void
h5dump_prefix(char *s/*out*/, const h5dump_t *info, hsize_t elmtno, int ndims,
	      hsize_t min_idx[], hsize_t max_idx[])
{
    hsize_t	p_prod[H5S_MAX_RANK], p_idx[H5S_MAX_RANK];
    hsize_t	n, i=0;
    char	temp[1024];

    if (ndims>0) {
	/*
	 * Calculate the number of elements represented by a unit change in a
	 * certain index position.
	 */
	for (i=ndims-1, p_prod[ndims-1]=1; i>0; --i) {
	    p_prod[i-1] = (max_idx[i]-min_idx[i]) * p_prod[i];
	}

	/*
	 * Calculate the index values from the element number.
	 */
	for (i=0, n=elmtno; i<(hsize_t)ndims; i++) {
	    p_idx[i] = n / p_prod[i] + min_idx[i];
	    n %= p_prod[i];
	}

	/*
	 * Print the index values.
	 */
	*temp = '\0';
	for (i=0; i<(hsize_t)ndims; i++) {
	    if (i) strcat(temp, OPT(info->idx_sep, ","));
	    sprintf(temp+strlen(temp), OPT(info->idx_n_fmt, "%lu"),
		    (unsigned long)p_idx[i]);
	}
    } else {
	/* Scalar */
	sprintf(temp, OPT(info->idx_n_fmt, "%lu"), (unsigned long)0);
    }

    /*
     * Add prefix and suffix to the index.
     */
    sprintf(s, OPT(info->idx_fmt, "%s: "), temp);
}


/*-------------------------------------------------------------------------
 * Function:	h5dump_sprint
 *
 * Purpose:	Prints the value pointed to by VP into the string S assuming
 *		the data type of VP is TYPE.
 *
 * Return:	void
 *
 * Programmer:	Robb Matzke
 *              Thursday, July 23, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void
h5dump_sprint(char *s/*out*/, const h5dump_t *info, hid_t type, void *vp)
{
    size_t	i, n, offset, size, dims[H5S_MAX_RANK], nelmts;
    unsigned	overflow = 0xaaaaaaaa;
    char	temp[8192];
    char	*name, quote='\0';
    hid_t	memb;
    int		nmembs, j, k, ndims;
    const int	repeat_threshold = 8;
    
    if (H5Tequal(type, H5T_NATIVE_DOUBLE)) {
	sprintf(temp, "%g", *((double*)vp));
	
    } else if (H5Tequal(type, H5T_NATIVE_FLOAT)) {
	sprintf(temp, "%g", *((float*)vp));
	
    } else if (info->ascii &&
	       (H5Tequal(type, H5T_NATIVE_CHAR) ||
	        H5Tequal(type, H5T_NATIVE_UCHAR))) {
	switch (*((char*)vp)) {
	case '"':
	    strcpy(temp, "\\\"");
	    break;
	case '\\':
	    strcpy(temp, "\\\\");
	    break;
	case '\b':
	    strcpy(temp, "\\b");
	    break;
	case '\f':
	    strcpy(temp, "\\f");
	    break;
	case '\n':
	    strcpy(temp, "\\n");
	    break;
	case '\r':
	    strcpy(temp, "\\r");
	    break;
	case '\t':
	    strcpy(temp, "\\t");
	    break;
	default:
	    if (isprint(*((char*)vp))) sprintf(temp, "%c", *((char*)vp));
	    else sprintf(temp, "\\%03o", *((unsigned char*)vp));
	    break;
	}
	
    } else if (H5T_STRING==H5Tget_class(type)) {
	size = H5Tget_size(type);
	temp[0] = '\0';
	quote = '\0';

	for (i=0; i<size; i++) {

	    /* Count how many times the next character repeats */
	    j=1;
	    while (i+j<size && ((char*)vp)[i]==((char*)vp)[i+j]) j++;

	    /*
	     * Print the opening quote.  If the repeat count is high enough
	     * to warrant printing the number of repeats instead of
	     * enumerating the characters, then make sure the character to be
	     * repeated is in it's own quote.
	     */
	    if (j>repeat_threshold) {
		if (quote) sprintf(temp+strlen(temp),  "%c", quote);
		quote = '\'';
		sprintf(temp+strlen(temp), "%s%c", i?" ":"", quote);
	    } else if (!quote) {
		quote = '"';
		sprintf(temp+strlen(temp), "%s%c", i?" ":"", quote);
	    }

	    /* Print the character */
	    switch (((char*)vp)[i]) {
	    case '"':
		strcat(temp, "\\\"");
		break;
	    case '\\':
		strcat(temp, "\\\\");
		break;
	    case '\b':
		strcat(temp, "\\b");
		break;
	    case '\f':
		strcat(temp, "\\f");
		break;
	    case '\n':
		strcat(temp, "\\n");
		break;
	    case '\r':
		strcat(temp, "\\r");
		break;
	    case '\t':
		strcat(temp, "\\t");
		break;
	    default:
		if (isprint(((char*)vp)[i])) {
		    sprintf(temp+strlen(temp), "%c", ((char*)vp)[i]);
		} else {
		    sprintf(temp+strlen(temp), "\\%03o",
			    ((unsigned char*)vp)[i]);
		}
		break;
	    }

	    /* Print the repeat count */
	    if (j>repeat_threshold) {
		sprintf(temp+strlen(temp), "%c repeats %d times", quote, j-1);
		quote = '\0';
		i += j-1;
	    }
	}
	if (quote) sprintf(temp+strlen(temp), "%c", quote);
	
    } else if (H5Tequal(type, H5T_NATIVE_CHAR)) {
	sprintf(temp, "%d", *((signed char*)vp));
	
    } else if (H5Tequal(type, H5T_NATIVE_UCHAR)) {
	sprintf(temp, "%u", *((unsigned char*)vp));
	
    } else if (H5Tequal(type, H5T_NATIVE_SHORT)) {
	sprintf(temp, "%d", *((short*)vp));
	
    } else if (H5Tequal(type, H5T_NATIVE_USHORT)) {
	sprintf(temp, "%u", *((unsigned short*)vp));
	
    } else if (H5Tequal(type, H5T_NATIVE_INT)) {
	sprintf(temp, "%d", *((int*)vp));
	
    } else if (H5Tequal(type, H5T_NATIVE_UINT)) {
	sprintf(temp, "%u", *((unsigned*)vp));
	
    } else if (H5Tequal(type, H5T_NATIVE_LONG)) {
	sprintf(temp, "%ld", *((long*)vp));
	
    } else if (H5Tequal(type, H5T_NATIVE_ULONG)) {
	sprintf(temp, "%lu", *((unsigned long*)vp));
	
    } else if (H5Tequal(type, H5T_NATIVE_HSSIZE)) {
	if (sizeof(hssize_t)==sizeof(long)) {
	    sprintf(temp, "%ld", *((long*)vp));
	} else {
	    char fmt[8];
	    strcpy(fmt, "%");
	    strcat(fmt, PRINTF_LL_WIDTH);
	    strcat(fmt, "d");
	    sprintf(temp, fmt, *((long long*)vp));
	}
	
    } else if (H5Tequal(type, H5T_NATIVE_HSIZE)) {
	if (sizeof(hsize_t)==sizeof(long)) {
	    sprintf(temp, "%lu", *((unsigned long*)vp));
	} else {
	    char fmt[8];
	    strcpy(fmt, "%");
	    strcat(fmt, PRINTF_LL_WIDTH);
	    strcat(fmt, "u");
	    sprintf(temp, fmt, *((unsigned long long*)vp));
	}
	
    } else if (H5T_COMPOUND==H5Tget_class(type)) {
	nmembs = H5Tget_nmembers(type);
	strcpy(temp, OPT(info->cmpd_pre, "{"));
	for (j=0; j<nmembs; j++) {
	    if (j) strcat(temp, OPT(info->cmpd_sep, ","));

	    /* The name */
	    name = H5Tget_member_name(type, j);
	    sprintf(temp+strlen(temp), OPT(info->cmpd_name, ""), name);
	    free(name);

	    /* The value */
	    offset = H5Tget_member_offset(type, j);
	    memb = H5Tget_member_type(type, j);
	    size = H5Tget_size(memb);
	    ndims = H5Tget_member_dims(type, j, dims, NULL);
	    assert(ndims>=0 && ndims<=H5S_MAX_RANK);
	    for (k=0, nelmts=1; k<ndims; k++) nelmts *= dims[k];

	    if (nelmts>1) strcat(temp, OPT(info->arr_pre, "["));
	    for (i=0; i<nelmts; i++) {
		if (i) strcat(temp, OPT(info->arr_sep, ","));
		h5dump_sprint(temp+strlen(temp), info, memb,
			      (char*)vp+offset+i*size);
	    }
	    if (nelmts>1) strcat(temp, OPT(info->arr_suf, "]"));
	    H5Tclose(memb);
	}
	strcat(temp, OPT(info->cmpd_suf, "}"));
	
    } else {
	strcpy(temp, "0x");
	n = H5Tget_size(type);
	for (i=0; i<n; i++) {
	    sprintf(temp+strlen(temp), "%02x", ((unsigned char*)vp)[i]);
	}
    }

    sprintf(s, OPT(info->elmt_fmt, "%s"), temp);

    /*
     * We should really fix this so it's not possible to overflow the `temp'
     * buffer.
     */
    assert(overflow==0xaaaaaaaa);
}


/*-------------------------------------------------------------------------
 * Function:	h5dump_simple
 *
 * Purpose:	Print some values from a dataset with a simple data space.
 *		This is a special case of h5dump().
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Robb Matzke
 *              Thursday, July 23, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
h5dump_simple(FILE *stream, const h5dump_t *info, hid_t dset, hid_t p_type)
{
    hid_t		f_space;		/*file data space	*/
    int			ndims;			/*dimensionality	*/
    hsize_t		elmtno, i;		/*counters		*/
    int			carry;			/*counter carry value	*/
    hssize_t		zero[8];		/*vector of zeros	*/
    int			need_prefix=1;		/*indices need printing	*/

    /* Print info */
    hsize_t		p_min_idx[H5S_MAX_RANK];/*min selected index	*/
    hsize_t		p_max_idx[H5S_MAX_RANK];/*max selected index	*/
    size_t		p_type_nbytes;		/*size of memory type	*/
    hsize_t		p_nelmts;		/*total selected elmts	*/
    char		p_buf[8192];		/*output string		*/
    size_t		p_column=0;		/*output column		*/
    size_t		p_ncolumns=80;		/*default num columns	*/
    char 		p_prefix[1024];		/*line prefix string	*/

    /* Stripmine info */
    hsize_t		sm_size[H5S_MAX_RANK];	/*stripmine size	*/
    hsize_t		sm_nbytes;		/*bytes per stripmine	*/
    hsize_t		sm_nelmts;		/*elements per stripmine*/
    unsigned char	*sm_buf;		/*buffer for raw data	*/
    hid_t		sm_space;		/*stripmine data space	*/

    /* Hyperslab info */
    hssize_t		hs_offset[H5S_MAX_RANK];/*starting offset	*/
    hsize_t		hs_size[H5S_MAX_RANK];	/*size this pass	*/
    hsize_t		hs_nelmts;		/*elements in request	*/

    /*
     * Check that everything looks okay.  The dimensionality must not be too
     * great and the dimensionality of the items selected for printing must
     * match the dimensionality of the dataset.
     */
    f_space = H5Dget_space(dset);
    ndims = H5Sget_simple_extent_ndims(f_space);
    if ((size_t)ndims>NELMTS(sm_size)) return -1;

    /* Assume entire data space to be printed */
    for (i=0; i<(hsize_t)ndims; i++) p_min_idx[i] = 0;
    H5Sget_simple_extent_dims(f_space, p_max_idx, NULL);
    for (i=0, p_nelmts=1; i<(hsize_t)ndims; i++) {
	p_nelmts *= p_max_idx[i]-p_min_idx[i];
    }
    if (0==p_nelmts) return 0; /*nothing to print*/

    /*
     * Determine the strip mine size and allocate a buffer.  The strip mine is
     * a hyperslab whose size is manageable.
     */
    p_type_nbytes = H5Tget_size(p_type);
    for (i=ndims, sm_nbytes=p_type_nbytes; i>0; --i) {
	sm_size[i-1] = MIN (p_max_idx[i-1]-p_min_idx[i-1],
			    H5DUMP_BUFSIZE/sm_nbytes);
	sm_nbytes *= sm_size[i-1];
	assert(sm_nbytes>0);
    }
    sm_buf = malloc(sm_nbytes);
    sm_nelmts = sm_nbytes/p_type_nbytes;
    sm_space = H5Screate_simple(1, &sm_nelmts, NULL);

    /* Local things */
    if (info->line_ncols>0) p_ncolumns = info->line_ncols;

    /* The stripmine loop */
    memset(hs_offset, 0, sizeof hs_offset);
    memset(zero, 0, sizeof zero);
    for (elmtno=0; elmtno<p_nelmts; elmtno+=hs_nelmts) {

	/* Calculate the hyperslab size */
	if (ndims>0) {
	    for (i=0, hs_nelmts=1; i<(hsize_t)ndims; i++) {
		hs_size[i] = MIN(sm_size[i], p_max_idx[i]-hs_offset[i]);
		hs_nelmts *= hs_size[i];
	    }
	    H5Sselect_hyperslab(f_space, H5S_SELECT_SET, hs_offset, NULL,
				hs_size, NULL);
	    H5Sselect_hyperslab(sm_space, H5S_SELECT_SET, zero, NULL,
				&hs_nelmts, NULL);
	} else {
	    H5Sselect_all(f_space);
	    H5Sselect_all(sm_space);
	    hs_nelmts = 1;
	}
	
	/* Read the data */
	if (H5Dread(dset, p_type, sm_space, f_space, H5P_DEFAULT, sm_buf)<0) {
	    return -1;
	}
	
	/* Print the data */
	for (i=0; i<hs_nelmts; i++) {
	    /* Render the element */
	    h5dump_sprint(p_buf, info, p_type, sm_buf+i*p_type_nbytes);
	    if (elmtno+i+1<p_nelmts) {
		strcat(p_buf, OPT(info->elmt_suf1, ","));
	    }

	    /* Print the prefix */
	    if ((p_column +
		 strlen(p_buf) +
		 strlen(OPT(info->elmt_suf2, " ")) +
		 strlen(OPT(info->line_suf, ""))) > p_ncolumns) {
		need_prefix = 1;
	    }
	    if (need_prefix) {
		h5dump_prefix(p_prefix, info, elmtno+i, ndims,
			      p_min_idx, p_max_idx);
		if (p_column) {
		    fputs(OPT(info->line_suf, ""), stream);
		    putc('\n', stream);
		    fputs(OPT(info->line_sep, ""), stream);
		}
		fputs(p_prefix, stream);
		p_column = strlen(p_prefix);
		need_prefix = 0;
	    } else {
		fputs(OPT(info->elmt_suf2, " "), stream);
		p_column += strlen(OPT(info->elmt_suf2, " "));
	    }
	    
	    fputs(p_buf, stream);
	    p_column += strlen(p_buf);
	}
	
	/* Calculate the next hyperslab offset */
	for (i=ndims, carry=1; i>0 && carry; --i) {
	    hs_offset[i-1] += hs_size[i-1];
	    if (hs_offset[i-1]==(hssize_t)p_max_idx[i-1]) {
		hs_offset[i-1] = p_min_idx[i-1];
	    } else {
		carry = 0;
	    }
	}
    }

    if (p_column) {
	fputs(OPT(info->line_suf, ""), stream);
	putc('\n', stream);
	fputs(OPT(info->line_sep, ""), stream);
    }
    H5Sclose(sm_space);
    H5Sclose(f_space);
    return 0;
}


/*-------------------------------------------------------------------------
 * Function:	h5dump_fixtype
 *
 * Purpose:	Given a file data type choose a memory data type which is
 *		appropriate for printing the data.
 *
 * Return:	Success:	Memory data type
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Thursday, July 23, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static hid_t
h5dump_fixtype(hid_t f_type)
{
    hid_t	m_type=-1, f_memb;
    hid_t	*memb=NULL;
    char	**name=NULL;
    int		nmembs=0, i, j, *ndims=NULL;
    size_t	size, offset, *dims=NULL, nelmts;

    size = H5Tget_size(f_type);
    switch (H5Tget_class(f_type)) {

    case H5T_INTEGER:
	/*
	 * Use the smallest native integer type of the same sign as the file
	 * such that the memory type is at least as large as the file type.
	 * If there is no memory type large enough then use the largest
	 * memory type available.
	 */
	if (size<=sizeof(char)) {
	    m_type = H5Tcopy(H5T_NATIVE_CHAR);
	} else if (size<=sizeof(short)) {
	    m_type = H5Tcopy(H5T_NATIVE_SHORT);
	} else if (size<=sizeof(int)) {
	    m_type = H5Tcopy(H5T_NATIVE_INT);
	} else if (size<=sizeof(long)) {
	    m_type = H5Tcopy(H5T_NATIVE_LONG);
	} else {
	    m_type = H5Tcopy(H5T_NATIVE_LLONG);
	}
	H5Tset_sign(m_type, H5Tget_sign(f_type));
	break;
	
    case H5T_FLOAT:
	/*
	 * Use the smallest native floating point type available such that
	 * its size is at least as large as the file type.  If there is not
	 * native type large enough then use the largest native type.
	 */
	if (size<=sizeof(float)) {
	    m_type = H5Tcopy(H5T_NATIVE_FLOAT);
	} else if (size<=sizeof(double)) {
	    m_type = H5Tcopy(H5T_NATIVE_DOUBLE);
	} else {
	    m_type = H5Tcopy(H5T_NATIVE_LDOUBLE);
	}
	break;

    case H5T_STRING:
	m_type = H5Tcopy(f_type);
	H5Tset_cset(m_type, H5T_CSET_ASCII);
	H5Tset_strpad(m_type, H5T_STR_NULLPAD);
	break;

    case H5T_COMPOUND:
	/*
	 * We have to do this in two steps.  The first step scans the file
	 * type and converts the members to native types and remembers all
	 * their names and sizes, computing the size of the memory compound
	 * type at the same time.  Then we create the memory compound type
	 * and add the members.
	 */
	nmembs = H5Tget_nmembers(f_type);
	memb = calloc(nmembs, sizeof(hid_t));
	name = calloc(nmembs, sizeof(char*));
	ndims = calloc(nmembs, sizeof(int));
	dims = calloc(nmembs*4, sizeof(size_t));
	
	for (i=0, size=0; i<nmembs; i++) {

	    /* Get the member type and fix it */
	    f_memb = H5Tget_member_type(f_type, i);
	    memb[i] = h5dump_fixtype(f_memb);
	    H5Tclose(f_memb);
	    if (memb[i]<0) goto done;

	    /* Get the member dimensions */
	    ndims[i] = H5Tget_member_dims(f_type, i, dims+i*4, NULL);
	    assert(ndims[i]>=0 && ndims[i]<=4);
	    for (j=0, nelmts=1; j<ndims[i]; j++) nelmts *= dims[i*4+j];

	    /* Get the member name */
	    name[i] = H5Tget_member_name(f_type, i);
	    if (NULL==name[i]) goto done;

	    /*
	     * Compute the new offset so each member is aligned on a byte
	     * boundary which is the same as the member size.
	     */
	    size = ALIGN(size, H5Tget_size(memb[i])) +
		     nelmts * H5Tget_size(memb[i]);
	}

	m_type = H5Tcreate(H5T_COMPOUND, size);
	for (i=0, offset=0; i<nmembs; i++) {
	    H5Tinsert_array(m_type, name[i], offset, ndims[i], dims+i*4,
			    NULL, memb[i]);
	    for (j=0, nelmts=1; j<ndims[i]; j++) nelmts *= dims[i*4+j];
	    offset = ALIGN(offset, H5Tget_size(memb[i])) +
		     nelmts * H5Tget_size(memb[i]);
	}
	break;

    case H5T_TIME:
    case H5T_BITFIELD:
    case H5T_OPAQUE:
	/*
	 * These type classes are not implemented yet.
	 */
	break;

    default:
	/* What the heck? */
	break;
    }

 done:
    /* Clean up temp buffers */
    if (memb && name && ndims && dims) {
	for (i=0; i<nmembs; i++) {
	    if (memb[i]>=0) H5Tclose(memb[i]);
	    if (name[i]) free(name[i]);
	}
	free(memb);
	free(name);
	free(ndims);
	free(dims);
    }
    
    return m_type;
}


/*-------------------------------------------------------------------------
 * Function:	h5dump
 *
 * Purpose:	Print some values from a dataset DSET to the file STREAM
 *		after converting all types to P_TYPE (which should be a
 *		native type).  If P_TYPE is a negative value then it will be
 *		computed from the dataset type using only native types.
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Robb Matzke
 *              Thursday, July 23, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
h5dump(FILE *stream, const h5dump_t *info, hid_t dset, hid_t _p_type)
{
    hid_t	f_space;
    hid_t	p_type = _p_type;
    hid_t	f_type;
    int		status;
    h5dump_t	info_dflt;

    /* Use default values */
    if (!stream) stream = stdout;
    if (!info) {
	memset(&info_dflt, 0, sizeof info_dflt);
	info = &info_dflt;
    }
    if (p_type<0) {
	f_type = H5Dget_type(dset);
	p_type = h5dump_fixtype(f_type);
	H5Tclose(f_type);
	if (p_type<0) return -1;
    }

    /* Check the data space */
    f_space = H5Dget_space(dset);
    if (H5Sis_simple(f_space)<=0) return -1;
    H5Sclose(f_space);

    /* Print the data */
    status = h5dump_simple(stream, info, dset, p_type);
    if (p_type!=_p_type) H5Tclose(p_type);
    return status;
}
