/*
 *  Note: this program is a modification of h5dump.c by Robb Matzke.
 */
#include <assert.h>
#include <ctype.h>
#include <h5dump.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*
 * The output functions need a temporary buffer to hold a piece of the
 * dataset while it's being printed.  This constant sets the limit on the
 * size of that temporary buffer in bytes.  For efficiency's sake, choose the
 * largest value suitable for your machine (for testing use a small value).
 */
#define H5DUMP_BUFSIZE	1024

#define OPT(X,S)	((X)?(X):(S))
#define MIN(X,Y)	((X)<(Y)?(X):(Y))
#define NELMTS(X)	(sizeof(X)/sizeof(*X))
#define ALIGN(A,Z)	((((A)+(Z)-1)/(Z))*(Z))

#define DATASET_DATA 	1

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
    hsize_t	p_prod[8], p_idx[8];
    hsize_t	n, i;
    char	temp[1024];

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
    size_t	i, n, offset;
    char	temp[1024], *name;
    hid_t	memb;
    int		nmembs, j;
    
    if (H5Tequal(type, H5T_NATIVE_DOUBLE)) {
	sprintf(temp, "%g", *((double*)vp));
    } else if (H5Tequal(type, H5T_NATIVE_FLOAT)) {
	sprintf(temp, "%g", *((float*)vp));
    } else if (H5Tequal(type, H5T_NATIVE_CHAR) ||
	       H5Tequal(type, H5T_NATIVE_UCHAR)) {
	switch (*((char*)vp)) {
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
	    h5dump_sprint(temp+strlen(temp), info, memb, (char*)vp+offset);
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
h5dump_simple(FILE *stream, const h5dump_t *info, hid_t dset, hid_t p_type, int obj_data)
{
    hid_t		f_space;		/*file data space	*/
    int			ndims;			/*dimensionality	*/
    hsize_t		elmtno, i;		/*counters		*/
    int			carry;			/*counter carry value	*/
    hssize_t		zero[8];		/*vector of zeros	*/
    int			need_prefix=1;		/*indices need printing	*/

    /* Print info */
    hsize_t		p_min_idx[8];		/*min selected index	*/
    hsize_t		p_max_idx[8];		/*max selected index	*/
    size_t		p_type_nbytes;		/*size of memory type	*/
    hsize_t		p_nelmts;		/*total selected elmts	*/
    char		p_buf[256];		/*output string		*/
    size_t		p_column=0;		/*output column		*/
    size_t		p_ncolumns=80;		/*default num columns	*/
    char 		p_prefix[1024];		/*line prefix string	*/

    /* Stripmine info */
    hsize_t		sm_size[8];		/*stripmine size	*/
    hsize_t		sm_nbytes;		/*bytes per stripmine	*/
    hsize_t		sm_nelmts;		/*elements per stripmine*/
    unsigned char	*sm_buf;		/*buffer for raw data	*/
    hid_t		sm_space;		/*stripmine data space	*/

    /* Hyperslab info */
    hssize_t		hs_offset[8];		/*starting offset	*/
    hsize_t		hs_size[8];		/*size this pass	*/
    hsize_t		hs_nelmts;		/*elements in request	*/

    /*
     * Check that everything looks okay.  The dimensionality must not be too
     * great and the dimensionality of the items selected for printing must
     * match the dimensionality of the dataset.
     */
    if (obj_data == DATASET_DATA)
         f_space = H5Dget_space(dset);
    else
         f_space = H5Aget_space(dset);
   
    ndims = H5Sextent_ndims(f_space);
    if ((size_t)ndims>NELMTS(sm_size)) return -1;

    /* Assume entire data space to be printed */
    for (i=0; i<(hsize_t)ndims; i++) p_min_idx[i] = 0;
    H5Sextent_dims(f_space, p_max_idx, NULL); 
    for (i=0, p_nelmts=1; i<(hsize_t)ndims; i++) {
	p_nelmts *= p_max_idx[i]-p_min_idx[i];
    }

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
	for (i=0, hs_nelmts=1; i<(hsize_t)ndims; i++) {
	    hs_size[i] = MIN(sm_size[i], p_max_idx[i]-hs_offset[i]);
	    hs_nelmts *= hs_size[i];
	}
	
	/* Read the data */
	H5Sselect_hyperslab(f_space, H5S_SELECT_SET, hs_offset, NULL,
			    hs_size, NULL);
	H5Sselect_hyperslab(sm_space, H5S_SELECT_SET, zero, NULL,
			    &hs_nelmts, NULL);


	if (obj_data == DATASET_DATA)
            H5Dread(dset, p_type, sm_space, f_space, H5P_DEFAULT, sm_buf);
        else
            H5Aread(dset, p_type, sm_buf);

	/* Print the data */
	for (i=0; i<hs_nelmts; i++) {
	    /* Render the element */
	    h5dump_sprint(p_buf, info, p_type, sm_buf+i*p_type_nbytes);
	    if (elmtno+i+1<p_nelmts) {
		strcat(p_buf, OPT(info->elmt_suf1, ","));
	    }

	    /* Print the prefix */
/* 
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
	    
 */

	    fputs(p_buf, stream);
/* 
	    p_column += strlen(p_buf);
 */
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
/* 

    if (p_column) {
	fputs(OPT(info->line_suf, ""), stream);
	putc('\n', stream);
	fputs(OPT(info->line_sep, ""), stream);
    }
 */
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
    int		nmembs, i;
    size_t	size;

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
	H5Tset_sign(m_type, H5Tget_size(f_type));
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

    case H5T_COMPOUND:
	nmembs = H5Tget_nmembers(f_type);
	memb = calloc(nmembs, sizeof(hid_t));
	name = calloc(nmembs, sizeof(char*));
	
	for (i=0, size=0; i<nmembs; i++) {
	    f_memb = H5Tget_member_type(f_type, i);
	    memb[i] = h5dump_fixtype(f_memb);
	    size = ALIGN(size, H5Tget_size(memb[i])) + H5Tget_size(memb[i]);
	    H5Tclose(f_memb);
	    name[i] = H5Tget_member_name(f_type, i);
	    if (memb[i]<0 || NULL==name[i]) goto done;
	}

	m_type = H5Tcreate(H5T_COMPOUND, size);
	for (i=0, size=0; i<nmembs; i++) {
	    H5Tinsert(m_type, name[i], size, memb[i]);
	    size = ALIGN(size, H5Tget_size(memb[i])) + H5Tget_size(memb[i]);
	}
	break;

    case H5T_TIME:
    case H5T_STRING:
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
    if (memb && name) {
	for (i=0; i<nmembs; i++) {
	    if (memb[i]>=0) H5Tclose(memb[i]);
	    if (name[i]) free(name[i]);
	}
	free(memb);
	free(name);
    }
    
    return m_type;
}


/*-------------------------------------------------------------------------
 * Function:	h5dump
 *
 * Purpose:	Print some values from a dataset.  The values to print are
 *		determined by the P_SPACE argument and the format to use to
 *		print them is determined by the P_TYPE argument.
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
h5dump1(FILE *stream, const h5dump_t *info, hid_t dset, hid_t _p_type, int obj_data)
{
    hid_t	f_space;
    hid_t	p_type = _p_type;
    hid_t	f_type;
    int		status;

    /* Check the data space */
    if (obj_data == DATASET_DATA) 
        f_space = H5Dget_space(dset);
    else
        f_space = H5Aget_space(dset);

    if (H5Sis_simple(f_space)<=0) return -1;
    H5Sclose(f_space);

    /*
     * Check the data type.  If the caller didn't supply a data type then
     * use an appropriate native data type.
     */

     if (obj_data == DATASET_DATA) 
         f_type = H5Dget_type(dset);
     else
         f_type = H5Aget_type(dset);

    if (p_type<0) {
	p_type = h5dump_fixtype(f_type);
	H5Tclose(f_type);
	if (p_type<0) return -1;
    }

    /* Print the data */
    status = h5dump_simple(stream, info, dset, p_type, obj_data);
    if (p_type!=_p_type) H5Tclose(p_type);
    return status;
}
