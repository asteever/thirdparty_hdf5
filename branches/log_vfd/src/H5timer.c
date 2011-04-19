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

/*-------------------------------------------------------------------------
 * Created: H5timer.c
 *          Aug 21 2006
 *          Quincey Koziol <koziol@hdfgroup.org>
 *
 * Purpose: Internal, platform-independent 'timer' support routines.
 *-------------------------------------------------------------------------
 */

/****************/
/* Module Setup */
/****************/


/***********/
/* Headers */
/***********/

/* Generic functions, including H5_timer_t */
#include "H5private.h"

/* Needed to fill the user-mode and system time fields of H5_timer_t */
#if defined(H5_HAVE_GETRUSAGE) && defined(H5_HAVE_SYS_RESOURCE_H)
#   include <sys/resource.h>
#endif

/* Needed to fill the elapsed time field of H5_timer_t */
#if defined(H5_HAVE_GETTIMEOFDAY) && defined(H5_HAVE_SYS_TIME_H)
#include <sys/time.h>
#endif


/****************/
/* Local Macros */
/****************/


/******************/
/* Local Typedefs */
/******************/


/********************/
/* Package Typedefs */
/********************/


/********************/
/* Local Prototypes */
/********************/


/*********************/
/* Package Variables */
/*********************/


/*****************************/
/* Library Private Variables */
/*****************************/


/*******************/
/* Local Variables */
/*******************/



/*-------------------------------------------------------------------------
 * Function: H5_timer_begin
 *
 * Purpose: Initialize a timer to time something.
 *
 * Return: void
 *
 * Programmer: Robb Matzke
 *             Thursday, April 16, 1998
 *-------------------------------------------------------------------------
 */
void
H5_timer_begin (H5_timer_t *timer)
{

#ifdef H5_HAVE_GETRUSAGE
    struct rusage     rusage;
#endif

#ifdef H5_HAVE_GETTIMEOFDAY
    struct timeval    etime;
#endif

    assert (timer);

#ifdef H5_HAVE_GETRUSAGE
    HDgetrusage (RUSAGE_SELF, &rusage);
    timer->utime = (double)rusage.ru_utime.tv_sec +
                   ((double)rusage.ru_utime.tv_usec / 1e6);
    timer->stime = (double)rusage.ru_stime.tv_sec +
                   ((double)rusage.ru_stime.tv_usec / 1e6);
#else
    timer->utime = 0.0;
    timer->stime = 0.0;
#endif

#ifdef H5_HAVE_GETTIMEOFDAY
    HDgettimeofday (&etime, NULL);
    timer->etime = (double)etime.tv_sec + ((double)etime.tv_usec / 1e6);
#else
    timer->etime = 0.0;
#endif

} /* end H5_timer_begin() */


/*-------------------------------------------------------------------------
 * Function:	H5_timer_end
 *
 * Purpose:	This function should be called at the end of a timed region.
 *		The SUM is an optional pointer which will accumulate times.
 *		TMS is the same struct that was passed to H5_timer_start().
 *		On return, TMS will contain total times for the timed region.
 *
 * Return:	void
 *
 * Programmer:	Robb Matzke
 *              Thursday, April 16, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void
H5_timer_end (H5_timer_t *sum/*in,out*/, H5_timer_t *timer/*in,out*/)
{
    H5_timer_t      now;

    assert (timer);
    H5_timer_begin (&now);

    timer->utime = MAX(0.0, now.utime - timer->utime);
    timer->stime = MAX(0.0, now.stime - timer->stime);
    timer->etime = MAX(0.0, now.etime - timer->etime);

    if (sum) {
        sum->utime += timer->utime;
        sum->stime += timer->stime;
        sum->etime += timer->etime;
    }
} /* end H5_timer_end() */


/*-------------------------------------------------------------------------
 * Function: H5_bandwidth
 *
 * Purpose: Prints the bandwidth (bytes per second) in a field 10
 *          characters wide widh four digits of precision like this:
 *
 *  NaN             If <=0 seconds
 *  1234.  TB/s
 *  123.4  TB/s
 *  12.34  GB/s
 *  1.234  MB/s
 *  4.000  kB/s
 *  1.000  B/s
 *  0.000  B/s      If NBYTES==0
 *  1.2345e-10      For bandwidth less than 1
 *  6.7893e+94      For exceptionally large values
 *  6.678e+106      For really big values
 *
 * Return: void
 *
 * Programmer: Robb Matzke
 *             Wednesday, August  5, 1998
 *-------------------------------------------------------------------------
 */
void
H5_bandwidth(char *buf/*out*/, double nbytes, double nseconds)
{
    double      bw;

    if(nseconds <= 0.0) {
        HDstrcpy(buf, "       NaN");
    } else {
        bw = nbytes/nseconds;
        if(fabs(bw) < 0.0000000001) {
            /* That is == 0.0, but direct comparison between floats is bad */
            HDstrcpy(buf, "0.000  B/s");
        } else if(bw < 1.0) {
            sprintf(buf, "%10.4e", bw);
        } else if(bw < 1024.0) {
            sprintf(buf, "%05.4f", bw);
            HDstrcpy(buf+5, "  B/s");
        } else if(bw < (1024.0 * 1024.0)) {
            sprintf(buf, "%05.4f", bw / 1024.0);
            HDstrcpy(buf+5, " kB/s");
        } else if(bw < (1024.0 * 1024.0 * 1024.0)) {
            sprintf(buf, "%05.4f", bw / (1024.0 * 1024.0));
            HDstrcpy(buf+5, " MB/s");
        } else if(bw < (1024.0 * 1024.0 * 1024.0 * 1024.0)) {
            sprintf(buf, "%05.4f", bw / (1024.0 * 1024.0 * 1024.0));
            HDstrcpy(buf+5, " GB/s");
        } else if(bw < (1024.0 * 1024.0 * 1024.0 * 1024.0 * 1024.0)) {
            sprintf(buf, "%05.4f", bw / (1024.0 * 1024.0 * 1024.0 * 1024.0));
            HDstrcpy(buf+5, " TB/s");
        } else {
            sprintf(buf, "%10.4e", bw);
            if(HDstrlen(buf) > 10) {
                sprintf(buf, "%10.3e", bw);
            }
        }
    }
} /* end H5_bandwidth() */


void
H5_timer_start2(H5_timer_t2 *timer/*in,out*/)
{
    /* If you have Unix getrusage(), init via that call */
    /* Else, if you have Windows GetSystemTimes(), init via that call */
    /* Else, if you have gettimeofday() use that */
    /* If all else fails, use time() */
    return;
}



void
H5_timer_get_times(H5_timer_t2 *timer/*in,out*/)
{
    /* If you have Unix getrusage(), init via that call */
    /* Else, if you have Windows GetSystemTimes(), init via that call */
    /* Else, if you have gettimeofday() use that */
    /* If all else fails, use time() */
    return;
}



#define H5TIMER_TIME_STRING_LEN 256

char *
H5_timer_print(H5_timespan_ns_t t)
{
    double hours    = 0;
    double minutes  = 0;
    double seconds  = 0;

    /* Allocate statically like ctime?
     * Might cause concurrency issues, but it'll be no worse than ctime().
     */
    static char *s[H5TIMER_TIME_STRING_LEN];

    /* Initialize */
    memset(s, 0, H5TIMER_TIME_STRING_LEN);
    seconds = t * 1.0E9;
    minutes = seconds / 60.0;
    hours   = seconds / 3600.0;

    /* Do we need a format string? Some people might like a certain 
     * number of milliseconds or s before spilling to the next highest
     * time unit.  Perhaps this could be passed as an integer.
     * (name? round_up_size? ?)
     */

    /* 1 decimal place on the lowest figure (s, ms, us or ns) */
    if(t < 1.0E3) {
        /* t < 1 us, Print time in ns */
        sprintf(s, "%.1f ns", t);
    } else if (t < 1.0E6) {
        /* t < 1 ms, Print time in us */
        sprintf(s, "%.1f us", t * 1.0E3);
    } else if (t < 1.0E9) {
        /* t < 1 s, Print time in ms */
        sprintf(s, "%.1f ms", t * 1.0E6);
    } else if (minutes < 1.0) {
        /* t < 1 m, Print time in s */
        sprintf(s, "%.1f s", seconds);
    } else if (hours < 0) {
        /* t < 1 h, Print time in m and s */
        sprintf(s, "%.f m %.1f s", minutes, seconds);
    } else {
        /* Print time in h, m and s */
        sprintf(s, "%.f h %.f m %.1f s", hours, minutes, seconds);
    }

    return &s;
}
