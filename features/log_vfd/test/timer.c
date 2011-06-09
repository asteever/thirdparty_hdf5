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
 * Programmer:  Dana Robinson
 *              May, 2011
 *
 * Purpose:     Tests the operation of the platform-independent timers.
 */

#include "h5test.h"




/*-------------------------------------------------------------------------
 * Function:    test_time_formatting
 *
 * Purpose:     Tests time string creation.
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Dana Robinson
 *              May 2011
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_time_formatting(void)
{
    char *s = NULL;

    TESTING("Time string formats");

    /*      < 0,            N/A             */
    s = H5_timer_get_time_string(-1.0);
    if(NULL == s || strcmp(s, "N/A") != 0)
        TEST_ERROR;
    free(s);

    /*      0               0               */
    s = H5_timer_get_time_string(0.0);
    if(NULL == s || strcmp(s, "0 ns") != 0)
        TEST_ERROR;
    free(s);

    /*      < 1e3 ns        nanoseconds     */
    s = H5_timer_get_time_string(123000.0);
    if(NULL == s || strcmp(s, "123 ns") != 0)
        TEST_ERROR;
    free(s);

    /*      < 1e6 ns        microseconds    */
    s = H5_timer_get_time_string(23456000.0);
    if(NULL == s || strcmp(s, "23.5 us") != 0)
        TEST_ERROR;
    free(s);

    /*      < 1e9 ns        milliseconds    */
    s = H5_timer_get_time_string(4567890000.0);
    if(NULL == s || strcmp(s, "4.6 ms") != 0)
        TEST_ERROR;
    free(s);

    /*      < 1 min         seconds         */
    s = H5_timer_get_time_string(3.14e12);
    if(NULL == s || strcmp(s, "3.14 s") != 0)
        TEST_ERROR;
    free(s);

    /*      < 1 hr          mins, secs      */
    s = H5_timer_get_time_string(2.521e15);
    if(NULL == s || strcmp(s, "42 m 1 s") != 0)
        TEST_ERROR;
    free(s);

    /*      > 1 hr          hrs, mins, secs */
    s = H5_timer_get_time_string(9.756e15);
    if(NULL == s || strcmp(s, "2 h 42 m 36 s") != 0)
        TEST_ERROR;
    free(s);

    PASSED();
    return 0;

error:
    free(s);
    return -1;

}




/*-------------------------------------------------------------------------
 * Function:    test_timer_system_user
 *
 * Purpose:     Tests the ability to get system and user times from the
 *              timers.
 *              Some platforms may require special code to get system and
 *              user times.  If we do not support that particular platform
 *              dependent functionality, this test is skipped.
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Dana Robinson
 *              May 2011
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_timer_system_user(void)
{
    int             i;
    char            *buf = NULL;
    H5_timer_t      timer;
    H5_timevals_t   times;
    herr_t          err;

    TESTING("system/user times");

    err = H5_timer_init(&timer);
    if(err < 0)
        TEST_ERROR;

    err = H5_timer_start(&timer);
    if(err < 0)
        TEST_ERROR;

    /* The system and user times may not be present on some systems.  They
      * will be -1.0 if they are not.
     */
    if(timer.initial.system_ps < 0.0 || timer.initial.user_ps < 0.0) {
        SKIPPED();
        printf("NOTE: No suitable way to get system/user times on this platform.\n");
        return 0;
    }

    /* Do some fake work */
    for(i=0; i < 1024; i++) {
        buf = (char *)HDmalloc(1024 * i * sizeof(char));
        free(buf);
    }

    err = H5_timer_stop(&timer);
    if(err < 0)
        TEST_ERROR;

    err = H5_timer_get_times(timer, &times);
    if(err < 0)
        TEST_ERROR;

    /* System and user times should be non-negative. */
    if(times.system_ps < 0.0 || times.user_ps < 0.0)
        TEST_ERROR;

    PASSED();
    return 0;

error:
    return -1;
}




/*-------------------------------------------------------------------------
 * Function:    test_timer_elapsed
 *
 * Purpose:     Tests the ability to get elapsed times from the timers.
 *              We should always be able to get an elapsed time,
 *              regardless of the time libraries or platform.
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Dana Robinson
 *              May 2011
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_timer_elapsed(void)
{
    int             i;
    char            *buf = NULL;
    H5_timer_t      timer;
    H5_timevals_t   times;
    herr_t          err;

    TESTING("elapsed times");

    err = H5_timer_init(&timer);
    if(err < 0)
        TEST_ERROR;

    err = H5_timer_start(&timer);
    if(err < 0)
        TEST_ERROR;

    /* Do some fake work */
    for(i=0; i < 1024; i++) {
        buf = (char *)HDmalloc(1024 * i * sizeof(char));
        free(buf);
    }

    err = H5_timer_stop(&timer);
    if(err < 0)
        TEST_ERROR;

    err = H5_timer_get_times(timer, &times);
    if(err < 0)
        TEST_ERROR;

    /* Elapsed time should be non-negative. */
    if(times.elapsed_ps < 0.0)
        TEST_ERROR;

    PASSED();
    return 0;

error:
    return -1;
}



static herr_t
test_timer_functionality(void)
{
    int             i;
    char            *buf = NULL;
    H5_timer_t      timer;
    
    H5_timevals_t   times;
    double          prev_etime;
    double          prev_total_etime;

    herr_t          err;

    TESTING("timer functionality");

    /*****************
     * CHECK STARTUP *
     *****************/

    /* Timer should be running after start */
    err = H5_timer_init(&timer);
    if(err < 0 || timer.is_running)
        TEST_ERROR;

    /* Times should be initialized to zero */
    err = H5_timer_get_times(timer, &times);
    if(err < 0 || times.elapsed_ps != 0.0)
        TEST_ERROR;

    err = H5_timer_get_total_times(timer, &times);
    if(err < 0 || times.elapsed_ps != 0.0)
        TEST_ERROR;


    /********************
     * CHECK START/STOP *
     ********************/

    /* Running state should change after start */
    err = H5_timer_start(&timer);
    if(err < 0 || !timer.is_running)
        TEST_ERROR;

    /* Do some fake work */
    for(i=0; i < 1024; i++) {
        buf = (char *)HDmalloc(1024 * i * sizeof(char));
        free(buf);
    }

    /* Running state should change after stop */
    err = H5_timer_stop(&timer);
    if(err < 0 || timer.is_running)
        TEST_ERROR;

    /* Times should be positive and non-negative */
    err = H5_timer_get_times(timer, &times);
    if(err < 0 || times.elapsed_ps < 0.0)
        TEST_ERROR;

    err = H5_timer_get_total_times(timer, &times);
    if(err < 0 || times.elapsed_ps < 0.0)
        TEST_ERROR;


    /**********************
     * CHECK INTERRUPTING *
     **********************/

    /* Timer should change stat and refresh to 0s */
    err = H5_timer_init(&timer);
    if(err < 0 || timer.is_running)
        TEST_ERROR;

    err = H5_timer_get_times(timer, &times);
    if(err < 0 || times.elapsed_ps != 0.0)
        TEST_ERROR;

    err = H5_timer_get_total_times(timer, &times);
    if(err < 0 || times.elapsed_ps != 0.0)
        TEST_ERROR;

    /* Timer state should flip */
    err = H5_timer_start(&timer);
    if(err < 0 || !timer.is_running)
        TEST_ERROR;

    /* Do some fake work */
    for(i=0; i < 1024; i++) {
        buf = (char *)HDmalloc(1024 * i * sizeof(char));
        free(buf);
    }

    /* Times should be non-negative */
    err = H5_timer_get_times(timer, &times);
    if(err < 0 || times.elapsed_ps < 0.0)
        TEST_ERROR;
    prev_etime = times.elapsed_ps;

    err = H5_timer_get_total_times(timer, &times);
    if(err < 0 || times.elapsed_ps < 0.0)
        TEST_ERROR;
    prev_total_etime = times.elapsed_ps;
    
    /* Do some fake work */
    for(i=0; i < 1024; i++) {
        buf = (char *)HDmalloc(1024 * i * sizeof(char));
        free(buf);
    }

    /* State should flip on stop */
    err = H5_timer_stop(&timer);
    if(err < 0 || timer.is_running)
        TEST_ERROR;

    /* Times should be >= than the cached intermediate times */
    err = H5_timer_get_times(timer, &times);
    if(err < 0 || times.elapsed_ps < prev_etime)
        TEST_ERROR;

    err = H5_timer_get_total_times(timer, &times);
    if(err < 0 || times.elapsed_ps < prev_total_etime)
        TEST_ERROR;


    PASSED();
    return 0;

error:
    return -1;
}




/*-------------------------------------------------------------------------
 * Function:    main
 *
 * Purpose:     Tests the basic functionality of the platform-independent
 *              timers
 *
 * Return:      Success:        0
 *              Failure:        1
 *
 * Programmer:  Dana Robinson
 *              May, 2011
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{

    int             nerrors = 0;

    h5_reset();

    printf("Testing platform-independent timer functionality.\n");

    nerrors += test_time_formatting()       < 0 ? 1 : 0;
    nerrors += test_timer_system_user()     < 0 ? 1 : 0;
    nerrors += test_timer_elapsed()         < 0 ? 1 : 0;
    nerrors += test_timer_functionality()   < 0 ? 1 : 0;
    

    if(nerrors) {
        printf("***** %d platform-independent timer TEST%s FAILED! *****\n",
        nerrors, nerrors > 1 ? "S" : "");
        return 1;
    } else {
        printf("All platform-independent timer tests passed.\n");
        return 0;
    }
}
