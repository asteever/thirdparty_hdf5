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

/*-------------------------------------------------------------------------
 *
 * Created:             H5ACproto.h
 *                      Jul 10 1997
 *                      Robb Matzke <matzke@llnl.gov>
 *
 * Purpose:             Public include file for cache functions.
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
#ifndef _H5ACpublic_H
#define _H5ACpublic_H

/* Public headers needed by this file */
#include "H5public.h"
#include "H5Cpublic.h"

#ifdef __cplusplus
extern "C" {
#endif

/****************************************************************************
 *
 * structure H5AC_cache_config_t
 *
 * H5AC_cache_config_t is a public structure intended for use in public APIs.
 * At least in its initial incarnation, it is a essentially a copy of
 * struct H5C_auto_size_ctl_t, minus the report_fcn field.  This is omitted,
 * as including it would require us to make H5C_t structure public.
 *
 * The structure is in H5ACpublic.h as we may wish to allow different
 * configuration options for metadata and raw data caches.
 *
 * The fields of the structure are discussed individually below:
 *
 * version: Integer field containing the version number of this version
 *      of the H5AC_cache_config_t structure.  Any instance of
 *      H5AC_cache_config_t passed to the cache must have a known
 *      version number, or an error will be flagged.
 *
 * rpt_fcn_enabled: Boolean field used to enable and disable the default
 *	reporting function.  This function is invoked every time the 
 *	automatic cache resize code is run, and reports on its activities.
 *
 *	This is a debugging function, and should normally be turned off.
 *
 * set_initial_size: Boolean flag indicating whether the size of the
 *      initial size of the cache is to be set to the value given in
 *      the initial_size field.  If set_initial_size is FALSE, the
 *      initial_size field is ignored.
 *
 * initial_size: If enabled, this field contain the size the cache is
 *      to be set to upon receipt of this structure.  Needless to say,
 *      initial_size must lie in the closed interval [min_size, max_size].
 *
 * min_clean_fraction: double in the range 0 to 1 indicating the fraction
 *      of the cache that is to be kept clean.  This field is only used
 *      in parallel mode.  Typical values are 0.1 to 0.5.
 *
 * max_size: Maximum size to which the cache can be adjusted.  The
 *      supplied value must fall in the closed interval
 *      [MIN_MAX_CACHE_SIZE, MAX_MAX_CACHE_SIZE].  Also, max_size must
 *      be greater than or equal to min_size.
 *
 * min_size: Minimum size to which the cache can be adjusted.  The
 *      supplied value must fall in the closed interval
 *      [H5C__MIN_MAX_CACHE_SIZE, H5C__MAX_MAX_CACHE_SIZE].  Also, min_size
 *      must be less than or equal to max_size.
 *
 * epoch_length: Number of accesses on the cache over which to collect
 *      hit rate stats before running the automatic cache resize code,
 *      if it is enabled.
 *
 *      At the end of an epoch, we discard prior hit rate data and start
 *      collecting afresh.  The epoch_length must lie in the closed
 *      interval [H5C__MIN_AR_EPOCH_LENGTH, H5C__MAX_AR_EPOCH_LENGTH].
 *
 *
 * Cache size increase control fields:
 *
 * incr_mode: Instance of the H5C_cache_incr_mode enumerated type whose
 *      value indicates how we determine whether the cache size should be
 *      increased.  At present there are two possible values:
 *
 *      H5C_incr__off:  Don't attempt to increase the size of the cache
 *              automatically.
 *
 *              When this increment mode is selected, the remaining fields
 *              in the cache size increase section ar ignored.
 *
 *      H5C_incr__threshold: Attempt to increase the size of the cache
 *              whenever the average hit rate over the last epoch drops
 *              below the value supplied in the lower_hr_threshold
 *              field.
 *
 *              Note that this attempt will fail if the cache is already
 *              at its maximum size, or if the cache is not already using
 *              all available space.
 *
 * lower_hr_threshold: Lower hit rate threshold.  If the increment mode
 *      (incr_mode) is H5C_incr__threshold and the hit rate drops below the
 *      value supplied in this field in an epoch, increment the cache size by
 *      size_increment.  Note that cache size may not be incremented above
 *      max_size, and that the increment may be further restricted by the
 *      max_increment field if it is enabled.
 *
 *      When enabled, this field must contain a value in the range [0.0, 1.0].
 *      Depending on the incr_mode selected, it may also have to be less than
 *      upper_hr_threshold.
 *
 * increment:  Double containing the multiplier used to derive the new
 *      cache size from the old if a cache size increment is triggered.
 *      The increment must be greater than 1.0, and should not exceed 2.0.
 *
 *      The new cache size is obtained my multiplying the current max cache
 *      size by the increment, and then clamping to max_size and to stay
 *      within the max_increment as necessary.
 *
 * apply_max_increment:  Boolean flag indicating whether the max_increment
 *      field should be used to limit the maximum cache size increment.
 *
 * max_increment: If enabled by the apply_max_increment field described
 *      above, this field contains the maximum number of bytes by which the
 *      cache size can be increased in a single re-size.
 *
 *
 * Cache size decrease control fields:
 *
 * decr_mode: Instance of the H5C_cache_decr_mode enumerated type whose
 *      value indicates how we determine whether the cache size should be
 *      decreased.  At present there are four possibilities.
 *
 *      H5C_decr__off:  Don't attempt to decrease the size of the cache
 *              automatically.
 *
 *              When this increment mode is selected, the remaining fields
 *              in the cache size decrease section are ignored.
 *
 *      H5C_decr__threshold: Attempt to decrease the size of the cache
 *              whenever the average hit rate over the last epoch rises
 *              above the value supplied in the upper_hr_threshold
 *              field.
 *
 *      H5C_decr__age_out:  At the end of each epoch, search the cache for
 *              entries that have not been accessed for at least the number
 *              of epochs specified in the epochs_before_eviction field, and
 *              evict these entries.  Conceptually, the maximum cache size
 *              is then decreased to match the new actual cache size.  However,
 *              this reduction may be modified by the min_size, the
 *              max_decrement, and/or the empty_reserve.
 *
 *      H5C_decr__age_out_with_threshold:  Same as age_out, but we only
 *              attempt to reduce the cache size when the hit rate observed
 *              over the last epoch exceeds the value provided in the
 *              upper_hr_threshold field.
 *
 * upper_hr_threshold: Upper hit rate threshold.  The use of this field
 *      varies according to the current decr_mode:
 *
 *      H5C_decr__off or H5C_decr__age_out:  The value of this field is
 *              ignored.
 *
 *      H5C_decr__threshold:  If the hit rate exceeds this threshold in any
 *              epoch, attempt to decrement the cache size by size_decrement.
 *
 *              Note that cache size may not be decremented below min_size.
 *
 *              Note also that if the upper_threshold is 1.0, the cache size
 *              will never be reduced.
 *
 *      H5C_decr__age_out_with_threshold:  If the hit rate exceeds this
 *              threshold in any epoch, attempt to reduce the cache size
 *              by evicting entries that have not been accessed for more
 *              than the specified number of epochs.
 *
 * decrement: This field is only used when the decr_mode is
 *      H5C_decr__threshold.
 *
 *      The field is a double containing the multiplier used to derive the
 *      new cache size from the old if a cache size decrement is triggered.
 *      The decrement must be in the range 0.0 (in which case the cache will
 *      try to contract to its minimum size) to 1.0 (in which case the
 *      cache will never shrink).
 *
 * apply_max_decrement:  Boolean flag used to determine whether decrements
 *      in cache size are to be limited by the max_decrement field.
 *
 * max_decrement: Maximum number of bytes by which the cache size can be
 *      decreased in a single re-size.  Note that decrements may also be
 *      restricted by the min_size of the cache, and (in age out modes) by
 *      the empty_reserve field.
 *
 * epochs_before_eviction:  Integer field used in H5C_decr__age_out and
 *      H5C_decr__age_out_with_threshold decrement modes.
 *
 *      This field contains the number of epochs an entry must remain
 *      unaccessed before it is evicted in an attempt to reduce the
 *      cache size.  If applicable, this field must lie in the range
 *      [1, H5C__MAX_EPOCH_MARKERS].
 *
 * apply_empty_reserve:  Boolean field controlling whether the empty_reserve
 *      field is to be used in computing the new cache size when the
 *      decr_mode is H5C_decr__age_out or H5C_decr__age_out_with_threshold.
 *
 * empty_reserve:  To avoid a constant racheting down of cache size by small
 *      amounts in the H5C_decr__age_out and H5C_decr__age_out_with_threshold
 *      modes, this field allows one to require that any cache size
 *      reductions leave the specified fraction of unused space in the cache.
 *
 *      The value of this field must be in the range [0.0, 1.0].  I would
 *      expect typical values to be in the range of 0.01 to 0.1.
 *
 ****************************************************************************/

#define H5AC__CURR_CACHE_CONFIG_VERSION  1

typedef struct H5AC_cache_config_t
{
    /* general configuration fields: */
    int32_t                     version;

    hbool_t			rpt_fcn_enabled;

    hbool_t                     set_initial_size;
    size_t                      initial_size;

    double                      min_clean_fraction;

    size_t                      max_size;
    size_t                      min_size;

    int64_t                     epoch_length;


    /* size increase control fields: */
    enum H5C_cache_incr_mode    incr_mode;

    double                      lower_hr_threshold;

    double                      increment;

    hbool_t                     apply_max_increment;
    size_t                      max_increment;


    /* size decrease control fields: */
    enum H5C_cache_decr_mode    decr_mode;

    double                      upper_hr_threshold;

    double                      decrement;

    hbool_t                     apply_max_decrement;
    size_t                      max_decrement;

    int32_t                     epochs_before_eviction;

    hbool_t                     apply_empty_reserve;
    double                      empty_reserve;

} H5AC_cache_config_t;


#ifdef __cplusplus
}
#endif
#endif
