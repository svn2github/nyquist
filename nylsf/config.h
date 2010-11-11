/* src/config.h.  Generated from config.h.in by configure.  */
/* src/config.h.in.  Generated from configure.ac by autoheader.  */
/* modified by RBD for Nyquist. Since Nyquist has a "config" file 
 * called switches.h, we'll put the stuff that matters in there.
 *
 * Some switches seem to be global or trivial to define in a system
 * independent fashion -- those definitions are in this file.
 */

#ifndef SWITCHES
#include "switches.h"
#endif

/* Set to 1 to enable experimental code. */
#define ENABLE_EXPERIMENTAL_CODE 0

/* Define to 1 if you have the <FLAC/all.h> header file. */
#undef HAVE_FLAC_ALL_H

/* Name of package */
#define PACKAGE "libsndfile"

/* Define to the full name of this package. */
#define PACKAGE_NAME "libsndfile"

/* Define to the version of this package. */
#define PACKAGE_VERSION "1.0.17"

/* Set to maximum allowed value of sf_count_t type. */
#define SF_COUNT_MAX 0x7FFFFFFFFFFFFFFFLL

/* The size of `void*', as computed by sizeof. */
#define SIZEOF_VOIDP sizeof(void *)

/* Define to 1 if you have the ANSI C header files. */
#define STDC_HEADERS 1

/* Version number of package */
#define VERSION "1.0.17"

/* inline is only defined for C++ in Visual C++ compiler */
#ifdef WIN32
 #define inline __inline
#endif

/* libsndfile uses HAVE_STDLIB_H instead of HAS_STDLIB_H */
#define HAVE_STDLIB_H HAS_STDLIB_H
