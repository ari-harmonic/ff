#ifndef FF_CONFIG_H
#define FF_CONFIG_H


/** enable os buffering in file mapping */
#define FF_USE_BUFFERING

/** maximum dimensions supported (as of a quick hack in r_api.c) */
#define FFM_MAX_DIMS 8

#if defined __WIN32 || defined _WIN32

#define FF_USE_WIN32

/** force minimum requirement Windows 2000 Professional
    as we need GetFileSizeEx instead of GetFileSize to determine
    correct file sizes. 
 */
#define _WIN32_WINNT 0x0500

#ifndef FF_SECTION_SIZE
#define FF_SECTION_SIZE (64*1024)
#endif

#else /* on unix alike */

#include "ac_config.h"

#define FF_USE_MMAP
#define FF_USE_STATFS

#ifndef FF_SECTION_SIZE
#define FF_SECTION_SIZE (4*1024)
#endif

#endif

#endif /* FF_CONFIG_H */

