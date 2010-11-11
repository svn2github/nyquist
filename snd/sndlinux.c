/* sndlinux.c -- implementation of system-specific function for snd library */

/* CHANGE LOG
 * --------------------------------------------------------------------
 * 28Apr03  dm  changes for portability and fix compiler warnings
 */

#include <fcntl.h>
#include <stdio.h>
#include <unistd.h>
#include <sys/stat.h>
#include <sndfile.h>

#include "snd.h"
#include "sndfileio.h"

#define PERMISSION 0644

#ifdef __cplusplus
extern "C" {
#endif

// snd_fail moved to sndfaillinux.c -RBD
/* jlh I don't think snd_file_open is used ANYWHERE. It's not getting
   put into any of the dictionaries. In fact, I don't think any of
   these are getting used anywhere... */
int snd_file_open(char *fname, int mode)
{
    int file;
    if (mode == SND_RDONLY)
      mode = O_RDONLY;
    else
      mode = O_RDWR;

    file = open(fname, mode);
    if (file < 0) file = SND_FILE_FAILURE;
    return file;
}


int snd_file_creat(char *fname)
{
    int file = creat(fname, PERMISSION);
    if (file < 0) file = SND_FILE_FAILURE;
    return file;
}


long snd_file_len(int file)
{
    long len;
    struct stat statbuf;

    fstat(file ,&statbuf);
    len = (long) statbuf.st_size;  /* get length of file */
    return len;
}

  /* This IS getting used, being called from the file_dictionary
     function file_read. */
long snd_file_read(SNDFILE *fp, float *data, long len)
{
  /* Original code 
    return read(fp, data, len);
  */
  return sf_readf_float(fp, data, len);
}


long snd_file_write(SNDFILE *fp, float *data, long len)
{
  /* Original code;
    return write(fp, data, len);
  */
  return sf_writef_float(fp, data, len);
}


long snd_file_close(SNDFILE *fp)
{
    return sf_close(fp);
}


int snd_file_lseek(int file, long offset, int param)
{
    if (param == SND_SEEK_CUR) param = SEEK_CUR;
    else if (param == SND_SEEK_SET) param = SEEK_SET;
    else param = SEEK_END;
    return lseek(file, offset, param);
}


void *snd_alloc(size_t s) { return malloc(s); }


void snd_free(void *a) { free(a); }

#ifdef __cplusplus
}
#endif

