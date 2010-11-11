#include <stdio.h>

#include "snd.h"
#include "sndfileio.h"

#ifdef __WXMAC__
#include "wx/filefn.h"
#endif

/* FIX -- take out all this because by using libsndfile, file I/O should be
   machine independent
 */
int snd_file_open(char *fname, int mode)
{
    // this function should work like open(), so it returns -1 on failure. 
    // We're using fopen() on the mac, so we need to convert NULL to -1
    // Note that -1 is never a valid FILE* because it would be an odd address
    int file;
#ifdef __WXMAC__
    file = (int)fopen(wxUnix2MacFilename( fname ), mode==SND_RDONLY? "rb" : "r+b");
#else
    file = (int)fopen(fname, mode==SND_RDONLY? "rb" : "r+b");
#endif
    return (file == NULL ? -1 : file);
}


int snd_file_creat(char *fname)
{
    int file;
#ifdef __WXMAC__
    file = (int) fopen(wxUnix2MacFilename( fname ), "wb");
#else
    file = (int) fopen(fname, "wb");
#endif
    /* file is zero if there was an error opening the file */
    if (file == 0) file = SND_FILE_FAILURE;
    return file;
}


long snd_file_len(int file)
{
  FILE *fp = (FILE *)file;
  
  long save = ftell(fp);
  long len;
  fseek(fp, 0, SEEK_END);
  len = ftell(fp);
  fseek(fp, save, SEEK_SET);
  
  return len;
}


long snd_file_read(SNDFILE *fp, float *data, long len)
{
  /* Original code 
    return fread(data, 1, len, (FILE *)fp);
  */
  return sf_readf_float(fp, data, len);
}


long snd_file_write(SNDFILE *fp, float *data, long len)
{
  /*
    return fwrite(data, 1, len, (FILE *)fp);
  */
  return sf_writef_float(fp, data, len);
}


long snd_file_close(SNDFILE *fp)
{
/*
    return fclose((FILE *)fp);
     */
    return sf_close(fp);
}

int snd_file_lseek(int file, long offset, int param)
{
    if (param == SND_SEEK_CUR) param = SEEK_CUR;
    else if (param == SND_SEEK_SET) param = SEEK_SET;
    else param = SEEK_END;
    
    fseek((FILE *)file, offset, param);
    
    return ftell((FILE *)file);
}

void *snd_alloc(size_t s) { return malloc(s); }

void snd_free(void *a) { free(a); }
