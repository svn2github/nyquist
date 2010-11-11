/* sndheader.c -- nice new empty */

/* functions to write or delete

references from nyqsrc/sndread.c/snd_make_read()

snd_seek()
snd_bytes_per_frame()
cvt_from_8
cvt_from_16
cvt_from_24
cvt_from_32
cvt_from_unknown


references from nyqsrc/sndwritepa.c/find_cvt_to_fn

cvt_from_8
cvt_from_16
cvt_from_24
cvt_from_32
cvt_from_unknown

snd/audiooss.o: In function `audio_poll':
snd_bytes_per_frame()

snd/snd.o: In function `file_close':
write_sndheader_finish()

snd/snd.o: In function `snd_read':
snd_bytes_per_frame()

snd/snd.o: In function `snd_write':
snd_bytes_per_frame()

snd/snd.o:
snd_open_file()  in the file_dictionary snd_fns_node.


*/


/* sndheader.c -- low-level sound I/O 
 *
 *
 * Judy Hawkins
 * Feb 2008
 *
 * Rewritten to use libsndfile 1.0.17.
 *
 * Based on sndheader.c from Nyquist 3.01, which had
 * a long history courtesy of Jim Zelenka, CMU/ITC, 9 Jun 1992
 * and Roger Dannenberg, CMU, Mar 1993
 *
 *
 */

/* Standard includes */
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "snd.h" /* probably going to modify this one */

/* jlh leaving a whole bunch of includes out; as needed I'll put them
   back in. */

#ifdef LINUX
#  ifdef WIN32
#    error LINUX and WIN32 both?
#  endif
#endif

/* jlh leaving out a bunch of Macintosh specific stuff on the theory
   that libsndfile does all that now. */

/* jlh leaving out some AIFF marker and instrument structure
   definitions on the theory that if anyone is using them I'll figure
   out how to use libsndfile to access them, and write structures as
   needed to support libsndfile. */

/* jlh leaving out convert functions; libsndfile handles all that. */

/* jlh leaving out a bunch of little bitty file and data handling
   support functions; libsndfile... */

/* Local buffer for constructing useful messages */
#define STRBUFF_LEN 1024
static char strBuffer [STRBUFF_LEN] ;


/* =====
 * lifted verbatim from sndheader.c v301
 */
void snd_open_fail(snd_type snd)
{
    /* char msg[250]; */

    snd->device = SND_DEVICE_NONE;
    snd->dictionary = &snd_none_dictionary;

    /* It's not fatal if the file does not exist...
    sprintf(msg, "snd_open: could not open file %s\n",
        snd->u.file.filename);
    snd_fail(msg); */

    return;
}

/*=====================
** Convert libsndfile's minor format to nyquist's SND_MODE specifier.
*/

int get_ny_mode (SF_INFO *sfinfo)
{
  
  int mode;
  mode = 0;

  switch (sfinfo->format & SF_FORMAT_SUBMASK )
    {
    case SF_FORMAT_IMA_ADPCM:
    case SF_FORMAT_MS_ADPCM:
    case SF_FORMAT_VOX_ADPCM:
    case SF_FORMAT_G721_32:
    case SF_FORMAT_G723_24:
    case SF_FORMAT_G723_40:
      mode = SND_MODE_ADPCM;
      break;

    case SF_FORMAT_RAW:
    case SF_FORMAT_PCM_S8:
    case SF_FORMAT_PCM_16:
    case SF_FORMAT_PCM_24:
    case SF_FORMAT_PCM_32:
      
    case SF_FORMAT_DPCM_8:     /* jlh or are these two the UPCM, see below? */
    case SF_FORMAT_DPCM_16:

      mode = SND_MODE_PCM;
      break;

    case SF_FORMAT_ULAW:
      mode = SND_MODE_ULAW;
      break;

    case SF_FORMAT_ALAW:
      mode = SND_MODE_ALAW;
      break;

    case SF_FORMAT_FLOAT:	
      mode = SND_MODE_FLOAT;
      break;

      /* jlh libsndfile doesn't do UPCM? or is that the same as DPCM,
	 see above? And what about the special case in the
	 sndheader.c-orig, about how 8 bit PCM WAV files are actually
	 UPCM? JLH */

    case SF_FORMAT_PCM_U8:      /* jlh looks more like UPCM than anything else */
      mode = SND_MODE_UPCM;
      break;

    default: 
#ifdef DEBUG_LSF
      printf(" at the default case in get_ny_mode -- check mask...\n");
#endif
      mode = SND_MODE_UNKNOWN;
      break;
    }

  return mode;
}

/*==============
 * Convert the libsndfile extension to what nyquist needs.
 */

int get_ny_head ( SF_INFO *sfinfo )
{

  int head;
  head = 0;
  /* this would already have been put in by snd_open_fail if the file
     open failed: SND_HEAD_NONE */

  switch (sfinfo->format & SF_FORMAT_TYPEMASK)
    {
    case SF_FORMAT_AIFF:
      head = SND_HEAD_AIFF;
      break;
 
    case SF_FORMAT_IRCAM:
      head = SND_HEAD_IRCAM;
      break;

    case SF_FORMAT_AU:
      head = SND_HEAD_NEXT;
      break;

    case SF_FORMAT_WAV:
      head = SND_HEAD_WAVE;
      break;

    default:
      head = SND_HEAD_NONE;
      break;
    }

  return head;
}


int get_ny_bits(SF_INFO *sfinfo)
{

  int bits;
  bits = 0;

  /* does libsdnfile give me any help with this? making a lot of
     guesses I'll have to verify later... jlh*/

  switch (sfinfo->format & SF_FORMAT_SUBMASK )
    {
      /* 8 bit */
    case SF_FORMAT_PCM_S8:
    case SF_FORMAT_DPCM_8:
    case SF_FORMAT_PCM_U8:
    case SF_FORMAT_RAW:
      bits = 8;
      break;

      /* 16 bit */
    case SF_FORMAT_IMA_ADPCM:
    case SF_FORMAT_MS_ADPCM:
    case SF_FORMAT_VOX_ADPCM:
    case SF_FORMAT_PCM_16:
    case SF_FORMAT_DPCM_16:
      bits = 16;
      break;

      /* 24 bit */
    case SF_FORMAT_G723_24:
    case SF_FORMAT_PCM_24:
      bits = 24;
      break;

      /* 32 bit */
    case SF_FORMAT_G721_32:
    case SF_FORMAT_PCM_32:
      bits = 32;
      break;

      /* 40 bit */
    case SF_FORMAT_G723_40:
      bits = 40;
      break;

      /* wild guess at 4 bytes = 32 bits */
    case SF_FORMAT_ULAW:
    case SF_FORMAT_ALAW:
    case SF_FORMAT_FLOAT:	
      bits = 32;
      break;



    default: 
#ifdef DEBUG_LSF
      printf(" at the default case in get_ny_bits -- check mask...\n");
#endif
      bits = 8;
      break;
    }
  return bits;
}


/*==================
 * Fill in the SF_INFO structure for opening a file for write or read/write.
 *
 * I am attempting to make this do double duty for SND_WRITE and
 * SND_OVERWRITE. Wish me luck.
 */

int make_sfinfo(snd_type snd)
{
  int format;
  int subformat;

  subformat = format = 0;

  /* Translate nyquist/snd header, mode and bits into libsndfile values. */

  switch(snd->u.file.header)
    {
    case SND_HEAD_AIFF:
      format = SF_FORMAT_AIFF;
      break;
 
    case SND_HEAD_IRCAM:
      format = SF_FORMAT_IRCAM;
      break;

    case SND_HEAD_NEXT:
      format = SF_FORMAT_AU;
      break;

    case SND_HEAD_WAVE:
      format = SF_FORMAT_WAV;
      break;

    default:
      format = SF_FORMAT_WAV;
      break;
    }

  switch(snd->format.mode)
    {
    case SND_MODE_ADPCM:
      subformat = SF_FORMAT_IMA_ADPCM;
      break;

    case SND_MODE_PCM:
      switch(snd->format.bits)
	{
	case 8:
	  subformat = SF_FORMAT_PCM_S8;
	  break;

	case 16:
	  subformat = SF_FORMAT_PCM_16;
	  break;

	case 24:
	  subformat = SF_FORMAT_PCM_24;
	  break;

	case 32:
	  subformat = SF_FORMAT_PCM_32;
	  break;

	default:
	  subformat = SF_FORMAT_PCM_16;
	  break;
	}
      break;

    case SND_MODE_ULAW:
      subformat = SF_FORMAT_ULAW;
      break;

    case SND_MODE_ALAW:
      subformat = SF_FORMAT_ALAW;
      break;

    case SND_MODE_FLOAT:
      subformat = SF_FORMAT_FLOAT;
      break;

    case SND_MODE_UPCM:
      subformat = SF_FORMAT_PCM_U8;
      break;

    default:

      /* I think for simplicity in this here not very limited first
	 cut, I'll go for a default of WAV, 16 bit PCM. I could get a
	 lot more complicated, but that can happen later. jlh.*/

      format = SF_FORMAT_WAV;
      subformat = SF_FORMAT_PCM_16;
      break;
    }

  snd->u.file.sfinfo.format = format | subformat;

  if (snd->format.srate > 0) 
    snd->u.file.sfinfo.samplerate = snd->format.srate;
  else
    snd->u.file.sfinfo.samplerate = 41000;

  if (snd->format.channels > 0)
    snd->u.file.sfinfo.channels = snd->format.channels;
  else
    snd->u.file.sfinfo.channels = 1;
  

  snd->u.file.sfinfo.frames = 0;

  return sf_format_check(&snd->u.file.sfinfo);

 }

/*==============================================================
 * The beating heart of this libsndfile project.
 *
 *  Code from the original sndheader.c, v301, is mostly gone; I tried
 *  to keep error handling the same.
 *
 *  Bits and pieces from libsndfile examples/
 */

int snd_open_file ( snd_type snd, long *flags)
{


  SNDFILE *sffile;

  (*flags) = 0; /* No file has successfully opened; at the end, when
		   all checks have passed, flags will get set to
		   values that say the file open worked and fields
		   have valid values. */
  if (snd->write_flag == SND_READ)
    {
      snd->u.file.loop_info = FALSE;

      /* jlh -- somewhere previous to here I have to make sure format
	 is 0, except if this is a RAW file, in which case I have to
	 set samplerate, channels and format fields. */

      snd->u.file.sfinfo.format = 0;
      /* if raw... */

      if ((sffile = sf_open (snd->u.file.filename, SFM_READ, &snd->u.file.sfinfo)) == NULL)
	{
	  printf ("Error : Not able to open input file %s.\n", 
		  snd->u.file.filename) ;
	  fflush (stdout) ;
	  memset (strBuffer, 0, sizeof (strBuffer)) ;
	  sf_command (sffile, SFC_GET_LOG_INFO, strBuffer, STRBUFF_LEN) ;
	  puts (strBuffer) ;
	  puts (sf_strerror (NULL)) ;
	  snd_open_fail(snd);
	  return SND_FILE_FAILURE;
	}

      /* snd_read_header OUTTA DA WINDOW!!! (sound of splintering code
	 fragments) jlh */

      /* jlh -- instead, I have put sfinfo into snd. Now I will make
	 sure it is a good happy little structure. */

      if ((sf_format_check(&snd->u.file.sfinfo)) != TRUE)
	{
	  printf ("Error : sfinfo not ok on file open: %s.\n", 
		  snd->u.file.filename) ;
	  fflush (stdout) ;
	  memset (strBuffer, 0, sizeof (strBuffer)) ;
	  sf_command (sffile, SFC_GET_LOG_INFO, strBuffer, STRBUFF_LEN) ;
	  puts (strBuffer) ;
	  puts (sf_strerror (NULL)) ;
	  snd_open_fail(snd);
	  return SND_FILE_FAILURE;
	}

      /* YEAH.  cooking with hydrogen. */

      /* Fill out the nyquist format structure. */
      snd->format.mode = get_ny_mode(&snd->u.file.sfinfo);
      snd->format.channels = snd->u.file.sfinfo.channels;
      snd->format.srate = snd->u.file.sfinfo.samplerate;
      snd->format.bits = get_ny_bits(&snd->u.file.sfinfo);

      /* nyquist header value, used in lisp code (I believe. jlh )*/
      snd->u.file.header = get_ny_head(&snd->u.file.sfinfo);
      
    } /*  if (snd->write_flag == SND_READ) */

  else if (snd->write_flag == SND_WRITE)
    {
      if (snd->u.file.header < 0 || snd->u.file.header >= SND_NUM_HEADS)
	{
	  sprintf(strBuffer, 
		  "snd_open_file: can't write file %s: invalid snd header value\n", 
		  snd->u.file.filename);
	  snd_fail (strBuffer);
	  snd_open_fail(snd);
	  return !SND_SUCCESS;
	}
      
      /* Writing a new file: clear the libsndfile structures for action */
      memset (&snd->u.file.sfinfo, 0, sizeof(snd->u.file.sfinfo));
      snd->u.file.sffile = NULL;

      if ( make_sfinfo(snd) == SF_FALSE )
	{
	  sprintf(strBuffer,
		  "snd_open_file: Header information invalid for writing %s\n",
		  snd->u.file.filename);
	  snd_fail (strBuffer);
	  snd_open_fail(snd);
	  return !SND_SUCCESS;
	}

      /* Headers and info structures all filled out; now create the
	 file */

      if ((sffile = sf_open (snd->u.file.filename, 
			     SFM_WRITE, 
			     &snd->u.file.sfinfo)) == NULL)
	{
	  sprintf(strBuffer,
		  "snd_open_file: failed to open \"%s\" for write;\nLIBSNDFILE error: %s\n",
		  snd->u.file.filename,
		  sf_strerror(sffile));
	  snd_fail (strBuffer);
	  snd_open_fail(snd);
	  return !SND_SUCCESS;
	}

      snd->u.file.file = 0; /* Ok, this is going to make for noise,
			       but I do want to know when .file is
			       being accessed so I can fix it. jlh */
      snd->u.file.sffile = sffile; /* prob redundant... but I may
				      get rid of file.file. jlh */

      /* JLH gee... is this all I need to do? that was easy. */

    } /* if snd->write_flag == SND_WRITE */

  else if (snd->write_flag == SND_OVERWRITE)
    {
      
      if (snd->u.file.header < 0 || snd->u.file.header >= SND_NUM_HEADS)
	{
	  sprintf(strBuffer, 
		  "snd_open_file: can't write file %s: invalid snd header value\n", 
		  snd->u.file.filename);
	  snd_fail (strBuffer);
	  snd_open_fail(snd);
	  return !SND_SUCCESS;
	}
      
      /* If sfinfo is valid, keep it; if not, make a new one. */
      if (sf_format_check(&snd->u.file.sfinfo) == SF_FALSE)
	{
	  memset (&snd->u.file.sfinfo, 0, sizeof(snd->u.file.sfinfo));
	  snd->u.file.sffile = NULL;
	  
	  if ( make_sfinfo(snd) == SF_FALSE )
	    {
	      sprintf(strBuffer,
		      "snd_open_file: Header information invalid for writing %s\n",
		      snd->u.file.filename);
	      snd_fail (strBuffer);
	      snd_open_fail(snd);
	      return !SND_SUCCESS;
	    }
	}

      /* Headers and info structures all filled out; now create the
	 file */

      if ((sffile = sf_open (snd->u.file.filename, 
			     SFM_RDWR, 
			     &snd->u.file.sfinfo)) == NULL)
	{
	  sprintf(strBuffer,
		  "snd_open_file: failed to open \"%s\" for read/write;\nLIBSNDFILE error: %s\n",
		  snd->u.file.filename,
		  sf_strerror(sffile));
	  snd_fail (strBuffer);
	  snd_open_fail(snd);
	  return !SND_SUCCESS;
	}

      /* Don't need to find the beginning of the data, libsndfile
	 handles that. */
    }
  /* JLH I'm not doing anything with swap, I'm assuming libsndfile
     handles all that. */

  snd->u.file.file = 0; /* jlh and fix problems as they occur */
  snd->u.file.sffile = sffile; /* prob redundant... but I may
				  get rid of file.file. jlh */

  /* jlh The file must have opened successfully; make flags reflect that,
     using old nyquist/snd values; lots of things use these??? */
  (*flags) = SND_HEAD_SRATE | SND_HEAD_CHANNELS | SND_HEAD_BITS | SND_HEAD_MODE | SND_HEAD_LEN;

  return SND_SUCCESS;
}

/*==================
 * used once, in sndread.c
 *
 */
/* FIX -- need to test this. I think offset is in seconds, so this
   function is not going to be compatible with previous one. */
 int snd_seek(snd_type snd, double offset)
 {
   sf_count_t frames;
   sf_count_t tsk;
   int whence;

   frames = offset;  /* Once again, the question of what units offset
			is in.... I'm going to assume frames until
			proven wrong. jlh */

   whence = (int)snd->u.file.current_offset; /* jlh and what if.... */

   tsk = sf_seek( snd->u.file.sffile, frames, whence);

   if (tsk < 0)
     {
       return !SND_SUCCESS;
     }
   else
     {
       snd->u.file.current_offset = (long)tsk;
     }
   return SND_SUCCESS; 
 }
