/* winstuff.c - windows interface routines for xlisp */
/* Written by Chris Tchou. */
/* This file contains the stuff that the other xlisp files call directly. */

#include "windows.h"
#include <stdio.h>
//#include <QuickDraw.h>	/* for Random */
#include <memory.h>		/* for DisposPtr */
#include <string.h>
//#include <SegLoad.h>	/* for ExitToShell */
#include "xlisp.h"
#include "textio.h"

const char os_pathchar = '\\';
const char os_sepchar = ',';


/* externals */
extern FILE *tfp;  /* transcript file pointer */
extern int cursorPos;
extern char *macgets (void);

#define LBSIZE 200

/* local variables */
static char lbuf[LBSIZE];
static int lpos[LBSIZE];
static int lindex;
static int lcount = 0;
static int lposition;
static int line_edit = TRUE;

//int isascii (char c) { return 1; }  /* every char is an ascii char, isn't it? */

void osinit (char *banner) {
//	int i;
    char version[] = "\nWindows console interface by Roger Dannenberg.\n";
//	InitMac ();  /* initialize the mac interface routines */
//	lposition = 0;  /* initialize the line editor */
//	for (i = 0; banner[i] != '\0'; i++) macputc (banner[i]);
//	for (i = 0; version[i] != '\0'; i++) macputc (version[i]);
    nyquist_printf(banner);
    nyquist_printf(version);
}

/* osrand - return next random number in sequence */
long osrand (long rseed) {
#ifdef OLDBUTINTERESTING
// note that this takes a seed and returns a big number,
// whereas I think XLisp's RANDOM is defined differently
    long k1;

    /* make sure we don't get stuck at zero */
    if (rseed == 0L) rseed = 1L;

    /* algorithm taken from Dr. Dobbs Journal, November 1985, page 91 */
    k1 = rseed / 127773L;
    if ((rseed = 16807L * (rseed - k1 * 127773L) - k1 * 2836L) < 0L)
    rseed += 2147483647L;

    /* return a random number between 0 and MAXFIX */
    return rseed;
#endif
    return rand() % rseed;	// rseed is a misnomer
}

FILE *osaopen (char *name, char *mode) {
    return fopen (name, mode);
}

FILE *osbopen (char *name, char *mode) {
    char nmode[4];
    strcpy (nmode, mode); strcat (nmode, "b");
    return (fopen (name, nmode));
}

int osclose (FILE *fp) { return (fclose (fp)); }
int osaputc (int ch, FILE *fp) { return (putc (ch, fp)); }
int osbputc (int ch, FILE *fp) { return (putc (ch, fp)); }

/* osagetc - get a character from an ascii file */
int osagetc(fp)
  FILE *fp;
{
    return (getc(fp));
}


extern int abort_flag;


#define OLDGETC
#ifdef OLDGETC

int ostgetc (void) {
/*	int i;

    if (numChars <= 0) {  /* get some more */
/*		if (linebuf) DisposPtr (linebuf);
        linebuf = macgets ();
        i = 0;
        while (linebuf[i] != '\0') i++;
        numChars = i;
        if (tfp) for (i = 0; i < numChars; i++) osaputc (linebuf[i], tfp);
        lineptr = linebuf;
    }
    numChars--;
    if (*lineptr == '\r') {
        lineptr++;
        return '\n';
    } else return (*lineptr++);*/

    int ch = ggetchar();
    oscheck(); /* in case user typed ^C */
    if (ch == BREAK_CHAR && abort_flag == BREAK_LEVEL) {
        abort_flag = 0;
    }
    return ch;
}

#else

void end_of_line_edit()
{
    line_edit = FALSE;
    if (tfp) {
    for (lindex = 0; lindex < lcount; ++lindex)
        osaputc(lbuf[lindex], tfp);
    }
    lindex = 0;
}



int ostgetc()
{
/*
 * NOTE: lbuf[] accumulates characters as they are typed
 *   lpos[] is the column position of the characters
 *   lcount is the number of characters in lbuf
 *   lposition is current position
 *   lindex is index of next char to output
 *   line_edit is true iff we're inputing characters
 *
 */
    int ch;

    while (line_edit) {
        ch = ggetchar();
        oscheck(); /* in case user typed ^C */
            if (ch == BREAK_CHAR && abort_flag == BREAK_LEVEL) {
                abort_flag = 0;
            }
        /* assume for now we should add the character */
        lbuf[lcount] = ch;
        lpos[lcount] = lposition;
        lcount++;
        lposition++;

        /* now do all the special character processing */
        switch (ch) {
          case '\n':
            lposition = 0;
            end_of_line_edit();
            gputchar('\r');
            gputchar(ch);
            break;
          /* delete key generates: 1b, 5b, 33, 7E
                 which is: ESC, [, 3, ~ */
          case '\010':	/* backspace */
          case '\177':	/* delete */
            lcount--; /* take out backspace or delete char */
            lposition--;
            if (lcount) {
            lcount--;
            while (lposition > lpos[lcount]) {
                      gputchar('\010');
                      gputchar(' ');
                      gputchar('\010');
                lposition--;
            }
            }
            break;
          case '\025': /* control-u */
            lcount--;
            lposition--;
            if (lcount) {
                while (lposition > lpos[0]) {
                      gputchar('\010');
                      gputchar(' ');
                      gputchar('\010');
                lposition--;
            }
            lcount = 0;
            }
            break;

              /* note that control-z never reaches here */
              case '\003':	/* control-c */
            xltoplevel();
            lcount = 0;
            break;
              case '\007':	/* control-g */
            xlcleanup();
            lcount = 0;
            break;
              case '\020':	/* control-p */
            xlcontinue();
            lcount = 0;
            break;
          case '\002':
            ostputc('\n');	/* control-b */
            xlbreak("BREAK",s_unbound);
            break;
          case '\024':	/* control-t */
            xinfo(); 
            lcount = 0;
            break;
          case '\t':	/* TAB */
            lposition--; /* undo the increment above */
            do {
            lposition++;
                    gputchar(' ');
            } while (lposition & 7);
            break;
          default:
            gputchar(ch);
            break;
        }
    }
    if (lindex + 1 >= lcount) {
        lcount = 0;
        line_edit = TRUE;
    }
    ch = lbuf[lindex++];
    /* printf("[%c]", ch); */
    fflush(stdout);
    return ch;
}
#endif


void ostputc (int ch) {
//	macputc (ch);
    gputchar(ch);			// console

    if (tfp) osaputc (ch, tfp);
}

void osflush (void) {
    lindex = lcount = lposition = 0; 
    line_edit = TRUE;
}

extern int abort_flag;

void oscheck (void) { 
    check_aborted(); 
    if (abort_flag == ABORT_LEVEL) {
        abort_flag = 0;
        osflush();
        xltoplevel();
    } else if (abort_flag == BREAK_LEVEL) {
        abort_flag = 0;
        osflush();
        xlbreak("BREAK",s_unbound);
    }
}

void oserror (char *msg) {
    char line[100], *p;
    sprintf (line,"error: %s\n",msg);
    for (p = line; *p != '\0'; ++p) ostputc (*p);
}

void osfinish (void) {
    portaudio_exit();
    /* dispose of everything... */
//	if (linebuf) DisposPtr (linebuf);
//	MacWrapUp ();
//	ExitToShell ();
}

int renamebackup (char *filename) { return 0; }


long randomseed = 1L;

long random () {
// note that this takes a seed and returns a big number,
// whereas I think XLisp's RANDOM is defined differently
    long k1;

    /* algorithm taken from Dr. Dobbs Journal, November 1985, page 91 */
    k1 = randomseed / 127773L;
    if ((randomseed = 16807L * (randomseed - k1 * 127773L) - k1 * 2836L) < 0L)
      randomseed += 2147483647L;

    /* return a random number between 0 and MAXFIX */
    return randomseed;
}

/* Added by Ning Hu		May.2001 
xsetdir - set current directory of the process */
LVAL xsetdir() {
    TCHAR ssCurDir[MAX_PATH], szCurDir[MAX_PATH];

    strcpy(ssCurDir, getstring(xlgastring()));
    xllastarg();
    if (SetCurrentDirectory(ssCurDir)) {
        if (GetCurrentDirectory(
            sizeof(szCurDir)/sizeof(TCHAR), szCurDir)) {	
        /* create the result string */
            stdputstr("Current Directory:");
            stdputstr(szCurDir);
            stdputstr("\n");
        }	
        else stdputstr("Directory Setting Error\n");
    }
    else stdputstr("Directory Setting Error\n");

    /* return the new string */
    return (NIL);
}
//Updated End
