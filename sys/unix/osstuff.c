/* unixtuff.c - unix interface routines for xlisp

 * HISTORY
 * 28-Apr-03	Mazzoni
 *  many changes for new conditional compilation organization
 *
 * 28-Jun-95	Dannenberg
 *	removed buffering (which could overflow) from ostgetc.
 *
 * 2-Aprl-88	Dale Amon at CMU-CSD
 *	Upgraded to xlisp 2.0. Used msstuff.c as a template.
 *
 * 20-Apr-87	Dale Amon at CMU-CSD
 *	Added control-c interrupt handler. Puts user in breakloop and allows
 *	continue. Prints line at which the interrupt occured. Interrupt
 *	occurs at first eval after ^C has been typed.
 *
 * 19-APR-87	Dale Amon at CMU-CSD
 *	switched from rand to random package. Corrected bug in osrand(). It
 *	did not use the argument n to calculate a rand in range 0 to n-1 as
 *	advertised.
 */

#include "switches.h"
#include <errno.h>

#include <stdio.h>
#include <string.h>
#include <unistd.h>

#include "xlisp.h"
#include "term.h"
#include "cext.h"

#define LBSIZE 200

/* external variables */
extern LVAL s_unbound,s_true;
extern FILE *tfp;

/* local variables */
static int lindex;
static int lcount = 0;
static int lposition;
static int line_edit = TRUE;

#ifndef READ_LINE
#define typeahead_max 128
static char typeahead[typeahead_max];
static int typeahead_tail = 0;
static int typeahead_head = 0;
static char lbuf[LBSIZE];
static int lpos[LBSIZE];
#endif

/* forward declarations */
FORWARD LOCAL void xflush();
FORWARD LOCAL int xcheck();

void term_character(void);
int term_testchar();

/*==========================================================================*/
/* control-c interrupt handling routines and variables. Uses B4.2 signal
   handling. Previous SIGINT handler is saved just in case someday we want
   to play with turning control c on and off.
*/

#include	<signal.h>

static int		ctc = FALSE;
static void control_c(int x)	{ctc = TRUE;}
void ctcinit()	{signal ( SIGINT, control_c );}
static void ctcreset()	{signal ( SIGINT, control_c );}


/*==========================================================================*/


const char os_pathchar = '/';
const char os_sepchar = ':';


/* osinit - initialize */
void osinit(char *banner)
{	printf("%s\n",banner);

    /* start the random number generator. Older version was srand(1)
       seed of 1 makes the sequence repeatable. Random gives better
       pseudo randomness than does rand().
    */
#if USE_RAND
    srand(1);
#endif

#if USE_RANDOM
    srandom(1);
#endif

#ifndef UNIX
    /* set control c trap to local routine */
    ctcinit();
#else
    /* sets terminal for raw input and calls ctcinit too */
    term_init();
    term_character();
#endif

    lposition = 0;
    lindex = 0;
    lcount = 0;
}

/* osfinish - clean up before returning to the operating system */
void osfinish(void) 
{
    term_exit();
    portaudio_exit();
}

/* oserror - print an error message */
void oserror(char *msg) {printf("error: %s\n",msg);}

#ifdef USE_RAND
long osrand(long n) {return (((int) rand()) % n);}
#endif

#ifdef USE_RANDOM
long osrand(long n) {return (((long) random()) % n);}
#endif

/* osaopen - open an ascii file */
FILE *osaopen(name,mode) char *name,*mode; {
    FILE *fp;
    fp = fopen(name,mode);
    return fp;
}

/* osbopen - open a binary file */
FILE *osbopen(name,mode) char *name,*mode;
 {  char bmode[10];
    FILE *fp;
    strcpy(bmode,mode); strcat(bmode,"b");
    fp = fopen(name,bmode);
    return fp;
 }

/* osclose - close a file */
int osclose(fp) FILE *fp; {
    return (fclose(fp));}

/* osagetc - get a character from an ascii file */
int osagetc(fp) FILE *fp; {return (getc(fp));}

/* osaputc - put a character to an ascii file */
int osaputc(ch,fp) int ch; FILE *fp; {return (putc(ch,fp));}

extern int dbgflg;

/* osbgetc - get a character from a binary file */
/* int osbgetc(fp) FILE *fp; {return (getc(fp));} */
int osbgetc(fp) FILE *fp; {int c; c = (getc(fp));
/*	if (dbgflg) printf("osbgetc: got %d from FILE %x\n", c, fp);
 *	return c;
 */
}

/* osbputc - put a character to a binary file */
int osbputc(ch,fp) int ch; FILE *fp; {return (putc(ch,fp));}

#ifdef OLDEST_OSTGETC
/* ostgetc - get a character from the terminal */
int ostgetc()
{
    int ch;
    switch (ch = getchar()) {
    case '\n':
        lbuf[lcount++] = '\n';
        lposition = 0;
        if (tfp)
            for (lindex = 0; lindex < lcount; ++lindex)
            osaputc(lbuf[lindex],tfp);
        lindex = 0; lcount = 0;
        return (ch);
    case '\010':
    case '\177':
        if (lcount) {
            lcount--;
            while (lposition > lpos[lcount]) {
            lposition--;
            }
        }
        break;
    case '\032':
        xflush();
        return (EOF);
    default:
        if (ch == '\t' || (ch >= 0x20 && ch < 0x7F)) {
            lbuf[lcount] = ch;
            lpos[lcount] = lposition;
            if (ch == '\t')
            do {} while (++lposition & 7);
            else {lposition++;}
            lcount++;
            return (ch);
        }
        else {
            xflush();
            switch (ch) {
            case '\003':	xltoplevel();	/* control-c */
            case '\007':	xlcleanup();	/* control-g */
            case '\020':	xlcontinue();	/* control-p */
            case '\032':	return (EOF);	/* control-z */

            /* moved from oscheck until I figure out how to
               set up interrupt to handle these two */
            case '\002':	xflush(); xlbreak("BREAK",s_unbound);
                    break;		/* control-b */
            case '\024':	xinfo(); break;	/* control-t */

            default:		return (ch);
            }
        }
    }
}
#else
#if OLD_OSTGETC
/* ostgetc - get a character from the terminal */
int ostgetc()
{   int ch;

    for (;;) {
    ch = getchar();
    oscheck();
    switch (ch) {
      case '\003':	xltoplevel();	/* control-c */
      case '\007':	xlcleanup();	/* control-g */
      case '\020':	xlcontinue();	/* control-p */
      case '\032':	return EOF;	/* control-z */
      case '\002':	xflush(); xlbreak("BREAK",s_unbound);
            break;		/* control-b */
      case '\024':	xinfo(); break;	/* control-t */
      case '\t':
      case '\n':
      default:
        if (tfp) osaputc(ch, tfp);
        return ch;
    }
    }
}
#else
#ifdef READLINE

#include <readline/readline.h>
#include <readline/history.h>

char *readline_line = NULL;
int readline_pos = 0;
int readline_len = 0;
int readline_first = 1;

extern int xldebug;

int ostgetc()
{
   int rval;

   if (readline_first)
      using_history();

   if (!readline_line) {
      char prompt[10];
      if (xldebug==0)
         sprintf(prompt, "> ");
      else
         sprintf(prompt, "%d> ", xldebug);
      readline_line = readline(prompt);
      if (readline_line == NULL)
         return EOF;
      add_history(readline_line);
      readline_len = strlen(readline_line);
      readline_pos = 0;
   }

   rval = readline_line[readline_pos];
   if (readline_pos == readline_len) {
      free(readline_line);
      readline_line = NULL;
      return '\n';
   }
   readline_pos++;
   
   return rval;
}


#else /* no readline */


void end_of_line_edit()
{
    line_edit = FALSE;
    if (tfp) {
    for (lindex = 0; lindex < lcount; ++lindex)
        osaputc(lbuf[lindex], tfp);
    }
    lindex = 0;
}

/* THIS IS THE "REAL" ostgetc(): */
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
        if (typeahead_tail != typeahead_head) {
            ch = typeahead[typeahead_head++];
            typeahead_head &= (typeahead_max - 1);
        }
        else {
            ch = getchar();
        }
        oscheck(); /* in case user typed ^C */
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
           osaputc('\r', stdout);
           osaputc(ch, stdout);
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
                 putchar('\010');
                 putchar(' ');
                 putchar('\010');
                 lposition--;
              }
           }
           break;
        case '\025': /* control-u */
           lcount--;
           lposition--;
           if (lcount) {
              while (lposition > lpos[0]) {
                 putchar('\010');
                 putchar(' ');
                 putchar('\010');
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
           xflush();	/* control-b */
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
              osaputc(' ', stdout);
           } while (lposition & 7);
           break;
        default:
           osaputc(ch, stdout);
           break;
        }
        // avoid line buffer overflow here:
        if (lposition > LBSIZE - 10) {
           // buffer is about to overflow, so write newline and
           // feed chars to XLISP
           osaputc('\r', stdout);
           osaputc('\n', stdout);
           lposition = 0;
           end_of_line_edit();
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
#endif
#endif


/* ostputc - put a character to the terminal */
void ostputc(int ch)
 {     
    oscheck();		/* check for control characters */

    /* output the character */
    if (ch == '\n') {lposition = 0;}
    else	    {lposition++;}

    /* output the character to the transcript file */
    if (tfp) osaputc(ch,tfp);
    putchar(((char) ch));
 }

/* osflush - flush the terminal input buffer */
void osflush(void)
{
    lindex = lcount = lposition = 0; 
    line_edit = TRUE;
}

/* oscheck - check for control characters during execution */
/*
 * NOTE: to support type-ahead, unused characters are put
 * into a queue to be removed by ostgetc
 */
void oscheck(void)
{
    int ch;

    if (ctc) { /* control-c */
        /* printf("[oscheck: control-c detected]"); */
        ctc=FALSE; ctcreset();
        xflush(); xltoplevel(); return;
    } 

    if ((ch = xcheck()))
    switch (ch) {
      case '\002':	xflush(); xlbreak("BREAK",s_unbound);
            break;			/* control-b */
      case '\024':	xinfo(); break;		/* control-t */
      default:
         #ifndef READ_LINE
         typeahead[typeahead_tail++] = ch;
         typeahead_tail &= (typeahead_max - 1);
         #endif
         break;
    }
}

/* xflush - flush the input line buffer and start a new line */
LOCAL void xflush()
{
   osflush();
   ostputc('\n');
}

/* xsystem - execute a system command */
LVAL xsystem()
{   /*LVAL strval;*/
    unsigned char *cmd = NULL;

    if (moreargs())
    cmd = (unsigned char *)getstring(xlgastring());
    xllastarg();
    return (system((char *) cmd) == -1 ? cvfixnum((FIXTYPE)errno) : s_true);
}


/* xsetdir -- set current directory of the process */
LVAL xsetdir()
{
    char *dir = getstring(xlgastring());
    int result;
    xllastarg();
    result = chdir(dir);
    if (result) {
        perror("SETDIR");
    }
    return NULL;
}


/* xcheck -- return a character if one is present */
LOCAL int xcheck()
{
    int ch = term_testchar();
    if (ch == -2) return 0;
    return ch;
}

/* xgetkey - get a key from the keyboard */
LVAL xgetkey() {xllastarg(); return (cvfixnum((FIXTYPE)getchar()));}

/* ossymbols - enter os specific symbols */
void ossymbols(void) {}

/* xsetupconsole -- used to configure window in Win32 version */
LVAL xsetupconsole() { return NIL; }



