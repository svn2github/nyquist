This documentation describes Intgen, a program for generating XLISP to C
interfaces.  Intgen works by scanning @code(.h) files with special comments in
them.  Intgen builds stubs that implement XLISP SUBR's.  When the SUBR is
called, arguments are type-checked and passed to the C routine declared in
the @code(.h) file.  Results are converted into the appropriate XLISP type and
returned to the calling XLISP function.  Intgen lets you add C functions
into the XLISP environment with very little effort.

The interface generator will take as command-line input:
@begin(itemize)
the name of the @code(.c) file to generate (do not include the @code(.c) extension; e.g. write
@code(xlexten), not @code(xlexten.c));

a list of @code(.h) files.
@end(itemize)
Alternatively, the command line may specify a command file from which to read file names. The command file name should be preceded by "@@", for example:
@begin(example)
intgen @@sndfns.cl
@end(example)
reads sndfns.cl to get the command-line input.  Only one level of indirection is allowed.

The output is:
@begin(itemize)
a single @code(.c) file with one SUBR defined for each designated
routine in a @code(.h) file. 

a @code(.h) file that declares each new C routine.  E.g. if the @code(.c) file is named @code(xlexten.c), this file will be named @code(xlextendefs.h);

a @code(.h) file that extends the SUBR table used by Xlisp.  E.g. if the @code(.c) file is named @code(xlexten.c), then this file is named @code(xlextenptrs.h);

a @code(.lsp) file with lisp initialization expressions copied from the
@code(.h)
files.  This file is only generated if at least one initialization expression is encountered.
@end(itemize)

For example, the command line
@begin(example)
intgen seint ~setypes.h access.h
@end(example)
generates the file @code(seint.c), using declarations in @code(setypes.h)
and @code(access.h).  Normally, the @code(.h) files are included by the
generated file using @code(#include) commands.  A @code(~) before a file
means do not include the @code(.h) file.  (This may be useful if you extend
@code(xlisp.h), which will be included anyway).  Also generated will be
@code(setintdefs.h) and @code(seintptrs.h).

@subsection(Extending Xlisp)@index(Extending Xlisp)
Any number of @code(.h) files may be named on the command line to Intgen,
and Intgen will make a single @code(.c) file with interface routines for all
of the @code(.h) files.  On the other hand, it is not necessary to put all
of the extensions to Xlisp into a single interface file.  For example, you
can run Intgen once to build interfaces to window manager routines, and
again to build interfaces to a new data type.  Both interfaces can be linked
into Xlisp.

To use the generated files, you must compile the @code(.c) files and link
them with all of the standard Xlisp object files.  In addition, you must
edit the file @code(localdefs.h) to contain an @code(#include) for each
@code(*defs.h) file, and edit the file @code(localptrs.h) to include each
@code(*ptrs.h) file.  For example, suppose you run Intgen to build
@code(soundint.c), @code(fugueint.c), and @code(tableint.c).  You would then
edit @code(localdefs.h) to contain the following:
@begin(example)
#include "soundintdefs.h"
#include "fugueintdefs.h"
#include "tableintdefs.h"
@end(example)
and edit @code(localptrs.h) to contain:
@begin(example)
#include "soundintptrs.h"
#include "fugueintptrs.h"
#include "tableintptrs.h"
@end(example)
These @code(localdefs.h) and @code(localptrs.h) files are in turn included
by @code(xlftab.c) which is where Xlisp builds a table of SUBRs.

To summarize, building an interface requires just a few simple steps:
@begin(itemize)
Write C code to be called by Xlisp interface routines.  This C code does the
real work, and in most cases is completely independent of Xlisp.

Add comments to @code(.h) files to tell Intgen which routines to build
interfaces to, and to specify the types of the arguments.

Run Intgen to build interface routines.

Edit @code(localptrs.h) and @code(localdefs.h) to include generated
@code(.h) files.

Compile and link Xlisp, including the new C code.
@end(itemize)

@section(Header file format)@index(Header file format)

Each routine to be interfaced with Xlisp must be declared as
follows:
@begin(example)
@i(type-name) @i(routine-name)(); /* LISP: (@i(func-name) @i(type@-(1)) @i(type@-(2)) ...) */
@end(example)
The comment may be on the line following the declaration, but the
declaration and the comment must each be on no more than one line.
The characters @code(LISP:) at the beginning of the comment mark routines
to put in the interface.  The comment also gives the
type and number of arguments.  The function, when accessed from lisp will
be known as @I(func-name), which need not bear any relationship to
@i(routine-name).  By convention, underscores in the C @i(routine-name)
should be converted to dashes in @i(func-name), and @i(func-name) should be in
all capitals.  None of this is enforced or automated though.

Legal type_names are:
@begin(description) 
@code(LVAL)@\returns an Xlisp datum.

@code(atom_type)@\equivalent to @code(LVAL), but the result is expected to
be an atom.

@code(value_type)@\a value as used in Dannenberg's score editor.

@code(event_type)@\an event as used in Dannenberg's score editor.

@code(int)@\interface will convert int to Xlisp @code(FIXNUM).

@code(boolean)@\interface will convert int to @code( T) or @code(nil).

@code(float) or @code(double)@\interface converts to @code(FLONUM).

@code(char *) or @code(string) or @code(string_type)@\interface converts to @code(STRING).  The result string will be copied into the XLISP heap.

	void@\interface will return @code(nil).
@end(description)

It is easy to extend this list.  Any unrecognized type will 
be coerced to an @code(int) and then returned as a @code(FIXNUM), and a warning will be
issued.

The ``@code(*)'' after char must be followed by @i(routine-name) with
no intervening space.

Parameter types may be any of the following:
@begin(description)
@code(FIXNUM)@\C routine expects an int.

@code(FLONUM) or @code(FLOAT)@\C routine expects a @code(double).

@code(STRING)@\C routine expects @code(char *), the string is not copied.

@code(VALUE)@\C routine expects a @code(value_type).  (Not applicable to Fugue.)

@code(EVENT)@\C routine expects an @code(event_type).  (Not applicable to Fugue.)

@code(ANY)@\C routine expects @code(LVAL).

@code(ATOM)@\C routine expects @code(LVAL) which is a lisp atom.

@code(FILE)@\C routine expects @code(FILE *).

@code(SOUND)@\C routine expects a @code(SoundPtr).

@end(description)
Any of these may be followed by ``@code(*)'': @code(FIXNUM*), @code(FLONUM*), @code(STRING*), @code(ANY*), @code(FILE*),
indicating C routine expects @code(int *), @code(double *), @code(char **),  @code(LVAL *), or @code(FILE **) .
This is basically a mechanism for returning more than one value, @i(not)
a mechanism for clobbering XLisp values.  In this spirit, the interface
copies the value (an @code(int), @code(double), @code(char *),  @code(LVAL), or @code(FILE *)) to a local variable
and passes the address of that variable to the C routine.  On return,
a list of resulting ``@code(*)''  parameters is constructed and bound to the
global XLisp symbol @code(*RSLT*@index(*RSLT*)).  (Strings are copied.)  If the C routine is void, then the result list is also returned by the corresponding XLisp function.

Note 1: this does not support C routines like strcpy that modify strings, 
because the C routine gets a pointer to the string in the XLisp heap.
However, you can always add an intermediate routine that allocates
space and then calls @code(strcpy), or whatever.

Note 2: it follows that a new XLisp @code(STRING) will be created for each @code(STRING*) parameter.

Note 3: putting results on a (global!) symbol seems a bit unstructured, but note that one could write a multiple-value binding macro that hides this ugliness from the user if desired.  In practice, I find that pulling the extra result values from @code(*RSLT*) when needed is perfectly acceptable.

For parameters that are result values only, the character ``@code(^)'' may
be substituted for ``@code(*)''.  In this case, the parameter is @i(not) to be passed in the XLisp calling site.
However, the address of an initialized
local variable of the given type is passed to the corresponding
C function, and the resulting value is passed back through @code(*RSLT*) as
ordinary result parameter as described above.
The local variables are initialized to zero or @code(NULL).

@section(Using #define'd macros)@index(#define'd macros)
If a comment of the form:
@begin(example)
/* LISP: @i(type-name) (@i(routine-name-2) @i(type-1) @i(type-2) ...) */
@end(example)
appears on a line by itself and there was a @code(#define) on the previous
line, then the preceding @code(#define) is treated as a C routine, e.g.
@begin(example)
#define leftshift(val, count) ((val) << (count))
/* LISP: int (LOGSHIFT INT INT) */
@end(example)
will implement the LeLisp function @code(LOGSHIFT).

The @i(type-name) following ``@code(LISP:)'' should have no spaces, e.g. use @code(ANY*), not 
@code(ANY *).

@section(Lisp Include Files)@index(Lisp Include Files)
Include files often define constants that we would like to have around
in the Lisp world, but which are easier to initialize just by loading
a text file.  Therefore, a comment of the form:
@begin(example)
/* LISP-SRC: (any lisp expression) */
@end(example)
will cause Intgen to open a file @i(name)@code(.lsp) and append
@begin(example)
(any lisp expression)
@end(example)
to @i(name)@code(.lsp), where @i(name) is the interface name passed on the command line.  If none of the include files examined have comments of
this form, then no @i(name)@code(.lsp) file is generated.
@p(Note:) @i(the LISP-SRC comment must be on a new line.)

@section(Example)
This file was used for testing Intgen.  It uses a trick (ok, it's a hack) to interface
to a standard library macro (tolower).  Since tolower is already
defined, the macro ToLower is defined just to give Intgen a name
to call.  Two other routines, strlen and tough, are interfaced as
well.
@begin(example)
/* igtest.h -- test interface for intgen */

#define ToLower(c) tolower(c)
/* LISP: int (TOLOWER FIXNUM) */

int strlen();	/* LISP: (STRLEN STRING) */

void tough(); 
  /* LISP: (TOUGH FIXNUM* FLONUM* STRING ANY FIXNUM) */
@end(example)

@section(More Details)

Intgen has some compiler switches to enable/disable the use of certain types, including
@code(VALUE) and @code(EVENT) types used by Dannenberg's score editing work, the @code(SOUND) type used by Fugue, and @code(DEXT) and @code(SEXT) types added for Dale Amon.
Enabling all of these is not likely to cause problems,
and the chances of an accidental use of these types getting through
the compiler and linker seems very small.
