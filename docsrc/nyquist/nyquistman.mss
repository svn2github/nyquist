@device(mac52postscript)
@make(manual)
@libraryfile(mathematics10)
@libraryfile(picture)
@libraryfile(multilevelindex)
@style(font timesroman)
@style(fontscale 11)
@commandstring(dash @Y[M])
@commandstring(subtract @Y[N])
@commandstring(itemsep @Y[M])
@Modify(indexenv, above=2 lines, leftmargin 8, columns=3, boxed)
@define(prg=i)
@define(detail=text, size 9, spacing 1.2, indent 0)
@define(code, FaceCode T, size 11)
@comment{codef is used to define a lisp function or variable -- 
   a processor uses this to extract completion hint info for NyquistIDE.}
@define(codef, FaceCode T, size 11)
@comment{pragma(define) is used to mark a term's point of definition --
   I tried to define(defn=index), but that didn't work in Scribe,
   so the approach now is to mark definitions. In s2h, the
   define pragma sets a flag that the NEXT index term is a definition.
   The s2h.lsp processor uses this to map terms defined within codef
   (which go into the completion list) to URLs for the help system.}
@define(smallcode, FaceCode T, size 8, spacing 0.8) @comment(was 10)
@define(rbdisplay = display, group)
@define(fndefs = description, leftmargin 0.5in, indent -0.5in)
@define(fdescription = text, leftmargin +0.5in, indent -0.5in, spread 1)
@define(pdescription = text, leftmargin +0.5in, indent -0.5in, spread 0)
@define(fgroup = text, indent -0.5in, spread 0)
@define(altdef = text, leftmargin +0.0in, indent -0.5in)
@textform(html = [])
@textform(htmltitle = [])
@textform(pragma = [])
@use(bibliography = "../bib/music.bib")
@style(references ClosedAlphabetic)
@counter(dummy-counter)
@modify(FigureCounter,Within=dummy-counter,
        Numbered <@@b[Figure@@ @1:@@ @@ ]@@$>,
        Referenced "@1",Init 0)
@modify(TableCounter,Within=dummy-counter,
        Numbered <@@b[Table@@ @1:@@ @@ ]@@$>,
        Referenced "@1",Init 0)
@pageheading(left "", center "@value(page)", right "")
@include(../template/filcap.mss)

@begin(comment)
@begin(figure)
@blankspace(0.3 inches) @comment(scribe doesn't leave enough space)
@center(@graphic((height = *** in, width = *** in, magnify = ***,
		postscript = "***.ps"))
@fillcaption(***)
@tag(***)
@end(figure)
@end(comment)

@html(<head><title>Nyquist Reference Manual</title></head><body>)
@htmltitle(Nyquist Reference Manual)
@begin(titlepage)
@begin(titlebox)
@blankspace(0.5 inch)
@majorheading(Nyquist Reference Manual)
@b(Version 3.05)
@blankspace(0.3 inch)
@b(Copyright 2011 by Roger B. Dannenberg)
@value(date)
@end(titlebox)
@pragma(startscribe)
@center(Carnegie Mellon University)
@center(School of Computer Science)
@center(Pittsburgh, PA 15213, U.S.A.)
@pragma(endscribe)
@html(<blockquote>Carnegie Mellon University<br>
School of Computer Science<br>
Pittsburgh, PA 15213, U.S.A.</blockquote>)
@blankspace(1 inch)
@end(titlepage)
@pragma(startscribe)
.
@pragma(endscribe)
@newpage
@pageheading(even, left "Page @Value(Page)", right "@c(Nyquist Manual)")
@pageheading(odd, left "@c[@title(chapter)]", right "Page @Value(page)")
@style(pagenumbers="@i")
@set(page=3)
@Unnumbered(Preface)
This manual is a guide for users of Nyquist, a language for composition and
sound synthesis.  Nyquist grew out of a series of research projects, notably
the languages Arctic and Canon.  Along with Nyquist, these languages promote a
functional style of programming and incorporate time into the language
semantics.

Please help by noting any errors@Index(errors), omissions@Index(omissions),
or suggestions@Index(suggestions) you may have.  You can send your
suggestions to Dannenberg@@CS.CMU.EDU (internet) via computer mail, or by
campus mail to Roger B. Dannenberg, School of Computer Science, or by
ordinary mail to Roger B. Dannenberg, School of Computer Science, Carnegie
Mellon University, 5000 Forbes Ave., Pittsburgh, PA 15213-3890, USA.  

Nyquist is a successor to Fugue, a language originally implemented by Chris
Fraley, and extended by George Polly and Roger Dannenberg.  Peter Velikonja
and Dean Rubine were early users, and they proved the value as well as
discovered some early problems of the system.  This led to Nyquist, a
reimplementation of Fugue by Roger Dannenberg with help from Joe Newcomer
and Cliff Mercer. Ning Hu ported Zheng (Geoffrey) Hua and Jim Beauchamp's
piano synthesizer to Nyquist and also built NyqIDE, the Nyquist Integrated
Development Environment for Windows. Dave Mowatt contributed the original
version of NyquistIDE, the cross-platform interactive development environment.
Dominic Mazzoni made a special version of Nyquist that runs 
within the Audacity audio editor, giving Nyquist a new interface and 
introducing Nyquist to many new users.

Many others have since contributed to Nyquist. 
Chris Tchou and Morgan Green worked on the Windows port. Eli Brandt contributed
a number of filters and other synthesis functions. Pedro J. Morales, 
Eduardo Reck Miranda, Ann Lewis, and Erich Neuwirth have all contributed
nyquist examples found in the demos folder of the Nyquist distribution.
Philip Yam ported some 
synthesis functions from Perry Cook and Gary Scavone's STK to Nyquist.
Pedro Morales ported many more STK instruments to Nyquist.
Dave Borel wrote the Dolby Pro-Logic encoding library and Adam Hartman wrote
stereo and spatialization effects. Stephen Mangiat wrote the MiniMoog 
emulator. Phil Light recorded the drum samples and wrote drum 
machine software. The Xmusic library, particularly the pattern specification,
was inspired by Rick Taube's Common Music. The functions for generating
probability distributions were implemented by Andreas Pfenning.

Starting with Version 3, Nyquist supports a version of SAL, providing
an alternative to Lisp syntax. SAL was designed by Rick Taube, and the
SAL implementation in Nyquist is based on Taube's original implementation
as part of his Common Music system.

The current NyquistIDE includes contributions from many. Chris Yealy and 
Derek D'Souza implemented early versions of the envelope editor. Daren
Makuck and Michael Rivera wrote the original equalizer editor.
Priyanka Raghavan implemented the sound browser. Dmitry Portnoy wrote the
original "UPIC" editor.


Many others have made contributions, offered suggestions, and found bugs.
If you were expecting to find your name here, I apologize for the omission,
and please let me know.

I also wish to acknowledge support from CMU, Yamaha, and IBM for this work.
@newpage
@blankspace(5 inches)
@pragma(startscribe)
.
@pragma(endscribe)
@newpage
@set(page=1)
@style(pagenumbers="@1")

@chapter(Introduction and Overview)

Nyquist is a language for sound synthesis and music composition.  Unlike score
languages that tend to deal only with events, or signal processing languages
that tend to deal only with signals and synthesis, Nyquist handles both in a
single integrated system.  Nyquist is also flexible and easy to use because it
is based on an interactive Lisp interpreter.

With Nyquist, you can design instruments by combining functions (much as you
would using the orchestra languages of Music V, cmusic, or Csound).  You can
call upon these instruments and generate a sound just by typing a simple
expression.  You can combine simple expressions into complex ones to create
a whole composition.

Nyquist runs under Linux, Apple Mac OS X, Microsoft Windows NT,
2000, XP, and Vista, 
and it produces sound files or directly generates audio.  
Recent versions have also run on AIX, NeXT, SGI, DEC pmax, and Sun Sparc
machines. (Makefiles for many of these are included, but out-of-date).
Let me know if you have problems with 
any of these machines.

To use Nyquist, you should have a basic knowledge of Lisp.  An excellent text
by Touretzky is recommended @cite(Touretzky).  Appendix @ref(xlisp-app) is
the reference manual for XLISP, of which Nyquist is a superset. Starting with
Version 3, Nyquist supports a variant of SAL, which is also available in 
Common Music. Since there are some differences, one should generally call this
implementation ``Nyquist SAL;'' however, in this manual, I will just call it
``SAL.'' SAL offers most of the capabilities of Lisp, but it uses an Algol-like
syntax that may be more familiar to programmers with experience in Java, C,
Basic, etc.

@label(install-sec)
@section(Installation)
@index(installation)@index(configure nyquist)@index(setup nyquist)
Nyquist is a C++ program intended to run under various operating systems 
including Unix, Mac OS X, and Windows. Nyquist is based on Lisp, but it 
includes its own Lisp interpreter (a modified version of XLISP), so you 
do not need to install some other Lisp to run Nyquist.  Other 
Lisp systems are not compatible with Nyquist.

Most Nyquist users run Nyquist under the Nyquist IDE, which is written in Java 
and depends on the Java runtime system. Most systems already have Java, but if
you do not, you will need to install it. When you install the Nyquist IDE,
you will automatically get Nyquist and a set of runtime libraries.

There are generally two ways to install Nyquist:
@begin(itemize)
Get a pre-compiled version of the Nyquist IDE for Windows or Mac OS X. The 
Windows version comes packaged in an installer that installs and 
configures the Nyquist IDE. The Mac OS X version 
unpacks to a complete OS X application.

Compile from sources. There is one set of sources for Mac, Windows, and Unix.
Instructions for building applications from the sources are provided in
the files @code(sys/win/README.txt), @code(sys/mac/README.txt), and 
@code(sys/unix/README.txt).
@end(itemize)

You can download source code and precompiled versions from the Nyquist project
on SourceForge (@code(http://sourceforge.net/projects/nyquist)). The latest 
source code can be obtained via Subversion (svn) using the following:
@begin(example)
svn co https://nyquist.svn.sourceforge.net/svnroot/nyquist/trunk nyquist
@end(example)
or by checking out nyquist using a graphical interface svn client such as 
TortoiseSVN for Windows.

@begin(comment)
@subsection(Unix Installation)
For Unix systems, Nyquist is distributed as a compressed tar file named @code(nyqsrc3@i(nn).zip),
where @i(nn) is the version number (e.g. v3.01 was 
in @code(nyqsrc301.zip)).  To
install Nyquist, copy @code(nyqsrc3@i(nn).zip) to the 
directory on your machine where you would like to install Nyquist, and type:
@begin(example)
gunzip nyqsrc3@i(nn).zip
cd nyquist
ln -s sys/unix/linux/Makefile Makefile
setenv XLISPPATH `pwd`/runtime:`pwd`/lib
make
@end(example)
The first line creates a @code(nyquist) directory and some subdirectories. The
second line (cd) changes directories to the new nyquist directory. The third line makes a link from the top-level directory to the Makefile for your
system. In place of @code(linux) in @code(sys/unix/linux/Makefile), you should
substitute your system type. Current systems are @code(next), @code(pmax),
@code(rs6k), @code(sgi), @code(linux), and @code(sparc).  The @code(setenv) 
command tells Nyquist where to search for lisp files to be loaded when a file
is not found in the current directory. The @code(runtime) directory should
always be on your @code(XLISPPATH) when you run Nyquist, so you may want to
set @code(XLISPPATH) in your shell startup file, e.g.  @code(.cshrc).
Assuming the make completes successfully, you can run Nyquist as follows:
@begin(example)
./ny
@end(example)
When you get the prompt, you may begin typing expressions such as
the ones in the following ``Examples'' section.

One you establish that Nyquist (ny) is working from the command line, you should
try using NyquistIDE, the Java-based Nyquist development environment. First,
make @code(jny) executable (do this only once when you install Nyquist):
@begin(example)
chmod +x jny
@end(example)
Then try running NyquistIDE by typing:
@begin(example)
./jny
@end(example)
If the NyquistIDE window does not appear, make sure you have Java installed (if not,
you probably already encountered errors when you ran @code(make)). You can also try recompiling the Java files:
@begin(example)
cd jnyqide
javac *.java
cd ..
@end(example)

@p(Note:) With Linux and the Macintosh OS X,
 NyquistIDE defines the environment passed to Nyquist. If you
set @code(XLISPPATH)@index(XLISPPATH)@index(search path) as shown above, it 
will be passed along to Nyquist under NyquistIDE. If not,
a default XLISPPATH will have the @code(lib) and @code(runtime) directories only.
This does not apply to Windows because even though the environment is there, 
the Windows version of Nyquist reads the @code(XLISPPATH) from the Registry.

You can also specify the search path by creating the 
file @code(nyquist/xlisppath), which should have colon-separated paths
on a single line of text. This file will override the environment
variable @code(XLISPPATH).

It is good to have @code(USER) in the environment with your user ID. This string
is used to construct some file names. NyquistIDE will look for it in the environment.
You can also specify your user ID using the file @code(nyquist/user), but
if you have a shared installation of Nyquist, this will not be very useful.

@p(Note:) Nyquist looks for the file @code(init.lsp) in the current
directory.  If you look in the @code(init.lsp) in @code(runtime), you
will notice two things.  First, @code(init.lsp) loads @code(nyquist.lsp)
from the Nyquist directory, and second, @code(init.lsp) loads @code(system.lsp)
which in turn defines the macro
@code(play).  You may have to modify @code(system.lsp) to invoke
the right programs on your machine.

@subsection(Win32 Installation)
The Win32 version of Nyquist is packaged as a compiled (runtime) system in an
 executable installer. A source version is also available (the same source
download is for Win32, Mac OS X, and Linux). The source version is
intended for developers who
want to recompile Nyquist. 
The contents of the source archive are extracted to the @code(C:\nyquist) 
directory,
but you can put it anywhere you like. You can then open the workspace file,
nyquist.sln, using Microsoft
Visual C++. You can build and run the command line version of Nyquist
from within Visual C++. There is a batch file, @code(comp-ide.bat), for 
bulding the Nyquist IDE. This requires the Java SDK from Sun Microsystems.

The runtime version contain everything you need to run Nyquist, including the executable,
examples, and documentation, packaged as an executable installer program.
After executing the installer, just find Nyquist in your Start menu to run it. 
You may begin typing expressions such as the ones in the following ``Examples'' section.

@p(Optional:)@index(Registry)@index(xlisppath)@index(search path) Nyquist needs to know where to find the standard runtime files. The location of runtime files must be stored in the Registry.
The installers create a registry entry, but if
you move Nyquist or deal with different versions, you can edit the Registry manually as follows:
@begin(itemize)
Run the Registry editor. Under Windows NT, run @code(C:\WINNT\system32\regedt32.exe). Under Windows95, run @code(C:\WINDOWS\regedit.exe).

Find and highlight the @code(SOFTWARE) key under @code(HKEY_LOCAL_MACHINE).

Choose @code(Add key ...) from the @code(Edit) menu, type @code(CMU), and click the @code(OK) button.

Highlight the new @code(CMU) key.

Choose @code(Add key ...) from the @code(Edit) menu, type @code(Nyquist), and click the @code(OK) button.  (Note that @code(CMU) and @code(Nyquist) are case sensitive.)

Highlight the new @code(Nyquist) key.

Choose @code(Add value ...) from the @code(Edit) menu, type @code(XLISPPATH), and click the @code(OK) button.  (Under WinXP the menu item is @code(Edit:New:String Value), after which you need to select the new string name that appears in the right panel, select @code(Edit:Rename), and type @code(XLISPPATH).)

In the String Edit box (select the @code(Edit:Modify) menu item in WinXP),
type a list of paths you want Nyquist to search for lisp files. For example, if you installed Nyquist as @code(C:\nyquist), then type:
@begin(example)
C:\nyquist\runtime,C:\nyquist\lib
@end(example)
The paths should be separated by a comma or semicolon and no space. The @code(runtime) path is essential, and the @code(lib) path may become essential in a future release. You can also add paths to personal libraries of Lisp and Nyquist code.

Click the @code(OK) button of the string box and exit from the Registry Editor application.
@end(itemize)

@paragraph(What if Nyquist functions are undefined?)
If you do not have administrative privileges for your machine, the installer may fail to set up the Registry entry that Nyquist uses to find initialization files. In this case, Nyquist will run a lisp interpreter, but many Nyquist functions will not be defined. If you can log in as administrator, do it and reinstall Nyquist. If you do not have permission, you can still run Nyquist as follows:

Create a file named @code(init.lsp) in the same directory as Nyquist.exe (the default location is @code(C:\Program Files\Nyquist), but you may have installed it in some other location.) Put the following text in @code(init.lsp):
@begin(example)
@begin(smallcode)
(setf *search-path* "C:/Program Files/Nyquist/runtime,C:/Program Files/Nyquist/lib")
(load "C:/Program Files/Nyquist/runtime/init.lsp")
@end(smallcode)
@end(example)
@p(Note:) in the three places where you see @code(C:/Program Files/Nyquist), insert the full path where Nyquist is actually installed. Use forward slashes (@code(/)) rather than back slashes (@code(\)) to separate directories. For example, if Nyquist is installed at @code(D:\rbd\nyquist), then @code(init.lsp) should contain:
@begin(example)
@begin(smallcode)
(setf *search-path* "D:/rbd/nyquist/runtime,D:/rbd/nyquist/lib")
(load "d:/rbd/nyquist/runtime/init.lsp")
@end(smallcode)
@end(example)
The variable @code(*search-path*), if defined, is used in place of the registry to determine search paths for files.

@paragraph(SystemRoot)
@index(SystemRoot)
(Ignore this paragraph if you are not planning to use Open Sound Control under Windows.)
If Nyquist prints an error message and quits when you enable Open Sound Control (using @code(osc-enable)), check to see if you have an environment variable @code(SystemRoot), e.g. type @code(set) to a command prompt and look for the value of @code(SystemRoot). The normal value is @code(C:\windows). If the value is something else, you should put the environment entry, for example:
@begin(example)
SystemRoot="D:\windows"
@end(example)
into a file named @code(systemroot) (no extension). Put this file in your @code(nyquist) directory. When you run @code(NyquistIDE), it will look for this file and pass the contents as an environment variable to Nyquist. The Nyquist process needs this to open a UDP socket, which is needed for Open Sound Control.

@paragraph(The "java is not recognized" Error)
Sometimes, Nyquist will run directly from the installer, but then it will not start from the Windows Start menu. You can try running the 
 @code(nyquist/jnyqide.bat) program from a Windows shell. If that fails, and you see an error similar to "java is not recognized as in internal or external command error", the problem may be that paths are not set up properly to allow the Windows shell to find java. Right click on ``My Computer'' on the Windows
desktop and select ``Properties.'' Under the ``Advanced'' tap, press the ``Environment Variables'' button, and look for PATH under ``System Variables.'' Make sure the Java bin directory is on the path. If it is not, you will have to find your installation of Java and add the appropriate directory to the PATH variable, e.g. ``C:\Program Files\Java\jdk1.6.0\bin.''

@subsection(MacOS X Installation)
The OS X version of Nyquist is very similar to the Linux version, but it
is developed using Xcode, Apple's programming environment. With a little
work, you can use the Linux installation instructions to compile Nyquist,
but it might be simpler to just open the Xcode project that is included
in the Nyquist sources. 

You can also download a pre-compiled version of Nyquist for the Mac. 
Just download @code(nyqosx2xx.tgz) to the desktop and open it to 
extract the folder <tt>nyqosx2xx</tt>. (Again, "2xx" refers to the current
version number, e.g. v2.31 would be named with "231".) Open the folder
to find a Mac Application named NyquistIDE and a directory named 
<tt>nyquist/doc</tt>. Documentation is in the <tt>nyquist/doc</tt> directory.

The file @code(NyquistIDE.app/Contents/Resources/Java/ny) 
is the command line executable (if you should need it). To run from the
command line, you will need to set the XLISPPATH environment variable as 
with Linux. On the topic of the @code(XLISPPATH), note that this variable is
set by NyquistIDE when running with that application, overriding any other
value. You can extend the search path by creating the file @code(xlisppath) 
in the same directory as the nyquist executable @code(ny). The 
@code(xlisppath) file should have colon-separated paths
on a single line of text.
@end(comment)

@section(Using NyquistIDE)@index(NyquistIDE)@index(IDE)@index(Integrated Development Environment)
The program named NyquistIDE is an ``integrated development environment'' for Nyquist. When you run NyquistIDE, it starts the Nyquist program and displays all Nyquist output in a window. NyquistIDE helps you by providing a Lisp and SAL editor, hints for command completion and function parameters, some graphical interfaces for editing envelopes and graphical equalizers, and a panel of buttons for common operations. A more complete description of NyquistIDE is in Chapter @ref(jnyqide-chapter).

For now, all you really need to know is that you can enter Nyquist commands by typing into the upper left window. When you type return, the expression you typed is sent to Nyquist, and the results appear in the window below. You can edit files by clicking on the New File or Open File buttons. After editing some text, you can load the text into Nyquist by clicking the Load button. NyquistIDE always saves the file first; then it tells Nyquist to load the file. You will be prompted for a file name the first time you load a new file.

@section(Using SAL)
SAL mode means that Nyquist reads and evaluates SAL commands rather than Lisp. The SAL mode prompt is "@code(SAL> )" while the Lisp mode prompt is "@code(> )".
When Nyquist starts it normally enters SAL mode automatically, but certain errors may exit SAL mode. You can reenter SAL mode by typing the Lisp expression @code[(sal)].

In SAL mode, you type commands in the SAL programming language. Nyquist reads the commands, compiles them into Lisp, and evaluates the commands. Commands can be entered manually by typing into the upper left text box in NyquistIDE.

@section(Helpful Hints)
Under Win95 and Win98, the console sometimes locks up. Activating another window and then reactivating the Nyquist window should unlock the output. 
(We suggest you use JNyqIDE, the interactive development environment rather than a console window.)

You can cut and paste text into Nyquist, but for serious work, you will want to use the Lisp @code(load) command. To save even more time, write a 
function to load your working file, e.g. @code[(defun l () (load "myfile.lsp"))]. Then you can type @code[(l)] to (re)load your file.

Using SAL, you can type 
@begin(example)
define function l () load "myfile.lsp"
@end(example)
and then 
@begin(example)
exec l()
@end(example)
to (re)load the file.

The Emacs editor is free GNU software and will help you balance parentheses if you use Lisp mode. In fact, you can run nyquist (without the IDE) as a subprocess to Emacs. A helful discussion is at //http://www.audacity-forum.de/download/edgar/nyquist/nyquist-doc/examples/emacs/main.html. If you use Emacs, there is also a SAL mode (the file is @code(sal-mode.el)) included with the Common Music distribution, which you can find on the Web at @code(sourceforge.net).

The NyquistIDE also runs Nyquist as a subprocess and has
built-in Lisp and SAL editors. If your editor does not help you balance parentheses, you may find yourself counting parens and searching for unbalanced 
expressions. If you are confused or desperate and using Lisp syntax,
try the 
@code(:print t)
option of the @code(load) function. By looking at the expressions printed,
you should be able to tell where the last unbalanced expression starts.
Alternatively, type @code[(file-sexprs)] and type the lisp file name at
the prompt. This function will read and print 
expressions from the file, reporting an error when an extra paren or end-of-file is reached unexpectedly. 

@section(Using Lisp)
Lisp mode means that Nyquist reads and evaluates Nyquist expressions in 
Lisp syntax. Since Nyquist is build on a Lisp interpreter, this is the
``native'' or machine language of Nyquist, and certain errors and functions 
may break out of the SAL interpreter, leaving you with a prompt for a Lisp
expression. Alternatively, you can exit SAL simply by typing @code(exit) to
get a Lisp prompt (@code(> )). Commands can be entered manually by typing
 into the upper left text box in NyquistIDE.

@section(Examples)
We will begin with some simple Nyquist programs. Throughout the manual,
we will assume SAL mode and give examples in SAL, but it should be
emphasized that all of these examples can be performed using Lisp
syntax. See Section @ref(sal-vs-lisp-section) on the relationship between
SAL and Lisp.

Detailed explanations of the functions used in these examples will be
presented in later chapters, so at this point, you should just read these
examples to get a sense of how Nyquist is used and what it can do.  The
details will come later.  Most of these examples can be found in the
file @code(nyquist/demos/examples.sal). Corresponding Lisp syntax
examples are in the file @code(nyquist/demos/examples.lsp).

Our first example makes and plays a sound:@index(osc)@index(play)
@begin(example)
@i(;; Making a sound.)
play osc(60) ; generate a loud sine wave
@comment{(play (osc 60))  @i(; generate a loud sine wave)} @end(example)
This example is about the simplest way to create a sound with Nyquist.  The
@code(osc) function generates a sound using a table-lookup oscillator.
There are a number of optional parameters, but the default is to compute a
sinusoid with an amplitude of 1.0.  The parameter @code(60) designates a
pitch of middle C.  (Pitch specification will be described in greater detail
later.)  The result of the @code(osc) function is a sound.  To hear a sound,
you must use the @code(play) command, which plays the file through the machine's D/A converters.  It also writes a soundfile in case the computation cannot keep up with real time. You can then (re)play the file by typing:
@begin(example)
exec r()
@end(example)
This @code[(r)] function is a general way to ``replay'' the last thing written by @code(play).

Note: when Nyquist plays a sound, it scales the signal by 2@+(15)-1 and (by default) converts to a 16-bit integer format. A signal like  @code[(osc 60)], which ranges from +1 to -1, will play as a full-scale 16-bit audio signal. 

@subsection(Waveforms)
@label(waveform-sec)
Our next example will be presented in several steps.  The goal is to create a
sound using a
wavetable consisting of several harmonics as opposed to a simple sinusoid.
In order to build a table, we will use a function that computes a single 
harmonic and add harmonics to form a wavetable.  An oscillator
will be used to compute the harmonics.

@begin(comment)
Ordinarily, Nyquist programs are sample-rate independent, and time (in
seconds) is used rather than sample numbers to indicate regions of signals.
Since we want a wave-table with a certain integer number of samples, we need
to do some extra calculations to convert from time to samples.  The
@code(build-harmonic) function (see Figure @ref(build-harmonic-fig))
produces a signal with @code(n) periods in the course of 2048 samples using
the @code(snd-sine) function.  

@begin(figure)
@begin(example)
(defun build-harmonic (n tablesize) (snd-sine 0 n tablesize 1))
@end(example)
@fillcaption(@code(build-harmonic) uses a low-level function,
@code(snd-sine), to construct a harmonic.  The function parameters denote:
``compute a sinusoid starting at time zero, with frequency @i(n), a sample
rate of @i(tablesize) samples per second and a duration of 1 second.''  This signal
becomes a periodic waveform to be resampled by a table lookup oscillator, so
the choice of sample rate and duration is a matter of convenience.
@tag(build-harmonic-fig)
@end(figure)
@end(comment)

The function
@code(mkwave@index(mkwave)) calls upon
@code(build-harmonic@index(build-harmonic)) to generate a total of four
harmonics with amplitudes 0.5, 0.25, 0.125, and 0.0625.  
These are scaled and added (using @code(+@index(sim))) to
create a waveform which is bound temporarily to @code(*table*).  

A complete Nyquist waveform is a list consisting of a sound, a pitch,
 and @code(T), indicating a periodic waveform.  The pitch gives the
nominal pitch of the sound.  (This is implicit in a single cycle wave
table, but a sampled sound may have many periods of the fundamental.)
Pitch is expressed in half-steps, where middle C is 60 steps, as in MIDI
pitch numbers.
The list of sound, pitch, and @code(T) is formed in the last line of
@code(mkwave): since @code(build-harmonic) computes signals with a duration
of one second, the fundamental is 1 Hz, and the @code(hz-to-step) function
converts to pitch (in units of steps) as required.

@begin(example)@label(build-harmonic-example)
define function mkwave()
  begin
    set *table* = 0.5 * build-harmonic(1.0, 2048) +
                  0.25 * build-harmonic(2.0, 2048) +
                  0.125 * build-harmonic(3.0, 2048) +
                  0.0625 * build-harmonic(4.0, 2048)
    set *table* = list(*table*, hz-to-step(1.0), #t)
  end
@end(example)

Now that we have defined a function, the last step of this example is to
build the wave.  The following code calls
@code(mkwave) the first time the code is executed (loaded from a file).  The second time, the variable @code(*mkwave*) will be true, so @code(mkwave) will not be invoked:
@begin(example)
if ! fboundp(quote(*mkwave*)) then
  begin
    exec mkwave()
    set *mkwave* = #t
  end
@end(example)

@subsection(Wavetables)@index(wavetables)@index(waveforms)@index(triangle wave)@index(sawtooth wave)
When Nyquist starts, several waveforms are created and stored in global variables for convenience. They are: @code(*sine-table*), @code(*saw-table*), and @code(*tri-table*), implementing sinusoid, sawtooth, and triangle waves, respectively. The variable @code(*table*) is initialized to @code(*sine-table*), and it is @code(*table*) that forms the default wave table for many Nyquist oscillator behaviors. If you want a proper, band-limited waveform, you should construct it yourself, but if you do not understand this sentence and/or you do not mind a bit of aliasing, give @code(*saw-table*) and @code(*tri-table*) a try.

Note that in Lisp and SAL, global variables often start and end with asterisks (*). These are not special syntax, they just happen to be legal characters for names, and their use is purely a convention.

@subsection(Sequences)@index(Sequences)
Finally, we define @code(my-note@index(my-note)) to use the waveform, and play several notes
in a simple score. Note that the function @code(my-note) has only one command
(a @code(return) command), so it is not necessary to use @code(begin) and 
@code(end). These are only necessary when the function body consists of a
sequence of statements:
@begin(example)
define function my-note(pitch, dur)
  return osc(pitch, dur, *table*)

play seq(my-note(c4, i), my-note(d4, i), my-note(f4, i), 
         my-note(g4, i), my-note(d4, q))

@end(example)
Here, @code(my-note) is defined to take pitch and duration as parameters;
it calls @code(osc) to do the work of generating a waveform, using
@code(*table*) as a wave table.

The @code(seq) function is used to invoke a sequence of behaviors.  Each
note is started at the time the previous note finishes.  The parameters to
@code(my-note) are predefined in Nyquist: @code(c4) is middle C, @code(i) (for
eIghth note) is 0.5, and @code(q) (for Quarter note) is 1.0.  See Section
@ref(constants-sec) for a complete description.  The result is the sum of
all the computed sounds.

Sequences can also be constructed using the @code(at) transformation to 
specify time offsets. See 
@code(sequence_example.htm)@index(sequence_example.htm)@index(score, musical)
@code(demos, sequence) for more examples and explanation.

@subsection(Envelopes)@index(Envelopes)
The next example will illustrate the use of envelopes.  In Nyquist,
envelopes are just ordinary sounds (although they normally have a low sample
rate).  An envelope@index(envelope) is applied to another sound by
multiplication using the @code(mult@index(mult)) function.  The code shows
the definition of @code(env-note@index(env-note)), defined in terms of the
@code(note) function in the previous example.  In @code(env-note), a 4-phase
envelope is generated using the @code(env@index(env)) function, which is
illustrated in Figure @ref(env-fig).

@begin(figure)
@center(@graphic((height = 2 in, width = 3.75 in, magnify = 1,
		postscript = "envfig.ps"))
@html(<img src="fig1.gif"><br><br>)
@fillcaption(An envelope generated by the @code(env) function.)
@tag(env-fig)
@end(figure)

@begin(Example)
@i[; env-note produces an enveloped note.  The duration
;   defaults to 1.0, but stretch can be used to change
;   the duration.
;   Uses my-note, defined above.
;]
define function env-note(p)
  return my-note(p, 1.0) *
         env(0.05, 0.1, 0.5, 1.0, 0.5, 0.4)

@i[; try it out:
;]
play env-note(c4)
@end(example)
While this example shows a smooth envelope multiplied by an audio signal,
you can also multiply audio signals to achieve
what is often called @i(ring modulation)@index(ring modulation). See
the code and description in 
@code(demos/scratch_tutorial.htm)@index(demos, ring modulation) for an
interesting use of ring modulation to create ``scratch'' sounds.

In the next example, The @i(stretch) operator (@code(~))@index(stretch) 
is used to modify durations:
@begin(example)
@i[; now use stretch to play different durations
;]
play seq(seq(env-note(c4), env-note(d4)) ~ 0.25,
         seq(env-note(f4), env-note(g4)) ~ 0.5,
         env-note(c4))
@end(example)

In addition to @i(stretch), there are a number of transformations supported
 by Nyquist,
and transformations of abstract behaviors is perhaps @i(the) fundamental
idea behind Nyquist.  Chapter @ref(behavioral-abstraction-sec) is devoted to
explaining this concept, and further elaboration can be found elsewhere
@cite(icmcfugue).

@subsection(Piece-wise Linear Functions)
It is often convenient to construct signals in Nyquist using a list of
(time, value) breakpoints which are linearly interpolated to form a smooth
signal.  Envelopes created by @code(env) are a special case of the more
general piece-wise linear functions created by @code(pwl).  Since @code(pwl)
is used in some examples later on, we will take a look at @code(pwl)
now.  The @code(pwl) function takes a list of parameters which denote (time,
value) pairs.  There is an implicit initial (time, value) pair of (0, 0),
and an implicit final value of 0.  There should always be an odd number of
parameters, since the final value (but not the final time) is implicit. 
Here are some examples:
@begin(Example)
@i[; symetric rise to 10 (at time 1) and fall back to 0 (at time 2):
;]
pwl(1, 10, 2)

@i[; a square pulse of height 10 and duration 5. 
; Note that the first pair (0, 10) overrides the default initial
; point of (0, 0).  Also, there are two points specified at time 5:
; (5, 10) and (5, 0).  (The last 0 is implicit).  The conflict is
; automatically resolved by pushing the (5, 10) breakpoint back to
; the previous sample, so the actual time will be 5 - 1/sr, where
; sr is the sample rate.
;]
pwl(0, 10, 5, 10, 5)

@i[; a constant function with the value zero over the time interval
; 0 to 3.5.  This is a very degenerate form of pwl.  Recall that there
; is an implicit initial point at (0, 0) and a final implicit value of
; 0, so this is really specifying two breakpoints: (0, 0) and (3.5, 0):
;]
pwl(3.5)

@i[; a linear ramp from 0 to 10 and duration 1.
; Note the ramp returns to zero at time 1.  As with the square pulse
; above, the breakpoint (1, 10) is pushed back to the previous sample.
;]
pwl(1, 10, 1)

@i[; If you really want a linear ramp to reach its final value at the 
; specified time, you need to make a signal that is one sample longer.
; The RAMP function does this:
;]
ramp(10) @i[; ramp from 0 to 10 with duration 1 + one sample period
;
; RAMP is based on PWL; it is defined in @p(nyquist.lsp).
;]
@end(example)

@section(Predefined Constants)
@label(constants-sec)
For convenience and readability, Nyquist pre-defines some constants, mostly
based on the notation of the Adagio score language, as follows:
@begin(itemize)
@b(Dynamics)
Note: these dynamics values are subject to change.
@begin(display)
@t(lppp@index(lppp)) = -12.0 (dB)
@t(lpp@index(lpp)) = -9.0
@t(lp@index(lp)) = -6.0
@t(lmp@index(lmp)) = -3.0
@t(lmf@index(lmf)) = 3.0
@t(lf@index(lf)) = 6.0
@t(lff@index(lff)) = 9.0
@t(lfff@index(lfff)) = 12.0
@t(dB0@index(dB0)) = 1.00
@t(dB1@index(dB1)) = 1.122
@t(dB10@index(dB10)) = 3.1623
@end(display)

@b(Durations@index(duration notation))
@begin(display)
@t(s@index(s)) = Sixteenth@index(Sixteenth note) = 0.25
@t(i@index(i)) = eIghth@index(eIghth note) = 0.5
@t(q@index(q)) = Quarter@index(quarter note) = 1.0
@t(h@index(h)) = Half@index(half note) = 2.0
@t(w@index(w)) = Whole@index(whole note) = 4.0
@t(sd@index(sd), id@index(id), qd@index(qd), hd@index(hd), wd@index(wd)) = dotted durations@index(dotted durations).
@t(st@index(st), it@index(it), qt@index(qt), ht@index(ht), wt@index(wt)) = triplet@index(triplet durations) durations.
@end(display)

@b(Pitches@index(pitch notation)) 
Pitches are based on an A4 of 440Hz.  To achieve a different tuning,
set @code(*A4-Hertz*) to the desired frequency for A4, and call
@code[(set-pitch-names)].  This will recompute the names listed below with a
different tuning.  In all cases, the pitch value 69.0 corresponds exactly to
440Hz, but fractional values are allowed, so for example, if you set
@code(*A4-Hertz*) to 444 (Hz), then the symbol @code(A4) will be bound to
69.1567, and @code(C4) (middle C), which is normally 60.0, will be 60.1567.
@begin(display)
@t(c0) = 12.0
@t(cs0, df0) = 13.0
@t(d0) = 14.0
@t(ds0, ef0) = 15.0
@t(e0) = 16.0
@t(f0) = 17.0
@t(fs0, gf0) = 18.0
@t(g0) = 19.0
@t(gs0, af0) = 20.0
@t(a0) = 21.0
@t(as0, bf0) = 22.0
@t(b0) = 23.0
@t(c1) ... @t(b1) = 24.0 ... 35.0
@t(c2) ... @t(b2) = 36.0 ... 47.0
@t(c3) ... @t(b3) = 48.0 ... 59.0
@t(c4) ... @t(b4) = 60.0 ... 71.0
@t(c5) ... @t(b5) = 72.0 ... 83.0
@t(c6) ... @t(b6) = 84.0 ... 95.0
@t(c7) ... @t(b7) = 96.0 ... 107.0
@t(c8) ... @t(b8) = 108.0 ... 119.0
@end(display)

@b(Miscellaneous)
@begin(display)
@codef(ny:all)@pragma(defn)@index(ny:all) = ``all the samples'' (i.e. a big number) = 1000000000
@end(display)
@end(itemize)

@section(More Examples)
More examples can be found in the directory @code(demos), part of the standard
Nyquist release. The file @code(demos/examples_home.htm) is an index to all the demo descriptions. In this directory, you will find the following and more:
@begin(itemize)
How to make arpeggios (@code(demos/arpeggiator.htm) and @code(arp.sal))@index(arpeggiator)

Gong sounds by additive synthesis@index(additive synthesis, gongs)
(@code(demos/pmorales/b1.lsp) and @code(demos/mateos/gong.lsp))@index(Gong sounds)@index(demos, gong sound)

Risset's spectral analysis of a chord
(@code(demos/pmorales/b2.lsp))@index(Risset)@index(demos, spectral analysis of a chord)

Bell sounds 
(@code(demos/pmorales/b3.lsp), @code(demos/pmorales/e2.lsp), @code(demos/pmorales/partial.lsp), and @code(demos/mateos/bell.lsp))@index(demos, bell sound)@index(bell sound)

Drum sounds by Risset
(@code(demos/pmorales/b8.lsp)@index(demos, drum sound)@index(drum sound)

Shepard tones (@code(demos/shepard.lsp) and @code(demos/pmorales/b9.lsp))@index(Shepard tones)@index(endless tones)

Random signals (@code(demos/pmorales/c1.lsp))

Buzz with formant filters 
(@code(demos/pmorales/buzz.lsp))@index(vocal sound)@index(demos, formants)

Computing samples directly in Lisp (using Karplus-Strong and physical modelling
as examples)
(@code(demos/pmorales/d1.lsp))@index(demos, sample-by-sample)@index(DSP in Lisp)@index(Lisp DSP)@index(Karplus-Strong synthesis)@index(physical model)@index(flute sound)

FM Synthesis examples, including bell@index(bell sound), wood drum@index(wood drum sound),
brass sounds@index(brass sound), tuba sound @index(tuba) (@code(demos/mateos/tuba.lsp) and clarinet sounds@index(clarinet sound) (@code(demos/pmorales/e2.lsp))@index(demos, FM synthesis)

Rhythmic patterns (@code(demos/rhythm_tutorial.htm))@index(demos, rhythmic pattern)

Drum Samples and Drum Machine
(@code(demos/plight/drum.lsp))@index(demos, drum machine)@index(drum
samples)@index(drum machine). (See Section @ref(drum-machine-sec)).
@end(itemize)

@chapter(The NyquistIDE Program)
@label(jnyqide-chapter)
The NyquistIDE program combines many helpful functions and interfaces to help you get the most out of Nyquist. NyquistIDE is implemented in Java, and you will need the Java runtime system or development system installed on your computer to use NyquistIDE. The best way to learn about NyquistIDE is to just use it. This chapter introduces some of the less obvious features. If you are confused by something and you do not find the information you need here, please contact the author.

@section(NyquistIDE Overview)
The NyquistIDE runs the command-line version of Nyquist as a subtask, so everything that works in Nyquist should work when using the NyquistIDE and vice-versa. Input to Nyquist is usually entered in the top left window of the NyquistIDE. When you type return, if the expression or statement appears to be complete, the expression you typed is sent to Nyquist. Output from Nyquist appears in a window below. You cannot type into or edit the output window text.

The normal way to use the NyquistIDE is to create or open one or more files. You edit these files and then click the Load button. To load a file, NyquistIDE saves the file, sets the current directory of Nyquist to that of the file, then issues a @code(load) command to Nyquist. In this case and several others, you may notice that NyquistIDE sends expressions to Nyquist automatically for evaluation. You can always see the commands and their results in the output window.

Notice that when you load a selected file window, NyquistIDE uses @code(setdir) to change Nyquist's current directory. This helps to keep the two programs in sync. Normally, you should keep all the files of a project in the same directory and avoid manually changing Nyquist's current directory (i.e. avoid calling @code(setdir) in your code).

Arranging windows in the NyquistIDE can be time-consuming, and depending on the
operating system, it is possible for a window to get into a position where you
cannot drag it to a new position. The Window:Tile menu command can be used
to automatically lay out windows in a rational way. There is a preference 
setting to determine the height of the completion list relative to the height of the output 
window.

@section(The Button Bar)
@index(button bar)
There are a number of buttons with frequently-used operations. These are:
@begin(itemize)
Info @itemsep @index(info button)Print information about Nyquist memory
utilization, including
the number of free cons cells, the number of garbage collections,
the total number of cons cells, the total amount of sample buffer memory,
and the amount of memory in free sample buffers.

Break @itemsep @index(break button)Send a break character to XLISP. This
can be used to enter the debugger (the break loop) while a program is 
running. Resume by typing @code[(co)].

SAL/Lisp @itemsep @index(SAL button)@index(Lisp button)Switch modes.
The button names the mode (SAL or Lisp) you will switch to, not the 
current mode. For example, if you are in Lisp mode and want to type
a SAL command, click the SAL button first.

Top @itemsep @index(top button)Enters @code[(top)] into Nyquist.
If the XLISP prompt is @code(1>) or 
some other integer followed by ``@code(>)'', clicking the Top button
will exit the debug loop and return to the top-level prompt.

Replay @itemsep @index(replay button)Enters @code[(r)] into Nyquist.
This command replays the last computed sound.

F2-F12 @itemsep @index(Fn button)Enters @code[(f2)] etc. into Nyquist.
These commands are not built-in, and allow users to define their own
custom actions.

Browse @itemsep @index(browse button)Equivalent to the Window:Browse
menu item. (See Section @ref(browser).)

EQ @itemsep @index(eq button)Equivalent to the Window:EQ menu item.
(See Section @ref(eq-window-sec).)

EnvEdit @itemsep @index(envedit button)Equivalent to the Window:Envelope Edit 
menu item. (See Section @ref(envelope-editor-sec).)

NewFile @itemsep @index(newfile button)Equivalent to the File:New menu
item. Opens a new file editing window for creating and
loading a Lisp or SAL program file.

OpenFile @itemsep @index(openfile button)Equivalent to the File:Open menu
item. Opens an existing Lisp or SAL program file for editing and loading.

SaveFile @itemsep @index(savefile button)Equivalent to the File:Save menu
item (found on the editing window's menu bar). Saves the contents
of an editing window to its associated file.

Load @itemsep @index(load button)Equivalent to the File:Load menu
item (found on the editing window's menu bar). Performs a Save operation,
then sends a command to Nyquist that loads the file as a program.

Mark @itemsep @index(mark button)Sends a Control-A to Nyquist. While
playing a sound, this displays and records the approximate time in the
audio stream. (See Section @ref(play-sec) for more detail.)

@end(itemize)

@section(Command Completion)
To help with programming, NyquistIDE maintains a command-completion window.
As you type the first letters of function names, NyquistIDE lists matching
functions and their parameters in the Completion List window. If you click
on an entry in this window, the displayed expression will replace the 
incompletely typed function name. A preference allows you to match initial
letters or any substring of the complete function name. This is controlled
by the ``Use full search for code completion'' preference.

In addition, if you right click (or under Mac OS X, hold down the Alt/Option
key and click) on an entry, NyquistIDE will display documentation for the function.
Documentation can come from a local copy or from the online copy (determined
by the ``Use online manual instead of local copy'' preference). Documentation 
can be displayed within the NyquistIDE window or in an external browser (determined
by the ``Use window in NyquistIDE for help browser'' preference.) Currently, the
external browser option does not seem to locate documentation properly, but 
this should be fixed in the future.

@section(Browser)
@label(browser)
@index(browser, jnyqide)@index(sound browser, jnyqide)
If you click on the Browse button or use the Window:Browse menu command, 
NyquistIDE will display a browser window that is pre-loaded with a number of
 Nyquist commands to create sounds. You can adjust parameters, audition
the sounds, and capture the expression that creates the sound. In many 
cases, the expression checks to see if necessary functions are defined,
loading files if necessary before playing the sound. If you want to use
a sound in your own program, you can often simplify things by explicitly
loading the required file just once at the beginning of your file.

Since Nyquist now supports a mix of Lisp and SAL, you may find yourself in
the position of having code from the browser in one language while you are 
working in the other. The best way to handle this is to put the code for
the sound you want into a function defined in a Lisp (@code(.lsp)) or SAL
(@code(.sal)) file. Load the file (from Lisp, use the @code(sal-load)
command to load a SAL file), and call the function from the language of 
your choice.

@section(Envelope Editor)
@label(envelope-editor-sec)
@index(envelope editor)
@index(editor for envelopes)
@index(graphical envelope editor)
The envelope editor allows you graphically to design and edit piece-wise
linear and exponential envelopes. The editor maintains a list of envelopes
and you select the one to edit or delete using the drop down list in the 
Saved Envelopes List area. The current envelope appears in the Graphical
Envelope Editor area. You can click to add or drag points. Alternatively,
you can use the Envelope Points window to select and edit any breakpoint
by typing coordinates. The duration of the envelope is controlled by the
Stop field in the Range area, and the vertical axis is controlled by the
Min and Max fields.

When you click the Save button, @i(all) envelopes are written to Nyquist.
You can then use the envelope by treating the envelope name as a function.
For example, if you define an envelope named ``fast-attack,'' then you
can create the envelope within a Nyquist SAL program by writing
the expression @code[fast-attack()].

These edited envelopes are saved to a file named @code(workspace.lsp) 
 @index(workspace) in
the current directory. The workspace is Nyquist's mechanism for saving
data of all kinds (see Section @ref(workspaces-sec)). The normal way to
work with workspaces is to (1) load the workspace, i.e.
 @code[load "workspace"], as soon as you start Nyquist; (2) invoke
the envelope editor to change values in the workspace; and (3) save the
workspace at any time, especially before you exit NyquistIDE. If you follow
these steps, envelopes will be preserved from session to session, and 
the entire collection of envelopes will appear in the editor. Be 
sure to make backups of your @code(workspace.lsp) file along with your
other project files.

The envelope editor can create linear and exponential envelopes. Use the
Type pull-down menu to select the type you want. Envelopes can be created
using default starting and ending values using @code(pwl) or @code(pwe), 
or you can specify the initial values using @code(pwlv) or @code(pwev).
The envelope editor uses @code(pwl) or @code(pwe) if no point is explicitly
entered as the initial or final point. To create a @code(pwlv) or @code(pwev)
function, create a point and drag it to the leftmost or rightmost edge
of the graphical editing window. You will see the automatically
generated default starting or ending
point disappear from the graph.

Exponential envelopes should never decay to zero. If you enter a zero
amplitude, you will see that the envelope remains at zero to the next
breakpoint. To get an exponential decay to ``silence,'' try using an
amplitude of about 0.001 (about -60dB). To enter small values like 
this, you can type them into the Amplitude box and click ``Update Point.''

The Load button refreshes the editor from data saved in the Nyquist 
process. Normally, there is no need to use this because the editor
automatically loads data when you open it.

@section(Equalizer Editor)
@label(eq-window-sec)@index(equalization editor)@index(graphical equalization)
The Equalizer Editor provides a graphical EQ interface for creating and
adjusting equalizers. Unlike the envelope editor, where you can type
any envelope name, equalizers are named @code(eq-0), @code(eq-1), etc.,
and you select the equalizer to edit using a pull-down menu. The @code(Set)
button should be use to record changes. 

@section(UPIC Editor)
@label(upic-editor-sec)@index(UPIC)@index(Xenakis)@index(Iannis Xenakis)The 
UPIC Editor is inspired by the UPIC system by Iannis Xenakis at the Centre
d'Edudes de Mathematique et Automatique Musicales (CEMaMu). The UPIC Editor
is accessed by the ``Upic Edit'' menu item in the ``Window'' menu of the 
NyquistIDE. Once opened, you can draw pitch contours in the main panel by
pressing the left mouse button and dragging with the mouse down. Contours
represent tones in a frequency vs. time coordinate system. Any contour can be
deleted by right-clicking (or shift-clicking on an Apple computer) to 
select the contour (indicated by the color red), and typing the Delete key.

A collection of contours can be saved to a file and later retrieved using the 
items in the File menu (use the File menu in the UPIC Editor window, not in 
the main NyquistIDE window.) The file is a SAL program in a special format
that can be parsed by the UPIC Editor. The file can also be loaded
into Nyquist using the File:Load menu item, or by executing a load command
in Nyquist.

The panel at the top of the editor offers control over various parameters.
The Name box is a Nyquist variable name. This name takes effect when you
save a file from the UPIC Editor. The variable name is stored in the
file so that when a UPIC Editor-generated file
is loaded into Nyquist, the data is assigned to this variable name. 
The data is
a list of contours, where each contour specifies a waveform, an envelope,
and a list of time-frequency coordinates.

The next item in the panel is the Waveform box. The Waveform box names a 
waveform for a contour. Default waveforms are sinusoid, triangle, and 
sawtooth, but you can type in your own names. The currently selected 
waveform is stored with the contour when it is created (entered by
drawing). You cannot change or edit the waveform name associated with
a contour once the contour is created, but you can always delete the
contour and replace it. The Envelope box names an envelope for a 
contour. The envelope names a Nyquist function. The default, @code(upic-env)
is a trapezoid shape with an onset time and offset time of 10ms. As with
waveforms, the envelope is stored with each contour when the contour is
created and cannot be edited.

The Stop Time box gives the duration of the drawing area in seconds. 
The Min Freq box gives the minimum frequency (at the bottom of the 
drawing area), and the Max Freq box gives the maximum frequency (at the
top of the drawing area). The vertical frequency axis can use a linear
scale corresponding to frequency in Hertz or a logarithmic scale
corresponding to semitones. The ``linear'' checkbox selects the linear
scale. When @i(any) of these parameters (described in this paragraph and 
delimited by the border labeled ``Range'' on the control panel) is changed,
you must press the Update Range button for the change to take effect.

The Background menu lets you display a grid that indicates pitch locations.
The ``C's'' item draws a line at C in every visible octave. E.g. middle C 
is about 260 Hz, so a reference line will be drawn near 260 Hz. 
Lines will be drawn around 130 Hz (an octave below middle C), and around
520 Hz (an octave above middle C), etc. The ``GrandStaff'' menu item
draws reference lines for each line of the grand staff commonly used
for piano music. The pitches are G2, B2, D3, F3, A3, E4, G4, B4, D5, and 
F5. Finally, you can load a picture using the 
Background:Load Picture... menu item. Then, the Background:Show Picture
menu item toggles whether the picture is displayed or not. This feature
allows you to trace an image. (For example, see the Sonic Self-Portrait
at @code(http://www.cs.cmu.edu/~rbd).) You may wish to use an image editor
to lighten the image so that superimposed contours will be more visible.

Each change to the Range data, background choice, and each entry of a 
contour is an action that you can undo or redo with the Undo and Redo 
buttons.

To convert UPIC data into sound, first load @code(upic.sal) and load
a file generated by the UPIC Editor. Now, suppose the variable name
used is @code(upicdata). You can play the data by writing
@begin(example)
play upic(upicdata)
@end(example)
If you created your own names for waveforms or envelopes, you must be
sure that these exist before calling the @code(upic) function. Each 
waveform must be the name of a variable which is set to a Nyquist
wave table. (See Section @ref(waveform-sec) for information on how
to create a wave table.) Also, each envelope must name a function with
no parameters that will return an amplitude envelope. The following is
the built-in definition for @code(upic-env):
@begin(example)
define function upic-env()
  return env(0.01, 0.01, 0.01, 1, 1, 1)
@end(example)
To make a custom envelope function named @code(upic-smooth) with a 0.2
 second attack and a 0.3 second decay, you could write:
@begin(example)
define function upic-smooth()
  return env(0.2, 0.01, 0.3, 1, 1, 1)
@end(example)


@chapter(Behavioral Abstraction)@index(behavioral abstraction)
@label(behavioral-abstraction-sec)
In Nyquist, all functions are subject to
transformations@index(transformations).  You can think of transformations as
additional parameters to every function, and functions are free to use these
additional parameters in any way.  The set of transformation parameters is
captured in what is referred to as the @i(transformation
environment@index(transformation environment)).  (Note that the term
@i(environment) is heavily overloaded in computer science.  This is yet
another usage of the term.)

Behavioral abstraction is the ability of functions to adapt their behavior
to the transformation environment.  This environment may contain certain
abstract notions, such as loudness, stretching a sound in time, etc.  These
notions will mean different things to different functions.  For example, an
oscillator should produce more periods of oscillation in order to stretch
its output.  An envelope, on the other hand, might only change the
duration of the sustain portion of the envelope in order to stretch.
Stretching a sample could mean resampling it to change its duration by the
appropriate amount.

Thus, transformations in Nyquist are not simply operations on signals.  For
example, if I want to stretch a note, it does not make sense to compute the
note first and then stretch the signal.  Doing so would cause a drop in the
pitch.  Instead, a transformation modifies the @i(transformation
environment) in which the note is computed.  Think of transformations as
making requests to functions.  It is up to the function to carry out the
request.  Since the function is always in complete control, it is possible
to perform transformations with ``intelligence;'' that is, the function can
perform an appropriate transformation, such as maintaining the desired pitch
and stretching only the ''sustain'' portion
of an envelope to obtain a longer note.

@section(The Environment)@index(environment)
@label(environment-sec)
The transformation environment consists of a set of special variables.
These variables should not be read directly and should @i(never) be set
directly by the programmer.  Instead, there are functions to read them, and
they are automatically set and restored by
transformation operators, which will be described below.

The transformation environment consists of the following elements.  Although
each element has a ``standard interpretation,'' the designer of an
instrument or the composer of a complex behavior is free to interpret the
environment in any way.  For example, a change in @code(*loud*) may change
timbre more than amplitude, and @code(*transpose*) may be ignored by
percussion instruments:


@begin(description)
 @codef(*warp*@pragma(defn)@index(*warp*))@\Time transformation, including time shift,
time stretch, and continuous time warp.  The value of @code[*warp*] is
interpreted as a function from logical (local score) time to physical
(global real) time.  Do not access @code(*warp*) directly.  Instead, use
@code[local-to-global(@i(t))] to
 convert from a logical (local) time to real (global) time.  Most often,
you will call @code[local-to-global(0)].  Several transformation operators
operate on @code[*warp*], including @i(at) (@code(@@)), @i(stretch) (@code(~)),
and @code(warp). See also @code[get-duration()] and @code[get-warp()].

 @codef(*loud*@pragma(defn)@index(*loud*))@\Loudness, expressed in decibels.  The default
(nominal) loudness is 0.0 dB (no change).  Do not access @code(*loud*)
directly.  Instead, use @code[get-loud()] to get the current value of
@code(*loud*) and either @code(loud) or @code(loud-abs) to modify it.

 @codef(*transpose*@pragma(defn)@index(*transpose*))@\Pitch transposition, 
expressed in
semitones.  (Default: 0.0).  Do not access @code(*transpose*) directly.
Instead, use @code[get-transpose()] to get the current value of
@code(*transpose*) and either @code(transpose) or @code(transpose-abs) to
modify it.

 @codef(*sustain*@pragma(defn)@index(*sustain*))@\The ``sustain,''
``articulation,'' ``duty factor,'' or amount by which to 
separate or overlap sequential notes.  For
example, staccato might be expressed with a @code(*sustain*) of 0.5, while very
legato playing might be expressed with a @code(*sustain*) of 1.2.
Specifically, @code(*sustain*) stretches the duration of notes (sustain)
without affecting the inter-onset time (the rhythm).  Do not access
@code(*sustain*) directly.  Instead, use @code[get-sustain()] to get the
current value of @code(*sustain*) and either @code(sustain) or
@code(sustain-abs) to modify it.

 @codef(*start*@pragma(defn)@index(*start*))@\Start 
time of a clipping region.  @i(Note:)
unlike the previous elements of the environment, @code(*start*) has a
precise interpretation: no sound should be generated before @code(*start*).
This is implemented in all the low-level sound functions, so it can
generally be ignored.  You can read @code(*start*) directly, but use
@code(extract) or @code(extract-abs) to modify it.  @p(Note 2:) Due
to some internal confusion between the specified starting time and 
the actual starting time of a signal after clipping, @code(*start*) 
is not fully implemented.

 @codef(*stop*@pragma(defn)@index(*stop*))@\Stop time of clipping 
region.  By analogy to
@code(*start*), no sound should be generated after this time.
@code(*start*) and
@code(*stop*) allow a composer to preview a small section of a work 
without computing it from beginning to end.  You can read @code(*stop*) 
directly, but use @code(extract) or @code(extract-abs) to modify it. 
 @p(Note:) Due to some internal confusion between the specified 
starting time and the actual starting time of a signal after 
clipping, @code(*stop*) is not fully implemented.

@codef(*control-srate*@pragma(defn)@index(*control-srate*))@\Sample 
rate of control signals.  This environment
element provides the default sample rate for control signals.  There is no
formal distinction between a control signal and an audio signal.  
You can read @code(*control-srate*) directly, but 
use @code(control-srate) or @code(control-srate-abs) to modify it.

 @codef(*sound-srate*@pragma(defn)@index(*sound-srate*))@\Sample 
rate of musical sounds.  This environment element provides the 
default sample rate for musical sounds.  You can 
read @code(*sound-srate*) directly, but use @code(sound-srate) 
or @code(sound-srate-abs) to modify it.

@end(description)

@section(Sequential Behavior)@index(sequential behavior)
Previous examples have shown the use of @code(seq), the sequential behavior
operator.  We can now explain @code(seq) in terms of transformations.
Consider the simple expression:
@begin(Example)
play seq(my-note(c4, q), my-note(d4, i))
@end(example)
The idea is to create the first note at time 0, and to start the next
note when the first one finishes.  This is all accomplished by manipulating
the environment.  In particular, @code[*warp*] is modified so that what is
locally time 0 for the second note is transformed, or warped, to the logical
stop time of the first note.

One way to understand this in detail is to imagine how it
might be executed: first, @code(*warp*) is set to an initial value that has no
effect on time, and @code[my-note(c4, q)] is evaluated.  A sound is returned and
saved.  The sound has an ending time, which in this case will be @code(1.0)
because the duration @code(q) is @code(1.0).  This ending time, @code(1.0),
is used to construct a new @code[*warp*] that has the effect of shifting
time by 1.0.  The second note is evaluated, and will start
at time 1.  The sound that is
returned is now added to the first sound to form a composite sound, whose
duration will be @code(2.0).  @code(*warp*) is restored to its initial value.

Notice that the semantics of @code(seq) can be expressed in terms of
transformations.  To generalize, the operational rule for @code(seq) is:
evaluate the first behavior according to the current @code(*warp*).
Evaluate each successive behavior with @code(*warp*) modified to shift the
new note's starting time to the ending time of the previous behavior.
Restore @code(*warp*) to its original value and return a sound which is the
sum of the results.  

In the Nyquist implementation, audio samples are only computed when they are
needed, and the second part of the @code(seq) is not evaluated until the
ending time (called the logical stop time) of the first part.  It is still
the case that when the second part is evaluated, it will see @code(*warp*)
bound to the ending time of the first part.  

A language detail: Even though Nyquist defers evaluation of the second part of the @code(seq), the expression can reference variables according to ordinary 
Lisp/SAL scope rules.  This is because the @code(seq) captures the expression in a closure, which retains all of the variable bindings.

@section(Simultaneous Behavior)@index(Simultaneous Behavior)
Another operator is @code(sim), which invokes multiple behaviors at the same
time.  For example,
@begin(example)
play 0.5 * sim(my-note(c4, q), my-note(d4, i))
@end(example)
will play both notes starting at the same time.

The operational rule for @code(sim) is: evaluate each behavior at the
current @code(*warp*) and return the sum of the results. (In SAL, the
@code(sim) function applied to sounds is equivalent to adding them
with the infix @code(+) operator. The following section
illustrates two concepts: first, a @i(sound) is not a
@i(behavior), and second, the @code(sim) operator and and the @code(at) 
transformation can be used to place sounds in time.

@section(Sounds vs. Behaviors)@index(Sounds vs. Behaviors)
The following example loads a sound from a file in the current directory and stores it in @code(a-snd):
@begin(example)
@i[; load a sound
;]
set a-snd = s-read(strcat(current-path(), "demo-snd.aiff"))

@i[; play it
;]
play a-snd
@end(example)

One
might then be tempted to write the following:
@begin(example)
play seq(a-snd, a-snd)  @i(;WRONG!)
@end(example)
Why is this wrong? Recall
that @code(seq) works by modifying @code(*warp*), not by operating on
sounds.  So, @code(seq) will proceed by evaluating @code(a-snd) with
different values of @code(*warp*).  However, the result of evaluating
@code(a-snd) (a variable) is always the same sound, regardless of the
environment; in this case, the second @code(a-snd) @i(should) start at time
@code(0.0), just like the first. In this case, after the first sound ends,
 Nyquist is unable to ``back up'' to time zero, so in fact, this @i(will) 
play two sounds in sequence, but that is a result of an implementation 
detail rather than correct program execution. In fact, a future version of
Nyquist might (correctly) stop and report an error when it detects that the
second sound in the sequence has a real start time that is before the 
requested one.

How then do we obtain a sequence of two sounds properly?  
What we really need here is a
behavior that transforms a given sound according to the current
transformation environment.  That job is performed by @code(cue).  For
example, the following will behave as expected, producing a sequence of two
sounds:
@begin(example)
play seq(cue(a-snd), cue(a-snd))
@end(example)
This example is correct because the second expression will shift the sound
 stored in @code(a-snd) to start at the end time of the first expression.

The lesson here is very important: @p(sounds are not behaviors!)  Behaviors
are computations that generate sounds according to the transformation
environment.  Once a sound has been generated, it can be stored, copied,
added to other sounds, and used in many other operations, but sounds are
@i(not) subject to transformations.  To transform a sound, use @code(cue),
@code(sound), or @code(control).  The differences between these operations
are discussed later.  For now, here is a ``cue sheet'' style score that
plays 4 copies of @code(a-snd):

@begin(example)
@i[; use sim and at to place sounds in time
;]
play sim(cue(a-snd) @@ 0.0,
         cue(a-snd) @@ 0.7,
         cue(a-snd) @@ 1.0,
         cue(a-snd) @@ 1.2)
@end(example)

@section(The At Transformation)@index(At Transformation)
The second concept introduced by the previous example is the @code(@@)
operation, which shifts the @code(*warp*) component of the environment.  For
example,
@begin(example)
cue(a-snd) @@ 0.7
@end(example)
can be explained operationally as follows: modify @code[*warp*] by shifting
it by @code(0.7) and evaluate @code[cue(a-snd)].  Return the resulting sound
after restoring @code(*warp*) to its original value.  Notice how @code(@@)
is used inside a @code(sim) construct to locate copies of @code(a-snd) in
time.  This is the standard way to represent a note-list or a cue-sheet in
Nyquist.

This also explains why sounds need to be @code(cue)'d in order to be shifted
in time or arranged in sequence.  If this were not the case, then @code(sim)
would take all of its parameters (a set of sounds) and line them up to start
at the same time.  But @code[cue(a-snd) @@ 0.7] is just a sound, so
@code(sim) would ``undo'' the effect of @code(@@), making all of the sounds
in the previous example start simultaneously, in spite of the @code(@@)!
Since @code(sim) respects the intrinsic starting times of sounds, a special
operation, @code(cue), is needed to create a new sound with a new starting
time.

@section(The Stretch Transformation)@index(Stretch Transformation)
In addition to At (denoted in SAL by the @code(@@) operator, the Stretch
transformation is very important. It appeared in the introduction, and
it is denoted in SAL by the @code(~) operator (or in LISP by the @code(stretch)
special form). Stretch also operates on the @code(*warp*) component of
the environment. For example,
@begin(example)
osc(c4) ~ 3
@end(example)
does the following: modify @code(*warp*), scaling the degree of 
"stretch" by 3, and evaluate @code[osc(c4)]. The @code(osc) behavior
uses the stretch factor to determime the duration, so it will return
a sound that is 3 seconds long. Restore @code(*warp*) to its original
value. Like At, Stretch only affects behaviors. @code(a-snd ~ 10) is
equivalent to @code(a-snd) because @code(a-snd) is a sound, not a
behavior. Behaviors are functions that compute sounds according to
the environment and return a sound.

@section(Nested Transformations)@index(Nested Transformations)
Transformations can be combined using nested expressions.  For example,
@begin(example)
sim(cue(a-snd),
    loud(6.0, cue(a-snd) @@ 3))
@end(example)
scales the amplitude as well as shifts the second entrance of @code(a-snd).

Why use @code(loud) instead of simply multiplying @code(a-snd) by some
scale factor? Using @code(loud) gives
the behavior the chance to implement the abstract
property @i(loudness) in an appropriate way, e.g. by including timbral 
changes. In this case, the behavior is @code(cue), which implements
@i(loudness) by simple amplitude scaling, so the result is equivalent
to multiplication by @code(db-to-linear(6.0)).

Transformations can also be applied to groups of behaviors:
@begin(example)
loud(6.0, sim(cue(a-snd) @@ 0.0,
              cue(a-snd) @@ 0.7))
@end(example)

@section(Defining Behaviors)@index(Defining Behaviors)
Groups of behaviors can be named using @code(define) (we already saw this
in the definitions of @code(my-note) and @code(env-note)).  Here is another example
of a behavior definition and its use.  The definition has one parameter:
@begin(example)
define function snds(dly)
  return sim(cue(a-snd) @@ 0.0,
             cue(a-snd) @@ 0.7,
             cue(a-snd) @@ 1.0,
             cue(a-snd) @@ (1.2 + dly))

play snds(0.1)
play loud(0.25, snds(0.3) ~ 0.9)
@end(example)
In the last line, @code(snds) is transformed: the transformations will apply
to the @code(cue) behaviors within @code(snds).  The @code(loud)
transformation will scale the sounds by @code(0.25), and the @i(stretch)
(@code(~)) will
apply to the shift (@code(@@)) amounts @code(0.0), @code(0.7), @code(1.0),
and @code[1.2 + dly].  The sounds themselves (copies of @code(a-snd)) will
not be stretched because @code(cue) never stretches sounds.

Section @ref(transformations-sec) describes the full set of transformations.

@section(Overriding Default Transformations)
In Nyquist, behaviors are @i(the) important abstraction mechanism.
A behavior represents a class of related functions or sounds. For example,
a behavior can represent a musical note. When a note is stretched, it 
usually means that the tone sustains for more oscillations, but if the
``note'' is a drum roll, the note sustains by more repetitions of the
component drum strokes. The concept of sustain is so fundamental that
we do not really think of different note durations as being different
instances of an abstract behavior, but in a music programming language,
we need a way to model these abtract behaviors. As the tone and drum
roll examples show, there is no one right way to ``stretch,'' so the
language must allow users to define exactly what it means to stretch.
By extension, the Nyquist programmer can define how all of the 
transformations affect different behaviors.

To make programming easier, almost all Nyquist sounds are constructed
from primitive behaviors that obey the environment in obvious ways:
Stretch transformations make things longer and At transformations shift
things in time. But sometimes you have to override the default behaviors.
Maybe the attack phase of an envelope should not stretch when the note
is stretched, or maybe when you stretch a trill, you should get more
notes rather than a slower trill.

To override default behaviors, you almost always follow the same 
programming pattern: first, capture the environment in a local variable;
then, use one of the absolute transformations to ``turn off'' the
environment's effect and compute the sound as desired. The following
example creates a very simple envelope with a fixed rise time to
illustrate the technique.
@begin(example)
define function two-phase-env(rise-time)
  begin
    with dur = get-duration(1)
    return pwl(rise-time, 1, dur) ~~ 1.0
  end
@end(example)
To ``capture the environment in a local variable,'' a @code(with)
construct is used to create the local variable @code(dur) and set
it to the value of @code[get-duration(1)], which answers the question:
``If I apply use the environment to stretch something whose nominal
duration is 1, what is the resulting duration?'' (Since time transformations
can involve continuous time deformations, this question is not as 
simple as it may sound, so please use the provided function rather
than peeking inside the @code(*warp*) structure and trying to do it
yourself.) Next, we ``turn off'' stretching using the @code(stretch-abs)
form, which in SAL is denoted by the @code(~~) operator. 
Finally, we are ready to compute the envelope using @code(pwl). Here,
we use absolute durations. The first breakpoint is at @code(rise-time),
so the attack time is given by the @code(rise-time) parameter. The 
@code(pwl) decays back to zero at time @code(dur), so the overall
duration matches the duration expected from the environment encountered
by this instance of @code(two-phase-env). Note, however, that since
the @code(pwl) is evaluated in a different environment established
by @code(~~), it is not stretched (or perhaps more accurately, it is
stretched by 1.0). This is good because it means @code(rise-time) will
not be stretched, but we must be careful to extend the envelope to 
@code(dur) so that it has the expected duration.

@section(Sample Rates)
@index(sample rates)
The global environment contains @code(*sound-srate*) and
@code(*control-srate*), which determine the sample rates of sounds and
control signals.  These can be overridden at any point by the
transformations @code(sound-srate-abs) and @code(control-srate-abs); for
example,
@begin(example)
sound-srate-abs(44100.0, osc(c4)
@end(example)
will compute a tone using a 44.1Khz sample rate even if the default rate
is set to something different.

@index(default sample rate)
As with other components of the environment, you should @i(never) change
@code(*sound-srate*) or @code(*control-srate*) directly.
  The global environment is determined by two additional
variables: @code(*default-sound-srate*) and @code(*default-control-srate*).
You can add lines like the following to your @code(init.lsp) file to change
the default global environment:
@begin(example)
(setf *default-sound-srate* 44100.0)
(setf *default-control-srate* 1102.5)
@end(example)
You can also do this using preferences in NyquistIDE. 
If you have already started Nyquist and want to change the defaults, the
preferences or the following functions can be used:
@begin(example)
exec set-control-srate(1102.5)@index(set-control-srate)
exec set-sound-srate(22050.0)@index(set-sound-srate)
@end(example)
These modify the default values and reinitialize the Nyquist environment.


@chapter(Continuous Transformations and Time Warps)
@label(warp-chap)
Nyquist transformations were discussed in the previous chapter, but all of
the examples used scalar values.  For example, we saw the @code[loud]
transformation used to change loudness by a fixed amount.  What if we want
to specify a crescendo, where the loudness changes gradually over time?

It turns out that all transformations can accept signals as well as numbers,
so transformations can be continuous over time.  This raises some
interesting questions about how to interpret continuous transformations.
Should a loudness transformation apply to the internal details of a note or
only affect the initial loudness?  It might seem unnatural for a decaying 
piano note to perform a crescendo.  On the other hand, a sustained
trumpet sound should probably crescendo continuously.  In the case of time
warping (tempo changes), it might be best for a drum roll to maintain a
steady rate, a trill may or may not change rates with tempo, and a run of
sixteenth notes will surely change its rate.

These issues are complex, and Nyquist cannot hope to automatically do the
right thing in all cases.  However, the concept of behavioral abstraction
provides an elegant solution.  Since transformations merely modify the
environment, behaviors are not forced to implement any particular style of
transformation.  Nyquist is designed so that the default transformation is
usually the right one, but it is always possible to override the default
transformation to achieve a particular effect.

@section(Simple Transformations)
The ``simple'' transformations affect some parameter, but have no effect on time itself.  The simple transformations that support continuously changing parameters are: @code(sustain), @code(loud), and @code(transpose).

As a first example, Let us use @code(transpose) to create a chromatic scale.
First define a sequence of tones at a steady pitch. The @code(seqrep) 
``function'' works like @code(seq) except that it creates copies of a sound
by evaluating an expression multiple times. Here, @code(i) takes on 16 values
from 0 to 15, and the expression for the sound could potentially use @code(i).
Technically, @code(seqrep) is not really a function but an abbreviation for
a special kind of loop construct.
@begin(example)
define function tone-seq()
  return seqrep(i, 16,
                osc-note(c4) ~ 0.25)
@end(example)
Now define a linearly increasing ramp to serve as a transposition function:
@begin(code)
define function pitch-rise()
  return sustain-abs(1.0, 16 * ramp() ~ 4)
@end(code)
This ramp has a duration of 4 seconds, and over that interval it rises from
0 to 16 (corresponding to the 16 semitones we want to transpose). The ramp
is inside a @code(sustain-abs) transformation, which prevents a @code(sustain)
transformation from having any effect on the ramp. (One of the drawbacks of
behavioral abstraction is that built-in behaviors sometimes do the wrong
thing implicitly, requiring some explicit correction to turn off the 
unwanted transformation.) Now,
@code(pitch-rise) is used to transpose @code(tone-seq):
@begin(code)
define function chromatic-scale()
  return transpose(pitch-rise(), tone-seq())
@end(code)

Similar transformations can be constructed to change the sustain or ``duty
factor'' of notes and their loudness.  The following expression plays the
@code(chromatic-scale) behavior with increasing note durations.  The
rhythm is unchanged, but the note length changes from staccato to legato:
@begin(code)
play sustain((0.2 + ramp()) ~ 4,
             chromatic-scale())
@end(code)
The resulting sustain function will ramp from 0.2 to 1.2.  A sustain of 1.2
denotes a 20 percent overlap between notes.  The @code(sum) has a stretch
factor of 4, so it will extend over the 4 second duration of
@code(chromatic-scale).

If you try this, you will discover that the @code(chromatic-scale) no longer
plays a chromatic scale. You will hear the first 4 notes going up in intervals
of 5 semitones (perfect fourths) followed by repeated pitches. What 
is happening is that the @code(sustain) operation applies to 
@code(pitch-rise) in addition to @code(tone-seq), so now the 4s
ramp from 0 to 16 becomes a 0.8s ramp. To fix this problem, we need to 
shield @code(pitch-rise) from the effect of @code(sustain) using the
@code(sustain-abs) transformation. Here is a corrected version of 
@code(chromatic-scale):
@begin(code)
define function chromatic-scale()
  return transpose(sustain-abs(1, pitch-rise()), tone-seq())
@end(code)

What do these transformations mean?  How did the system know to produce a
pitch rise rather than a continuous glissando?  This all relates to the idea
of behavioral abstraction.  It is possible to design sounds that @i(do)
glissando under the transpose transform, and you can even make sounds that
@i(ignore) transpose altogether.  As explained in Chapter 
@ref(behavioral-abstraction-sec), the transformations modify the
environment, and behaviors can reference the environment to determine what
signals to generate.  All built-in functions, such as @code(osc), have a
default behavior.

The default behavior for sound primitives under @code(transpose), 
@code(sustain), and @code(loud) transformations is
to sample the environment at the beginning of the
note.  Transposition is not quantized to semitones or any other scale,
but in our example, we arranged for the transposition to work out to integer
numbers of semitones, so we obtained a chromatic scale anyway.

Transposition only applies to the oscillator and sampling primitives
@code(osc), @code(partial), @code(sampler), @code(sine), @code(fmosc),
and @code(amosc).  Sustain applies to @code(osc), @code(env), @code(ramp),
and @code(pwl). (Note that @code(partial), @code(amosc), and 
@code(fmosc) get their durations
from the modulation signal, so they may indirectly depend upon the sustain.)
Loud applies to @code(osc), @code(sampler), @code(cue), @code(sound),
@code(fmosc), and @code(amosc). (But not @code(pwl) or @code(env).)


@section(Time Warps)
The most interesting transformations have to do with transforming time
itself.  The @code(warp) transformation provides a mapping function from
logical (score) time to real time.  The slope of this function tells us how
many units of real time are covered by one unit of score time.  This is
proportional to 1/tempo.  A higher slope corresponds to a slower tempo.

To demonstrate @code(warp), we will define a time warp function using
@code(pwl):
@begin(example)
define function warper()
  return pwl(0.25, .4, .75, .6, 1.0, 1.0, 2.0, 2.0, 2.0)
@end(example)
This function has an initial slope of .4/.25 = 1.6.  It may be easier to
think in reciprocal terms: the initial tempo is .25/.4 = .625.  Between 0.25
and 0.75, the tempo is .5/.2 = 2.5, and from 0.75 to 1.0, the tempo is again
.625.  It is important for warp functions to completely span the interval of
interest (in our case it will be 0 to 1), and it is safest to extend a bit
beyond the interval, so we extend the function on to 2.0 with a
tempo of 1.0.  Next, we stretch and scale the @code(warper) function to
cover 4 seconds of score time and 4 seconds of real time:
@begin(example)
define function warp4()
  return 4 * warper() ~ 4
@end(example)

@begin(figure)
@center(@graphic((height = 3.25 in, width = 3.5 in, magnify = 0.5,
		postscript = "warpfig.ps"))
@html(<img src="fig2.gif"><br><br>)
@fillcaption{The result of @code[(warp4)], intended to map 4 seconds of
score time into 4 seconds of real time.  The function extends beyond 4
seconds (the dashed lines) to make sure the function is well-defined at
location (4, 4).  Nyquist sounds are ordinarily open on the right.}
@tag(warp-fig)
@end(figure)

Figure @ref(warp-fig) shows a plot of this warp function.  Now, we can
warp the tempo of the @code(tone-seq) defined above using @code(warp4):
@begin(example)
play warp(warp4(), tone-seq())
@end(example)
Figure @ref(warp-notes-fig) shows the result graphically.  Notice that the
durations of the tones are warped as well as their onsets.  Envelopes are 
not shown in detail in the figure.  Because of the way @code(env) is
defined, the tones will have constant attack and decay times, and the
sustain will be adjusted to fit the available time.

@begin(figure)
@center(@graphic((height = 1.75 in, width = 6.75 in, magnify = 1.0,
		postscript = "warpnotesfig.ps"))
@html(<img src="fig3.gif"><br><br>)
@fillcaption[When @code{(warp4)} is applied to @code{(tone-seq-2)}, the note onsets and durations are warped.]
@tag(warp-notes-fig)
@end(figure)

@section(Abstract Time Warps)
We have seen a number of examples where the default behavior did the
``right thing,'' making the code straightforward.  This is not always the
case.  Suppose we want to warp the note onsets but not the durations.  We
will first look at an incorrect solution and discuss the error.  Then we
will look at a slightly more complex (but correct) solution.  

The default behavior for most Nyquist built-in functions is to sample the
time warp function at the nominal starting and ending score times of the
primitive.  For many built-in functions, including @code(osc), the starting
logical time is 0 and the ending logical time is 1, so the time warp
function is evaluated at these points to yield real starting and stopping
times, say 15.23 and 16.79.  The difference (e.g. 1.56) becomes the signal
duration, and there is no internal time warping.  The @code(pwl) function
behaves a little differently.  Here, each breakpoint is warped individually,
but the resulting function is linear between the breakpoints.

A consequence of the default behavior is that notes stretch when the tempo
slows down.  Returning to our example, recall that we want to warp only the
note onset times and not the duration.  One would think that the following
would work:
@begin(example)
define function tone-seq-2 ()
  return seqrep(i, 16,
                osc-note(c4) ~~ 0.25)

play warp(warp4(), tone-seq-2())
@end(example)
Here, we have redefined @code(tone-seq), renaming it to @code(tone-seq-2)
and changing the stretch (@code(~)) to absolute stretch (@code(~~)).  The
absolute stretch should override the warp function and produce a fixed
duration.

If you play the example, you will hear steady
sixteenths and no tempo changes.  What is wrong?  In a sense, the ``fix''
works too well.  Recall that sequences (including @code(seqrep)) determine
the starting time of the next note from the logical stop time of the
previous sound in the sequence.  When we forced the stretch to 0.25, we also
forced the logical stop time to 0.25 real seconds from the beginning, so
every note starts 0.25 seconds after the previous one, resulting in a
constant tempo.

Now let us design a proper solution.  The trick is to use absolute
stretch (@code(~~))
as before to control the duration, but to restore the logical stop time to a
value that results in the proper inter-onset time interval:
@begin(example)
define function tone-seq-3()
  return seqrep(i, 16,
                set-logical-stop(osc-note(c4) ~~ 0.25, 0.25))

play warp(warp4(), tone-seq-3())
@end(example)
Notice the addition of @code(set-logical-stop) enclosing the
absolute stretch (@code(~~)) expression to set the logical
 stop time.  A possible
point of confusion here is that the logical stop time is set to 0.25, the
same number given to @code(~~)!  How does setting the logical stop
time to 0.25 result in a tempo change?  When used within a @code(warp)
transformation, the second argument to @code(set-logical-stop) refers to
@i(score) time rather than @i(real) time.  Therefore, the score duration of
0.25 is warped into real time, producing tempo changes according to the
enviroment.  Figure @ref(warp-onset-fig) illustrates the result graphically.

@begin(figure)
@center(@graphic((height = 1.75 in, width = 6.75 in, magnify = 1.0,
	postscript = "warponsetfig.ps")) 
@html(<img src="fig4.gif"><br><br>)
@fillcaption[When @code[(warp4)] is applied
to @code[(tone-seq-3)], the note onsets are warped, but not the duration,
which remains a constant 0.25 seconds.  In the fast middle section, this
causes notes to overlap.  Nyquist will sum (mix) them.]
@tag(warp-onset-fig)
@end(figure)

@section(Nested Transformations)
Transformations can be nested.  In particular, a simple transformation such
as transpose can be nested within a time warp transformation.  Suppose we
want to warp our chromatic scale example with the @code(warp4) time warp
function.  As in the previous section, we will show an erroneous simple
solution followed by a correct one.

The simplest approach to a nested transformation is to simply combine them
and hope for the best:
@begin(example)
play warp(warp4(),
          transpose(pitch-rise(), tone-seq()))
@end(example)
This example will not work the way you might expect.  Here is why: the warp
transformation applies to the @code[(pitch-rise)] expression, which is
implemented using the @code(ramp) function.  The default
behavior of @code(ramp) is to interpolate linearly (in real time) between two points.
Thus, the ``warped'' @code(ramp) function will not truly reflect the internal
details of the intended time warp.  When the notes are moving faster, they
will be closer together in pitch, and the result is not chromatic.
What we need is a way to properly
compose the warp and ramp functions.  If we continuously warp the ramp function
in the same way as the note sequence, a chromatic scale should be obtained.
This will lead to a correct solution.

Here is the modified code to properly warp a transposed sequence.  Note that
the original sequence is used without modification.  The only complication
is producing a properly warped transposition function:
@begin(example)
  play warp(warp4(),
            transpose(
              control-warp(get-warp(),
                           warp-abs(nil, pitch-rise())),
              tone-seq()))
@end(example)
To properly warp the @code(pitch-rise) transposition function, we use
@code(control-warp), which applies a warp function to a function of score time,
yielding a function of real time.  We need to pass the desired function
to @code(control-warp), so we fetch it  from the environment with
@code[get-warp()].  Finally, since the warping is done here, we want to
shield the @code(pitch-rise) expression from further warping, so we enclose
it in @code[warp-abs(nil, ...)].

 @i(An aside:) This last example illustrates a difficulty in the design of
Nyquist.  To support behavioral abstraction universally, we must rely upon
behaviors to ``do the right thing.''  In this case, we would like the
@code(ramp) function to warp continuously according to the environment.  But
this is inefficient and unnecessary in many other cases where @code(ramp)
and especially @code(pwl) are used.  (@code(pwl) warps its breakpoints, but still interpolates linearly between them.)  Also, if the default behavior of
primitives is to warp in a continuous manner, this makes it difficult to
build custom abstract behaviors.  The final vote is not in.

@chapter(More Examples)

This chapter explores Nyquist through additional examples.  The reader may
wish to browse through these and move on to Chapter @ref(lisp-chap), which
is a reference section describing Nyquist functions.

@section(Stretching Sampled Sounds)@index(Stretching Sampled Sounds)

This example illustrates how to stretch a sound, resampling it in the process.
Because sounds in Nyquist are @i(values) that contain the sample rate, start
time, etc., use @code(sound) to convert a sound into a behavior that can be
stretched, e.g. @code[sound(a-snd)]. This behavior stretches a sound according
to the stretch factor in the environment, set using @code(stretch). For
accuracy and efficiency, Nyquist does not resample a stretched sound until
absolutely necessary. The @code(force-srate) function is used to resample
the result so that we end up with a ``normal'' sample rate that is playable
on ordinary sound cards.

@begin(example)
@i[; if a-snd is not loaded, load sound sample:
;]
if not(boundp(quote(a-snd))) then
  set a-snd = s-read("demo-snd.aiff")

@i[; the SOUND operator shifts, stretches, clips and scales 
; a sound according to the current environment
;]
define function ex23()
  play force-srate(*default-sound-srate*,  sound(a-snd) ~ 3.0)

define function down()
  return force-srate(*default-sound-srate*, 
                     seq(sound(a-snd) ~ 0.2,
                         sound(a-snd) ~ 0.3,
                         sound(a-snd) ~ 0.4,
                         sound(a-snd) ~ 0.6))
play down()

@i[; that was so much fun, let's go back up:
;]
define function up()
  return force-srate(*default-sound-srate*,
                     seq(sound(a-snd) ~ 0.5,
                         sound(a-snd) ~ 0.4,
                         sound(a-snd) ~ 0.3,
                         sound(a-snd) ~ 0.2))

@i[; and write a sequence
;]
play seq(down(), up(), down())
@end(example)

Notice the use of the @code(sound) behavior as opposed to @code(cue).  The
@code(cue) behavior shifts and scales its sound according to @code(*warp*)
and @code(*loud*), but it does not change the duration or resample the
sound.  In contrast, @code(sound) not only shifts and scales its sound, but
it also stretches it by resampling or changing the effective sample rate
 according to @code(*warp*).  If
@code[*warp*] is a continuous warping function, then the sound will be
stretched by time-varying amounts.
(The @code(*transpose*) element of the environment is
ignored by both @code(cue) and @code(sound).)  

@p(Note:) @code(sound) may use linear interpolation rather than a high-quality resampling algorithm.  In some cases, this may introduce errors audible as noise. Use @code(resample) (see Section @ref(resample-sec)) for high-quality interpolation.

In the functions @code(up) and @code(down), the @code(*warp*) is set by
@i(stretch) (@code(~)), which simply scales time by a constant scale factor. In this case,
@code(sound) can ``stretch'' the signal simply by changing the sample rate without
any further computation. When @code(seq) tries to add the signals together, it
discovers the sample rates do not match and uses linear interpolation to adjust
all sample rates to match that of the first sound in the sequence. The result of
@code(seq) is then converted using @code(force-srate) to convert the sample rate,
again using linear interpolation. 
It would be slightly better, from a computational
standpoint, to apply @code(force-srate) individually 
to each stretched sound rather
than applying @code(force-srate) after @code(seq).

Notice that the overall duration of @code[sound(a-snd) ~ 0.5] will
be half the duration of @code(a-snd).

@section(Saving Sound Files)@index(Saving Sound Files)

So far, we have used the @code(play) command to play a sound.  The
@code(play) command works by writing a sound to a file while 
simultaneously playing it.
This can be done one step at a time, and
it is often convenient to save a sound to a particular file for later use:
@begin(example)
@i[; write the sample to a file, 
;    the file name can be any Unix filename.  Prepending a "./" tells
;    s-save to not prepend *default-sf-dir*
;]
exec s-save(a-snd, 1000000000, "./a-snd-file.snd")

@i[; play a file
; play command normally expects an expression for a sound
; but if you pass it a string, it will open and play a
; sound file]
play "./a-snd-file.snd"

@i[; delete the file (do this with care!)
; only works under Unix (not Windows)]
exec system("rm ./a-snd-file.snd")

@i[; now let's do it using a variable as the file name
;]
set my-sound-file = "./a-snd-file.snd"

exec s-save(a-snd, 1000000000, my-sound-file)

@i[; play-file is a function to open and play a sound file]
exec play-file(my-sound-file)

exec system(strcat("rm ", my-sound-file))
@end(example)
This example shows how @code(s-save) can be used to save a sound to a file.

This example also shows how the @code(system) function can be used to invoke
Unix shell commands, such as a command to play a file or remove it.
Finally, notice that @code(strcat) can be used to concatenate a command name
to a file name to create a complete command that is then passed to
@code(system).  (This is convenient if the sound file name is stored in a
parameter or variable.)

@section(Memory Space and Normalization)
@label(normalization-sec)
@index(normalization)@index(peak amplitude)@index(maximum amplitude)@index(clip)@index(s-max)@index(s-min)@index(rescaling)@index(not enough memory for normalization)
Sound samples take up lots of memory, and often, there is not enough primary (RAM) memory to hold a complete composition.  For this reason, Nyquist can compute sounds incrementally, saving the final result on disk.  @i(However,) Nyquist can also save sounds in memory so that they can be reused efficiently.  In general, if a sound is saved in a global variable, memory will be allocated as needed to save and reuse it.

The standard way to compute a sound and write it to disk is to pass an expression to the @code(play) command:
@begin(example)
play my-composition()
@end(example)

@label(peak-ex-sec)
Often it is nice to @i(normalize) sounds so that they use the full available
dynamic range of 16 bits.  Nyquist has an automated facility to help with
normalization. By default, Nyquist computes up to 1 million samples (using 
about 4MB of memory) looking for the peak. The entire sound is normalized so
that this peak will not cause clipping. If the sound has less than 1 million
samples, or if the first million samples are a good indication of the overall
peak, then the signal will not clip.

With this automated normalization technique, you can choose the desired 
peak value by setting @code(*autonorm-target*), which is initialized to 0.9.
The number of samples examined is @code(*autonorm-max-samples*), initially
1 million. You can turn this feature off by executing:
@begin(example)
exec autonorm-off()@index(autonorm-off)
@end(example)
and turn it back on by typing:
@begin(example)
exec autonorm-on()@index(autonorm-on)
@end(example)
This normalization technique is in effect when @code(*autonorm-type*) is 
@code(quote(lookahead)), which is the default.

An alternative normalization method uses the peak value from the previous
call to @code(play). After playing a file, Nyquist can adjust an internal
scale factor so that if you play the same file again, the peak amplitude
will be @code(*autonorm-target*), which is initialized to 0.9. This can 
be useful if you want to carefully normalize a big sound that does not
have its peak near the beginning. To select this style of normalization,
set @code(*autonorm-type*) to the (quoted) atom @code(quote(previous)). 

You can also create your own normalization method in Nyquist. 
The @code(peak) function computes the maximum value of a sound.  
The peak value is also returned from the @code(play) macro. You can
normalize in memory if you have enough memory; otherwise you can compute 
the sound twice.  The two techniques are illustrated here:
@begin(example)
@i[; normalize in memory.  First, assign the sound to a variable so
; it will be retained:]
set mysound = sim(osc(c4), osc(c5))
@i[; now compute the maximum value (ny:all is 1 giga-samples, you may want a
; smaller constant if you have less than 4GB of memory:]
set mymax = snd-max(mysound, NY:ALL)
display "Computed max", mymax
@i[; now write out and play the sound from memory with a scale factor:]
play mysound * (0.9 / mymax)

@i[; if you don't have space in memory, here's how to do it:]
define function myscore()
  return sim(osc(c4), osc(c5))
@i[; compute the maximum:]
set mymax = snd-max(list(quote(myscore)), NY:ALL)
display "Computed max", mymax
@i[; now we know the max, but we don't have a the sound (it was garbage
; collected and never existed all at once in memory).  Compute the sound
; again, this time with a scale factor:]
play myscore() * (0.9 / mymax)
@end(example)

You can also write a sound as a floating point file.  This
file can then be converted to 16-bit integer with the proper scaling
applied.  If a long computation was involved, it should be much faster
to scale the saved sound file than to recompute the sound from scratch. 
Although not implemented yet in Nyquist, some header formats can
store maximum amplitudes, and some soundfile player programs can 
rescale floating point files on the fly, allowing normalized 
soundfile playback without an extra normalization pass (but at a cost
of twice the disk space of 16-bit samples).  
You can use Nyquist to rescale a floating point file and
convert it to 16-bit samples for playback.

@section(Frequency Modulation)@index(Frequency Modulation)
The next example uses the Nyquist frequency modulation behavior @code(fmosc)
to generate various sounds.  The parameters to @code(fmosc) are:
@begin(example)
fmosc(@i(pitch) @i(modulator) @i(table) @i(phase))
@end(example)
Note that pitch is the number of half-steps, e.g. @code(c4) has the value of 60 which is middle-C, and phase is in degrees.  Only the first two parameters are required:
@begin(example)
@i[; make a short sine tone with no frequency modulation
;]
play fmosc(c4, pwl(0.1))

@i[; make a longer sine tone -- note that the duration of
;   the modulator determines the duration of the tone
;]
play fmosc(c4, pwl(0.5))
@end(example)
In the example above, @code(pwl) (for Piece-Wise Linear) is used to generate
sounds that are zero for the durations of @code(0.1) and @code(0.5) seconds,
respectively.  In effect, we are using an FM oscillator with no modulation
input, and the result is a sine tone.  The duration of the modulation
determines the duration of the generated tone (when the modulation signal
ends, the oscillator stops).

The next example uses a more interesting modulation function, a ramp from
zero to C@-(4), expressed in hz.  More explanation of @code(pwl) is in
order.  This operation constructs a piece-wise linear function sampled at
the @code(*control-srate*).  The first breakpoint is always at @code[(0,
0)], so the first two parameters give the time and value of the second
breakpoint, the second two parameters give the time and value of the third
breakpoint, and so on.  The last breakpoint has a value of @code(0), so only
the time of the last breakpoint is given.  In this case, we want the ramp to
end at C@-(4), so we cheat a bit by having the ramp return to zero
``almost'' instantaneously between times @code(0.5) and @code(0.501).

The @code(pwl) behavior always expects an odd number of parameters.  The
resulting function is shifted and stretched linearly according to
@code[*warp*] in the environment.  Now, here is the example:
@begin(example)
@i[; make a frequency sweep of one octave; the piece-wise linear function
; sweeps from 0 to (step-to-hz c4) because, when added to the c4
; fundamental, this will double the frequency and cause an octave sweep.
;]
play fmosc(c4, pwl(0.5, step-to-hz(c4),  0.501))
@end(Example)

The same idea can be applied to a non-sinusoidal carrier.  Here, we assume that @code(*fm-voice*) is predefined (the next section shows how to define it):
@begin(example)
@i[; do the same thing with a non-sine table
;]
play fmosc(cs2, pwl(0.5, step-to-hz(cs2), 0.501),
           *fm-voice*, 0.0)
@end(example)

The next example shows how a function can be used to make a special
frequency modulation contour.  In this case the contour generates a sweep
from a starting pitch to a destination pitch:
@begin(example)
@i[; make a function to give a frequency sweep, starting
; after <delay> seconds, then sweeping from <pitch-1>
; to <pitch-2> in <sweep-time> seconds and then
; holding at <pitch-2> for <hold-time> seconds.
;]
define function sweep(delay, pitch-1, sweep-time, 
                      pitch-2, hold-time)
  begin
    with interval = step-to-hz(pitch-2) - step-to-hz(pitch-1)
    return pwl(delay, 0.0,
               @i[; sweep from pitch 1 to pitch 2]
               delay + sweep-time, interval,
               @i[; hold until about 1 sample from the end]
               delay + sweep-time + hold-time - 0.0005, 
               interval,
               @i[; quickly ramp to zero (pwl always does this,]
               @i[;    so make it short)]
               delay + sweep-time + hold-time)
  end


@i[; now try it out
;]
play fmosc(cs2, sweep(0.1, cs2, 0.6, gs2, 0.5),
           *fm-voice*, 0.0)
@end(example)

FM can be used for vibrato as well as frequency sweeps.  Here, we use the
@code(lfo) function to generate vibrato.  The @code(lfo) operation is
similar to @code(osc), except it generates sounds at the
@code(*control-srate*), and the parameter is hz rather than a pitch:
@begin(example)
play fmosc(cs2, 10.0 * lfo(6.0), *fm-voice*, 0.0)
@end(Example)

What kind of manual would this be without the obligatory FM sound?  Here, a
sinusoidal modulator (frequency C@-(4)) is multiplied by a slowly increasing
ramp from zero to @code(1000.0).
@begin(example)
set modulator = pwl(1.0, 1000.0, 1.0005) * 
                osc(c4)
@i[; make the sound]
play fmosc(c4, modulator)
@end(example)

For more simple examples of FM in Nyquist, see 
@index(warble)@index(FM synthesis)@index(demos, FM)@index(tutorial, FM)
@code(demos/warble_tutorial.htm). Another interesting FM sound 
reminiscent of ``scratching'' can be found with a detailed explanation
in @code(demos/scratch_tutorial.htm).@index(demos, scratch tutorial)
@index(vinal scratch)@index(scratch sound).

@section(Building a Wavetable)
In Section @ref(waveform-sec), we saw how to synthesize a wavetable.   A
wavetable for @code(osc) also can be extracted from any sound.  This is
especially interesting if the sound is digitized from some external sound
source and loaded using the @code(s-read) function.  Recall that a table
is a list consisting of a sound, the pitch of that sound, and T (meaning the
sound is periodic).

In the following, a sound is first read from the file @code(demo-snd.nh).
Then, the @code(extract) function is used
to extract the portion of the sound between 0.110204 and 0.13932 seconds.
(These numbers might be obtained by first plotting the sound and estimating
the beginning and end of a period, or by using some software to look for
good zero crossings.)  The result of @code(extract) becomes the first
element of a list.  The next element is the pitch (24.848422), and the last
element is @code(T).  The list is assigned to @code(*fm-voice*).
@begin(example)
if not(boundp(quote(a-snd))) then
  set a-snd = s-read("demo-snd.aiff")

set *fm-voice* = list(extract(0.110204, 0.13932, cue(a-snd)),
                      24.848422,
                      #T)
@end(example)

The file
@i(demos/examples.sal) contains an extensive example of how to locate
zero-crossings, extract a period, build a waveform, and generate a tone from it.  (See @code(ex37) through @code(ex40) in the file.)

@begin(comment)
The @code(maketable) command is also useful and is documented on page
@ref(maketable).  If @code(sample) is the source sound, then the following
will extract 0.01 seconds (starting at time 0.2s) of audio and convert it
into a waveform named @code(mytable):
@begin(example)
(setf mytable (maketable (extract -abs 0.2 0.21 sample)))
@end(example)
Nyquist does not provide any pitch analysis or other help finding good splice points, so it is up to you to make sure the table you extract is a reasonable one.
@end(comment)

@section(Filter Examples)

Nyquist provides a variety of filters.  All of these filters take either
real numbers or signals as parameters.  If you pass a signal as a filter
parameter, the filter coefficients are recomputed at the sample rate of the
@i(control) signal.  Since filter coefficients are generally expensive to
compute, you may want to select filter control rates carefully.  Use
@code(control-srate-abs) (Section @ref(control-srate-abs-sec)) to specify
the default control sample rate, or use @code(force-srate) (Section
@ref(force-srate-sec)) to resample a signal before passing it to a filter.  

Before presenting examples, let's generate some unfiltered white noise:
@begin(example)
play noise()
@end(example)
Now low-pass filter the noise with a 1000Hz cutoff:
@begin(example)
play lp(noise(), 1000.0)
@end(example)
The high-pass filter is the inverse of the low-pass:
@begin(example)
play hp(noise(), 1000.0)
@end(example)

Here is a low-pass filter sweep from 100Hz to 2000Hz:
@begin(example)
play lp(noise(), pwl(0.0, 100.0, 1.0, 2000.0, 1.0))
@end(example)
And a high-pass sweep from 50Hz to 4000Hz:
@begin(example)
play hp(noise(), pwl(0.0, 50.0, 1.0, 4000.0, 1.0))
@end(example)

The band-pass filter takes a center frequency and a bandwidth parameter.
This example has a 500Hz center frequency with a 20Hz bandwidth.  The scale
factor is necessary because, due to the resonant peak of the filter, the
signal amplitude exceeds 1.0:
@begin(example)  
play reson(10.0 * noise(), 500.0, 20.0, 1)
@end(example)
In the next example, the center frequency is swept from 100 to 1000Hz, using a constant 20Hz bandwidth:
@begin(example)
play reson(0.04 * noise(),
           pwl(0.0, 200.0, 1.0, 1000.0, 1.0),
           20.0)
@end(example)

For another example with explanations, see 
@index(wind_tutorial.htm)@index(wind sound)@index(filter example)
@index(demos, wind sound)
@code(demos/wind_tutorial.htm).


@section(DSP in Lisp)
@index(DSP in Lisp)@index(Lisp DSP)In almost any 
signal processing system, the vast majority of computation
takes place in the inner loops of DSP algorithms, and Nyquist is designed so
that these time-consuming inner loops are in highly-optimized
machine code rather than relatively slow interpreted lisp code. As a result,
Nyquist typically spends 95% of its time in these inner loops; the overhead
of using a Lisp interpreter is negligible.

The drawback is that Nyquist must provide the DSP operations you need, or
you are out of luck. When Nyquist is found lacking, you can either write a
new primitive signal operation, or you can perform DSP in Lisp code. Neither
option is recommended for inexperienced programmers. Instructions for
extending Nyquist are given in Appendix @ref(extending-app). This section
describes the process of writing a new signal processing function in Lisp.

Before implementing a new DSP function, you should decide which approach is
best. First, figure out how much of the new function can be implemented
using existing Nyquist functions. For example, you might think that a
tapped-delay line would require a new function, but in fact, it can be
implemented by composing sound transformations to accomplish delays, scale
factors for attenuation, and additions to combine the intermediate results.
This can all be packaged into a new Lisp function, making it easy to use.
If the function relies on built-in DSP primitives, it will execute very
efficiently.

Assuming that built-in functions cannot be used, try to define a new
operation that will be both simple and general. Usually, it makes sense to
implement only the kernel of what you need, combining it with existing
functions to build a complete instrument or operation.  For example, if you
want to implement a physical model that requires a varying breath pressure
with noise and vibrato, plan to use Nyquist functions to add a basic
pressure envelope to noise and vibrato signals to come up with a composite
pressure signal. Pass that signal into the physical model rather than
synthesizing the envelope, noise, and vibrato within the model. This not
only simplifies the model, but gives you the flexibility to use all of
Nyquist's operations to synthesize a suitable breath pressure signal.

Having designed the new ``kernel'' DSP operation that must be implemented,
decide whether to use C or Lisp. (At present, SAL is not a good option
because it has no support for object-oriented programming.) 
To use C, you must have a C compiler, the
full source code for Nyquist, and you must learn about extending Nyquist by
reading Appendix @ref(extending-app). This is the more complex approach, but
the result will be very efficient. A C implementation will deal properly
with sounds that are not time-aligned or matched in sample rates.
To use Lisp, you must learn something
about the XLISP object system, and the result will be about 50 times slower
than C. Also, it is more difficult to deal with time alignment and
differences in sample rates.
The remainder of this section gives an example of a Lisp version of
@code(snd-prod) to illustrate how to write DSP functions for Nyquist in Lisp.

The @code(snd-prod) function is the low-level multiply routine. It has two
sound parameters and returns a sound which is the product of the two. To
keep things simple, we will assume that two sounds to be multiplied have a
matched sample rate and matching start times. The DSP algorithm for each
output sample is simply to fetch a sample from each sound, multiply them,
and return the product.

To implement @code(snd-prod) in Lisp, three components are required:
@begin(enumerate)
An object is used to store the two parameter sounds. This object will be
called upon to yield samples of the result sound;

Within the object, the @code(snd-fetch) routine is used to fetch samples
from the two input sounds as needed;

The result must be of type @code(SOUND), so @code(snd-fromobject) is used
to create the result sound.
@end(enumerate)

The combined solution will work as follows: The result is a value of type
@code(sound) that retains a reference to the object.  When Nyquist needs
samples from the sound, it invokes the sound's ``fetch'' function, which in
turn sends an XLISP message to the object. The object will use
@code(snd-fetch) to get a sample from each stored sound, multiply the
samples, and return a result.

Thus the goal is to design an XLISP object that, in response to a
@code(:next) message will return a proper sequence of samples.  When the
sound reaches the termination time, simply return @code(NIL).

The XLISP manual (see Appendix @ref(XLISP-app) describes the object system,
but in a very terse style, so this example will include some explanation of
how the object system is used. First, we need to define a class for the
objects that will compute sound products. Every class is a subclass of class
@code(class), and you create a subclass by sending @code(:new) to a class.
@begin(example)
(setf product-class (send class :new '(s1 s2)))
@end(example)
The parameter @code['(s1 s2)] says that the new class will have two instance
variables, @code(s1) and @code(s2). In other words, every object which is an
instance of class @code(product-class) will have its own copy of 
these two variables.

Next, we will define the @code(:next) method for @code(product-class):
@begin(example)
(send product-class :answer :next '()
  '((let ((f1 (snd-fetch s1))
          (f2 (snd-fetch s2)))
      (cond ((and f1 f2)
             (* f1 f2))
            (t nil)))))
@end(example)
The @code(:answer) message is used to insert a new method into our new
@code(product-class). The method is described in three parts: the name
(@code(:next)), a parameter list (empty in this case), and a list of
expressions to be evaluated. In this case, we fetch samples from @code(s1)
and @code(s2). If both are numbers, we return their product. If either is
@code(NIL), we terminate the sound by returning @code(nil).

The @code(:next) method assumes that @code(s1) and @code(s2) hold the sounds
to be multiplied. These must be installed when the object is created.
Objects are created by sending @code(:new) to a class. A new object is
created, and any parameters passed to @code(:new) are then sent in a
@code(:isnew) message to the new object. Here is the @code(:isnew)
definition for @code(product-class):
@begin(example)
(send product-class :answer :isnew '(p1 p2) 
  '((setf s1 (snd-copy p1))
    (setf s2 (snd-copy p2))))
@end(example)
Take careful note of the use of @code(snd-copy) in this initialization. The
sounds @code(s1) and @code(s2) are modified when accessed by
@code(snd-fetch) in the @code(:next) method defined above, but this destroys
the illusion that sounds are immutable values. The solution is to copy the
sounds before accessing them; the original sounds are therefore unchanged.
(This copy also takes place implicitly in most Nyquist sound functions.)

To make this code safer for general use, we should add checks that @code(s1)
and @code(s2) are sounds with identical starting times and sample rates;
otherwise, an incorrect result might be computed.

Now we are ready to write @code(snd-product), an approximate replacement for
@code(snd-prod):
@begin(example)
(defun snd-product (s1 s2)
  (let (obj)
    (setf obj (send product-class :new s1 s2))
    (snd-fromobject (snd-t0 s1) (snd-srate s1) obj)))
@end(example)
This code first creates @code(obj), an instance of @code(product-class), to
hold @code(s1) and @code(s2). Then, it uses @code(obj) to create a sound
using @code(snd-fromobject). This sound is returned from
@code(snd-product).  Note that in @code(snd-fromobject), you must also
specify the starting time and sample rate as the first two parameters. These
are copied from @code(s1), again assuming that @code(s1) and @code(s2) have
matching starting times and sample rates.

Note that in more elaborate DSP algorithms we could expect the object to
have a number of instance variables to hold things such as previous samples,
waveform tables, and other parameters.

@chapter(SAL)
@label(SAL-chap)
Nyquist supports two languages: XLISP and SAL. In some sense, XLISP and SAL
are the same language, but with differing syntax. This chapter describes SAL: how it works, SAL syntax and semantics, and the relationship between SAL and XLISP, and differences between Nyquist SAL and Common Music SAL.

Nyquist SAL is based on Rick Taube's SAL language, which is
part of Common Music. SAL offers the power
of Lisp but features a simple, Algol-like syntax. SAL is implemented
in Lisp: Lisp code translates SAL into a Lisp program and uses the
underlying Lisp engine to evaluate the program. Aside from the translation
time, which is quite fast, SAL programs execute at about the same speed as
the corresponding Lisp program. (Nyquist SAL programs run just
 slightly slower than XLISP
because of some runtime debugging support automatically added to
user programs by the SAL compiler.)

From the user's perspective, these implementation details are hidden. You 
can enter SAL mode from XLISP by typing @code[(SAL)] to the XLISP prompt.
The SAL input prompt (@code(SAL> )) will be displayed. From that point on,
you simply type SAL commands, and they will be executed. By setting a 
preference in the NyquistIDE program, SAL mode will be entered automatically.

It is possible to encounter errors that will take you from the SAL interpreter
to an XLISP prompt. In general, the way to get back to SAL is by typing 
@code[(top)] to get back to the top level XLISP interpreter and reset the
Nyquist environment. Then type @code[(sal)] to restart the SAL interpreter.

@section(SAL Syntax and Semantics)
@index(SAL)
 The most unusual feature of SAL syntax is that identifiers
are Lisp-like, including names such as ``play-file'' and even ``*warp*.''
In SAL, most operators must be separated from identifiers by white space.
For example, @code(play-file) is one identifier, but @code(play - file)
is an expression for ``play minus file,'' where @code(play) and @code(file) are
two separate identifiers. Fortunately, no spaces are needed around commas
and parentheses.

In SAL, whitespace (any sequence of space, newline, or tab characters)
is sometimes necessary to separate lexical tokens, but
otherwise, spaces and indentation are ignored. To make SAL readable,
it is @i(strongly) advised that you indent SAL programs as in the examples
here. The NyquistIDE program is purposely insistent about SAL indentation,
so if you use it to edit SAL programs, your indentation should be 
both beautiful and consistent.

As in Lisp (but very unlike C or Java), comments @index(comments)
are indicated by
semicolons. Any text from an unquoted semicolon to the end of the 
line is ignored.

@begin(example)
@i(; this is a comment)
@i(; comments are ignored by the compiler)
print "Hello World" @i(; this is a SAL statement)
@end(example)

As in Lisp, identifiers are translated to upper-case, making SAL 
case-insensitive@index(case-insensitive). For example, the function name @code(autonorm) can
be typed in lower case or as @code(AUTONORM), @code(AutoNorm), or even 
@code(AuToNoRm). All forms denote the same function. The recommended
approach is to write programs in all lower case.

SAL is organized around statements, most of which
contain expressions. We will begin with expressions and then look at
statements.

@subsection(Expressions)
@index(sal expressions)@index(expressions, sal)
@paragraph(Simple Expressions)
As in XLISP, simple expressions include:
@begin(itemize)
integers (FIXNUM's), such as @code(1215), 

floats (FLONUM's) such as @code(12.15), 

strings (STRING's) such as @code("Magna Carta"), and

symbols (SYMBOL's) such as @code(magna-carta). A symbol with a leading colon 
(@code(:)) evaluates to itself as in Lisp. Otherwise, a symbol denotes either
a local variable, a formal parameter, or a global variable. As in Lisp, 
variables do not have data types or type declarations. The type of a 
variable is determined at runtime by its value.
@end(itemize)

Additional simple expressions in SAL are:
@begin(itemize)
lists such as @code[{c 60 e 64}]. Note that there are no commas to separate list elements, and symbols in lists are not evaluated as variables but stand for themselves. Lists may contain numbers, booleans, symbols, strings, and other lists.

Booleans: SAL interprets @code(#t)@index(#t) as true and @code(#f)@index(#f)
as false. (As far
as the SAL compiler is concerned, @code(t) and @code(nil) are just variables.
Since these are the Lisp versions of true and false, they are interchangeable
with @code(#t) and @code(#f), respectively.)
@end(itemize)
A curious property of Lisp and Sal is that @i(false) and the empty list are 
the same value. Since SAL is based on Lisp, @code(#f) and @code({}) (the empty
list)@index(empty list) are equal.

@paragraph(Operators)
Expressions can be formed with unary and binary operators using infix notation. The operators are:
@begin(itemize)
@index(+)@code(+) - addition, including sounds

@index(-)@code(-) - subtraction, including sounds

@index(*)@code(*) - multiplication, including sounds

@index(/)@code(/) - division (due to divide-by-zero problems, does not operate on sounds)

@index(%)@code(%) - modulus (remainder after division)

@index(^)@code(^) - exponentiation

@index(=)@code(=) - equal (using Lisp @code(eql))

@index(!=)@code(!=) - not equal

@index(>)@code(>) - greater than

@index(<)@code(<) - less than

@index(>=)@code(>=) - greater than or equal

@index(<=)@code(<=) - less than or equal

@index(~=)@code(~=) - general equality (using Lisp @code(equal))

@index(&)@code(&) - logical and

@index(|)@code(|) - logical or

@index(!)@code(!) - logical not (unary)

@index(@@)@index(time shift, sal)@index(at, sal)@code(@@) - time shift

@index(@@@@)@index(absolute time shift, sal)@index(at-abs, sal)@code(@@@@) - time shift to absolute time

@index(~)@index(stretch, sal)@code(~) - time stretch

@index(~~)@index(absolute stretch, sal)@code(~~) - time stretch to absolute stretch factor
@end(itemize)
Again, remember that operators @i(must) be delimited from their operands using
spaces or parentheses. Operator precedence is based on the following levels of 
precedence:
@begin(example)
@@ @@@@ ~ ~~
^
/ * 
% - +
~= <= >= > ~= =
!
&
|
@end(example)

@paragraph(Function Calls)
@index(function calls, sal)
A function call is a function name followed by zero or more comma-delimited
argument expressions
enclosed within parentheses:
@begin(example)
list()
piano-note(2.0, c4 + interval, 100)
@end(example)
Some functions use named parameters,
in which case the name of the argument with a colon precedes the argument
expression.
@begin(example)
s-save(my-snd(), ny:all, "tmp.wav", play: #t, bits: 16)
@end(example)

@paragraph(Array Notation)
@index(array notation, sal)
An array reference is a variable identifier followed by an index expression
in square brackets, e.g.:
@begin(example)
x[23] + y[i]
@end(example)

@paragraph(Conditional Values)
@index(conditional expression, sal)
@index(#?, sal)
The special operator @code(#?) evaluates the first argument expression. 
If the result is @i(true), the second expression is evaluated and
its value is returned. If @i(false), the third expression is evaluated
and returned (or @i(false) is returned if there is no third expression):
@begin(example)
#?(random(2) = 0, unison, major-third)
#?(pitch >= c4, pitch - c4) ; returns false if pitch < c4
@end(example)

@subsection(SAL Statements)
@index(statements, sal)
SAL compiles and evaluates @i(statements) one at a time. You can type
statements at the SAL prompt or load a file containing SAL statements.
SAL statements are described below. The syntax is indicated at the
beginning of each statement type description: @code(this font) indicates
literal terms such as keywords, @i(the italic font) indicates a 
place-holder for some other statement or expression. Bracket [like this]
indicate optional (zero or one) syntax elements, while braces with a plus
{like this}+ indicate one or more occurrences of a syntax element. Braces
with a star {like this}* indicate zero or more occurrences of a syntax element: { @i(non-terminal) }* is equivalent to [ {@i(non-terminal)}+ ].

@paragraph(begin and end)
@index(begin)@index(end)
@code(begin) [@i(with-stmt)] {@i(statement)}+ @code(end)

A @code(begin)-@code(end) statement 
consists of a sequence of statements surrounded by
the @code(begin) and @code(end) keywords. This form is often used for function
definitions and after @code(then) or @code(else) where the syntax demands a
single statement but you want to perform more than one action. Variables may be
declared using an optional @code(with) statement immediately after @code(begin).
For example:
@begin(example)
begin
  with db = 12.0,
       linear = db-to-linear(db)
  print db, "dB represents a factor of", linear
  set scale-factor = linear
end  
@end(example)

@paragraph(chdir)
@index(chdir, sal)
@code(chdir) @i(expression)

The @code(chdir) statement changes the working directory. This statement
is provided for compatibility with Common Music SAL, but it really
should be avoided if you use NyquistIDE. The @i(expression) following the
@code(chdir) keyword should evaluate to a string that is a directory
path name. Note that literal strings themselves are valid expressions.
@begin(example)
chdir "/Users/rbd/tmp"
@end(example)

@paragraph(define variable)
@index(global variables, sal)@index(define variable)
[@code(define)] @code(variable) @i(name) [= @i(expression)] {, @i(name) [= @i(expression)]}*

Global variables can be declared and initialized. A list of variable names,
each with an optional initialization follows the @code(define variable) 
keywords. (Since @code(variable) is a keyword, @code(define) is redundant
and optional in Nyquist SAL, but required in Common Music SAL.)
If the initialization part is omitted, the variable is initialized
to false. Global variables do not really need to be declared: just using the
name implicitly creates the corresponding variable. However, it is an error
to use a global variable that has not been initialized;
@code(define variable) is a good way to introduce a variable (or constant)
with an initial value into your program.

@begin(example)
define variable transposition = 2,
                print-debugging-info, @i(; initially false)
                output-file-name = "salmon.wav"
@end(example)

@paragraph(define function)
@index(function, sal)@index(define function)
[@code(define)] @code(function) @i(name) @code[(] [@i(parameter)], {, @i(parameter)}* @code[)] @i(statement)

Before a function be called from an expression (as described above), it must
be defined. A function definition gives the function @i(name), a list of
@i(parameters), and a @i(statement). When a function is called, the actual
parameter expressions are evaluated from left to right and the formal parameters
of the function definition are set to these values. Then, @i(statement) is 
evaluated. 

The formal parameters may be positional parameters that are matched with
actual parameters by position from left to right. Syntactically, these are
symbols and these symbols
are essentially local variables that exist only until @i(statement) completes 
or a @code(return) statement causes the function evaluation to end. As in Lisp,
parameters are passed by value, so assigning a new value to a formal parameter
has no effect on the actual value. However, lists and arrays are not copied,
so internal changes to a list or array produce observable side effects.

Alternatively, formal parameters may be keyword parameters. Here the @i(parameter)
is actually a pair: a keyword parameter, which is a symbol followed by a colon,
and a default value, given by any expression. Within the body of the function,
the keyword parameter is named by a symbol whose name matches the keyword
parameter except there is no final colon.
@begin(example)
define function foo(x: 1, y: bar(2, 3))
    display "foo", x, y

exec foo(x: 6, y: 7)
@end(example)
In this example, @code(x) is bound to the value 6 and @code(y) is bound to
the value 7, so the example prints ``@code(foo : X = 6, Y = 7)''. Note that
while the keyword parameters are @code(x:) and @code(y:), the corresponding
variable names in the function body are @code(x) and @code(y), respectively.

The @i(parameters) are meaningful only within the lexical (static) scope of
@i(statement). They are not accessible from within other
functions even if they are called by this function.

Use a @code(begin)-@code(end) statement if the body of the function should
contain more than one statement or you need to define local variables. Use 
a @code(return) statement to return a value from the function. If @i(statement)
completes without a @code(return), the value false is returned.

@paragraph(display)
@index(display statement, sal)
@code(display) @i(string) {, @i(expression)}*

The @code(display) statement is handy for debugging. At present, it is only
implemented in Nyquist SAL. When executed, @code(display) prints the @i(string)
followed by a colon and then, for each @i(expression), the expression and its
value are printed, after the last expression, a newline is printed. For example,
@begin(example)
display "In function foo", bar, baz
@end(example)
prints
@begin(example)
In function foo : bar = 23, baz = 5.3
@end(example)
SAL may print the expressions using Lisp syntax, e.g. if the expression is
``bar + baz,'' do not be surprised if the output is ``@code[(sum bar baz) = 28.3].''

@paragraph(exec)
@index(exec statement, sal)
@code(exec) @i(expression)

Unlike most other programming languages, you cannot simply type an expression as
a statement. If you want to evaluate an expression, e.g. call a function, 
you must use an @code(exec) statement. The statement simply evaluates 
the @i(expression). For example,
@begin(example)
exec set-sound-srate(22050.0) @i(; change default sample rate)
@end(example)

@paragraph(if)
@index(if statement, sal)
@code(if) @i(test-expr) @code(then) @i(true-stmt) [@code(else) @i(false-stmt)]

An @code(if) statement evaluates the expression @i(test-expr). If it is true,
it evaluates the statement @i(true-stmt). If false, the statement
@i(false-stmt) is evaluated. Use a @code(begin)-@code(end) statement
to evaluate more than one statement in then @code(then) or @code(else)
parts.

@begin(example)
if x < 0 then x = -x @i(; x gets its absoute value)

if x > upper-bound then
  begin
    print "x too big, setting to", upper-bound
    x = upper-bound
  end
else
  if x < lower-bound then
    begin
      print "x too small, setting to", lower-bound
      x = lower-bound
    end
@end(example)
Notice in this example that the @code(else) part is another @code(if)
statement. An @code(if) may also be the @code(then) part of another
@code(if), so there could be two possible @code(if)'s with which to
associate an @code(else). An @code(else) clause always associates
with the closest previous @code(if) that does not already have an
@code(else) clause.

@paragraph(when)
@code(when) @i(test) @i(statement)

The @code(when) statement is similar to @code(if), but there is no @code(else) clause.

@begin(example)
when *debug-flag* print "you are here"
@end(example)

@paragraph(unless)
@code(unless) @i(test) @i(statement)

The @code(unless) statement is similar to @code(when) (and @code(if)) but the
@i(statement) is executed when the @i(test) expression is @i(false).

@begin(example)
unless count = 0 set average = sum / count
@end(example)

@paragraph(load)
@index(load statement, sal)
@code(load) @i(expression)

The @code(load) command loads a file named by @i(expression), which must
evauate to a string path name for the file. To load a file, SAL interprets
each statement in the file, stopping when the end of the file or an error
is encountered. If the file ends in @code(.lsp), the file is assumed to 
contain Lisp expressions, which are evaluated by the XLISP interpreter.
In general, SAL files should end with the extension @code(.sal).

@paragraph(loop)
@index(loop statement, sal)
@code(loop) [@i(with-stmt)] {@i(stepping)}* {@i(stopping)* @i(action)+ [@i(finally)] @code(end)

The @code(loop) statement is by far the most complex statement in SAL, but
it offers great flexibility for just about any kind of iteration. The basic
function of a loop is to repeatedly evaluate a sequence of @i(action)'s which 
are statements. Before the loop begins, local variables may be declared in 
@i(with-stmt), a @code(with) statement. 

The @i(stepping) clauses do several
things. They introduce and initialize additional local variables similar
to the @i(with-stmt).
However, these local variables are updated to new values after the @i(action)'s.
In addition, some @i(stepping) clauses have associated stopping conditions, 
which are tested on each iteration @i(before) evaluating the @i(action)'s.

There are also @i(stopping) clauses that provide additional tests to
stop the iteration. These are also evaluated and tested
on each iteration before evaluating the @i(action)'s.

When some @i(stepping) or @i(stopping) condition causes the iteration to stop,
the @i(finally) clause is evaluated (if present). Local variables and their
values can still be accessed in the @i(finally) clause. After the @i(finally)
clause, the @code(loop) statement completes.

The @i(stepping) clauses are the following:
@begin(description)
@code(repeat) @i(expression)@\Sets the number of iterations to the value of @i(expression), which should be an integer (FIXNUM).

@code(for) @i(var) = @i(expression) [ @code(then) @i(expr2) ]@\Introduces a new local
variable named @i(var) and initializes it to @i(expression). Before each subsequent
iteration, @i(var) is set to the value of @i(expr2). If the @code(then) part is
omitted, @i(expression) is re-evaluated and assigned to @i(var)
on each subsequent iteration. Note that this differs from a @i(with-stmt) where
expressions are evaluated and variables are only assigned their values once.

@code(for) @i(var) @code(in) @i(expression)@\Evaluates @i(expression) to 
obtain a list and creates a new local variable initialized to the first element
of the list. After each iteration, @i(var) is assigned the next element of the
list. Iteration stops when @i(var) has assumed all values from the list. If the
list is initially empty, the loop @i(action)'s are not evaluated (there are zero
iterations).

@code(for) @i(var) [@code(from) @i(from-expr)] [[@code(to) | @code(below) | @code(downto) | @code(above)] @i(to-expr)] [@code(by) @i(step-expr)]@\Introduces a new local variable named @i(var) and intialized
to the value of the expression @i(from-expr) (with a default value of 0). After
each iteration of the loop, @i(var) is incremented by the value 
of @i(step-expr) (with a default value of 1). 
The iteration ends when @i(var) is greater than 
the value of @i(to-expr) if there is a @code(to) clause, 
greater than or equal to the value of @i(to-expr) 
if there is a @code(below) clause, 
less than the value of @i(to-expr) if there is a @code(downto) clause, 
or less than or equal to the value of @i(to-expr) if there is a @code(above)
clause. (In the cases of @i(downto) and @i(above), the default increment value
is -1. If there
is no @code(to), @code(below), @code(downto), @code(above), or @code(below) clause, no interation stop test is created for this
stepping clause.
@end(description)

The @i(stopping) clauses are the following:
@begin(description)
@code(while) @i(expression)@\The iterations are stopped when @i(expression) evaluates to @i(false). Anything not false is considered to mean true.

@code(until) @i(expression)@\The iterations are stopped when @i(expression) evaluates to @i(true).
@end(description)

The @i(finally) clause is defined as follows:
@index(finally clause, sal)
@begin(description)
@code(finally) @i(statement)@\The @i(statement) is evaluated when one of the 
@i(stepping) or @i(stopping) clauses ends the loop. As always, @i(statement) may
be a @code(begin)-@code(end) statement. If an @i(action) evaluates a @code(return)
statement, the @code(finally) statement is not executed.
@end(description)

@index(loop examples, sal)Loops often fall into common patterns, such as iteratiing a fixed number of
times, performing an operation on some range of integers, collecting results
in a list, and linearly searching for a solution. These forms are illustrated
in the examples below.

@begin(example)
@i(; iterate 10 times)
loop
  repeat 10
  print random(100)
end

@i(; print even numbers from 10 to 20
; note that 20 is printed. On the next iteration,
;   i = 22, so i >= 22, so the loop exits.)
loop
  for i from 10 to 22 by 2
  print i
end

@i(; collect even numbers in a list)
loop
  with lis
  for i = 0 to 10 by 2
  set lis @@= i @i(; push integers on front of list,)
               @i(; which is much faster than append,)
               @i(; but list is built in reverse)
  finally result = reverse(lis)
end
@i(; now, the variable result has a list of evens)

@i(; find the first even number in a list)
result = #f @i(; #f means "false")
loop
  for elem in lis
  until evenp(elem)
  finally result = elem
end
@i[; result has first even value in lis (or it is #f)]
@end(example)

@paragraph(print)
@index(print statement, sal)
@code(print) @i(expr) {, @i(expr)}*

The @code(print) statement prints the values separated by
spaces and followed by a newline. [Note that in the original
SAL, the newline is printed @i(before) the values, not after.]


@begin(example)
print "The value of x is", x
@end(example)

@paragraph(return)
@index(return statement, sal)
@code(return) @i(expression)

The @code(return) statement can only be used inside a function. It evaluates
@i(expression) and then the function returns the value of the expression
to its caller.

@paragraph(set)
@index(set statement, sal)
@code(set) @i(var) @i(op) @i(expression) {, @i(var) @i(op) @i(expression)}*

The @code(set) statement changes the value of a variable @i(var) according
to the operator @i(op) and the value of the @i(expression). The operators are:
@begin(description)
@code(=)@\The value of @i(expression) is assigned to @i(var).

@index(+=)@code(+=)@\The value of @i(expression) is added to @i(var).

@index(*=)@code(*=)@\The value of @i(var) is multiplied by the value of the expression.

@index(&=)@code(&=)@\The value of @i(expression) is inserted as the last element of
the list referenced by @i(var). If @i(var) is the empty list (denoted by @code(#f)),
then @i(var) is assigned a newly constructed list of one element, the value
of @i(expression).

@index(^=)@code(^=)@\The value of @i(expression), a list, is appended to the list referenced
by @i(var). If @i(var) is the empty list (denoted by @code(#f)), then @i(var)
is assigned the (list) value of @i(expression).

@index(@@=)@code(@@=)@\Pushes the value of @i(expression) onto the front of the list
referenced by @i(var). If @i(var) is empty (denoted by @code(#f)), then @i(var) 
is assigned a newly constructed list of one element, the value of @i(expression).

@index(<=)@code(<=)@\Sets the new value of @i(var) to the minimum of the old value 
of @i(var) and the value of @i(expression).

@index(>=)@code(>=)@\Sets the new value of @i(var) to the maximum of the old value 
of @i(var) and the value of @i(expression).
@end(description)

@begin(example)
@i(; example from Rick Taube's SAL description)
loop
  with a, b = 0, c = 1, d = {}, e = {}, f = -1, g = 0
  for i below 5
  set a = i, b += 1, c *= 2, d &= i, e @@= i, f <= i, g >= i
  finally display "results", a, b, c, d, e, f, g
end
@end(example)

@paragraph(with)
@index(with statement, sal)
@code(with) @i(var) [= @i(expression)] {, @i(var) [= @i(expression)]}*

The @code(with) statement declares and initializes local variables. It 
can appear only after @code(begin) or @code(loop). If the @i(expression) is
omitted, the initial value is @i(false). The variables are visible only 
inside the @code(begin)-@code(end) or @code(loop) statement where the 
@code(with) statement appears. Even in @code(loop)'s the variables 
are intialized only when the loop is entered, not on each iteration.

@paragraph(exit)
@index(exit statement, sal)
@code(exit) [@code(nyquist)]

The @code(exit) statement is unique to Nyquist SAL. It returns from SAL 
mode to the XLISP interpreter. (Return to SAL mode by typing ``@code[(sal)]'').
If @code(nyquist) is included in the statement, then the entire Nyquist
process will exit.

@section(Interoperability of SAL and XLISP)
@index(sal and lisp)@index(interoperability, sal and lisp)
@label(sal-vs-lisp-section)
When SAL evaluatas command or loads files, it translates SAL into XLISP.
You can think of SAL as a program that translates everything you write
into XLISP and entering it for you. Thus, when you define a SAL function,
the function actually exists as an XLISP function (created using
Lisp's @code(defun) special form). When you set or evaluate global variables
in SAL, these are exactly the same Lisp global variables. Thus, XLISP
functions can call SAL functions and vice-versa. At run time, 
everything is Lisp.

@subsection(Function Calls)
In general, there is a very simple translation from SAL to Lisp syntax
and back. A function call is SAL, for example, 
@begin(example)
osc(g4, 2.0)
@end(example)
is translated to Lisp by moving the open parenthesis in front of the
function name and removing the commas:
@begin(example)
(osc g4 2.0)
@end(example)
Similarly, if you want to translate a Lisp function call to SAL, just
reverse the translation.

@subsection(Symbols and Functions)
SAL translates keywords with trailing colons (such as @code(foo:))
into Lisp keywords with leading colons (such as @code(:foo)), but
SAL keywords are not treated as expressions as they are in Lisp.
You cannot write @code[open("myfile.txt", direction: output:)]
because SAL expects an expression after direction. A special form
@code(keyword) is defined to generate a Lisp keyword as an 
expression. The argument is the keyword @i(without) a colon, e.g.
@code[open("myfile.txt", direction: keyword(output))]. Alternatively, 
you can write the Lisp-style keyword with the leading colon, e.g.
@code[open("myfile.txt", direction: :output)].

In Nyquist SAL, the hash character (#), can be used as a prefix to a 
Lisp function name. For example, the following command is not legal
because @code(print) is a SAL command name, not a legal function name:
@code[set v = append(print(a), print(b))]. (Here the intent is to print
arguments to append). However, you can use the hash character to access
the Lisp @code(print) function: @code[set v = append(#print(a), #print(b))].

@subsection(Playing Tricks On the SAL Compiler)
In many cases, the close coupling between SAL and XLISP gives SAL
unexpected expressive power. A good example is @code(seqrep). This
is a special looping construct in Nyquist, implemented as a macro in
XLISP. In Lisp, you would write something like:
@begin(example)
(seqrep (i 10) (pluck c4))
@end(example)
One might expect SAL would have to define a special @code(seqrep)
statement to express this, but since statements do not return values,
this approach would be problematic. The solution (which is already
fully implemented in Nyquist) is to define a
new macro @code(sal-seqrep) that is equivalent to @code(seqrep) 
except that it is called as follows:
@begin(example)
(sal-seqrep i 10 (pluck c4))
@end(example)
The SAL compiler automatically translates the identifier @code(seqrep) to 
 @code(sal-seqrep). Now, in SAL, you can just write
@begin(example)
seqrep(i, 10, pluck(c4))
@end(example)
which is translated in a pretty much semantics-unaware fashion to
@begin(example)
(sal-seqrep i 10 (pluck c4))
@end(example)
and viola!, we have Nyquist control constructs in SAL even though SAL
is completely unaware that @code(seqrep) is actually a special form.

@chapter(Nyquist Functions)
@label(lisp-chap)
This chapter provides a language reference for Nyquist.  Operations
are categorized by functionality and abstraction level.
Nyquist is implemented in two important levels: the ``high level'' supports
behavioral abstraction, which means that operations like @code(stretch) and
@code(at) can be applied.  These functions are the ones that typical users
are expected to use, and most of these functions are written in XLISP.

The ``low-level'' primitives directly operate on sounds, but know nothing of
environmental variables (such as @code(*warp*), etc.).  The
names of most of these low-level functions start with ``@code(snd-)''.  In
general, programmers should avoid any function with the ``@code(snd-)''
prefix.  Instead, use the ``high-level'' functions, which know about the
environment and react appropriately.  The names of high-level functions
do not have prefixes like the low-level functions.  

There are certain low-level operations that apply directly to sounds (as
opposed to behaviors) and are relatively ``safe'' for ordinary use.  These
are marked as such.

Nyquist uses both linear frequency and equal-temperament pitch numbers to
specify repetition rates.  Frequency is always specified in either cycles
per second (hz), or pitch numbers, also referred to as ``steps,'' as in
steps of the chromatic scale.  Steps are floating point numbers such that 60
= Middle C, 61 = C#, 61.23 is C# plus 23 cents, etc.  The mapping from pitch
number to frequency is the standard exponential conversion, and fractional
pitch numbers are allowed:
@pragma(startscribe)
@math[frequency = 440 @mult 2@+{(pitch - 69)/12}].
@pragma(endscribe)
@html[@center{frequency = 440 * 2^((pitch - 69)/12)}]
There are many
predefined pitch names.  By default these are tuned in equal temperament,
with A4 = 440Hz, but these may be changed.  (See Section @ref(constants-sec)).

@section(Sounds)@index(Sounds)
A sound is a primitive data type in Nyquist.  Sounds can be created, passed
as parameters, garbage collected, printed, and set to variables just like
strings, atoms, numbers, and other data types.  

@subsection(What is a Sound?)
Sounds have 5 components:
@begin(itemize)
@code(srate@index(srate)) @itemsep the sample rate of the sound.

@code(samples@index(samples)) @itemsep the samples.

@code(signal-start@index(signal-start)) @itemsep the time of the first sample.

@code(signal-stop@index(signal-stop)) @itemsep the time of one past the last sample.

@code(logical-stop@index(logical-stop)) @itemsep the time at which the sound logically ends, e.g. a 
sound may end at the beginning of a decay.  This value defaults 
to @code(signal-stop),
but may be set to any value.  
@end(itemize)
It may seem that there should be @code(logical-start) to indicate the
logical or perceptual beginning of a sound as well as a @code(logical-stop)
to indicate the logical ending of a sound.  In practice, only
@code(logical-stop) is needed; this attribute tells when the next sound
should begin to form a sequence of sounds.  In this respect, Nyquist sounds
are asymmetric: it is possible to compute sequences forward in time by
aligning the logical start of each sound with the @code(logical-stop) of the
previous one, but one cannot compute ``backwards'', aligning the logical end
of each sound with the logical start of its successor.  The root of this
asymmetry is the fact that when we invoke a behavior, we say when to start,
and the result of the behavior tells us its logical duration.  There is no
way to invoke a behavior with a direct specification of when to
stop@foot(Most behaviors will stop at time 1, warped according to @code(*warp*) to some real time, but this is by convention and is not a direct specification.).  

@p(Note:) there is no way to enforce the
intended ``perceptual'' interpretation of 
@code(logical-stop).  As far as Nyquist is concerned, these are just numbers to
guide the alignment of sounds within various control constructs.

@subsection(Multichannel Sounds)
@index(Multichannel Sounds)
Multichannel sounds are represented by Lisp arrays of sounds.  To create an
array of sounds the XLISP @code(vector) function is useful.  Most low-level 
Nyquist functions (the ones starting with @code(snd-)) do not operate on
multichannel sounds.  Most high-level functions do operate on multichannel
sounds.

@subsection(Accessing and Creating Sound)
@label(flatten-sec)
@label(snd-srate-sec)

Several functions display information concerning a sound and can be used to
query the components of a sound. There are functions that access samples in
a sound and functions that construct sounds from samples.

@begin(fndefs)
@codef[sref(@pragma(defn)@index(sref)@indexSecondary(primary="sound", 
secondary="accessing point")@i(sound), @i(time))] @c{[sal]}@*
@altdef{@code[(sref @i(sound) @i(time))] @c{[lisp]}}@\Accesses @i(sound) at 
the point @i(time), which is a local time. If @i(time) does not 
correspond to a sample time, then the nearest samples are linearly 
interpolated to form the result.  To access a particular sample, either 
convert the sound to an array (see @code(snd-samples) below), or use 
@code(snd-srate) and @code(snd-t0) (see below) to find the sample rate 
and starting time,  and compute a time (@i(t)) from the sample number (@i(n)):
@pragma(startscribe)
@begin(math)
t = (n / srate) + t0
@end(math)
@pragma(endscribe)
@html[<blockquote>t = (n / srate) + t0</blockquote>]
Thus, the lisp code to access the n@+(th) sample of a sound would look like:
@begin(code)
(sref sound (global-to-local (+ (/ n (snd-srate sound)) (snd-t0 sound))))
@end(code)
Here is why @code(sref) interprets its time argument as a local time:
@begin(example)
> (sref (ramp 1) 0.5) @i(; evaluate a ramp at time 0.5)
0.5
> (at 2.0 (sref (ramp 1) 0.5)) @i(; ramp is shifted to start at 2.0)
		@i(; the time, 0.5, is shifted to 2.5)
0.5
@end(example)
If you were to use @code(snd-sref), which treats time as global, instead of @code(sref), which treats time as local, then the first example above would return the same answer (0.5), but the second example would return 0.  Why? Because the @code[(ramp 1)] behavior would be shifted to start at time 2.0, but the resulting sound would be evaluated at global time 0.5.  By definition, sounds have a value of zero before their start time.

@codef[sref-inverse(@pragma(defn)@index(sref-inverse)@i(sound), @i(value))] @c{[sal]}@*
@altdef{@code[(sref-inverse @i(sound) @i(value))] @c{[lisp]}}@\Search @i(sound) for the first point at which it achieves @i(value) and return the corresponding (linearly interpolated) time.  If no inverse exists, an error is raised.  This function is used by Nyquist in the implementation of time warping.

@label(snd-from-array-sec)
@codef[snd-from-array(@IndexSecondary(primary="sound",
 secondary = "creating from array")@pragma(defn)@index(snd-from-array)@i(t0), @i(sr),
@i(array))] @c{[sal]}@*
@altdef{@code[(snd-from-array @i(t0) @i(sr) @i(array))] @c{[lisp]}}@\Converts a lisp array of @code(FLONUM)s into a sound with starting
time @i(t0) and sample rate @i(sr).  Safe for ordinary use.  Be aware that
arrays of floating-point samples use 14 bytes per sample, and an additional
4 bytes per sample are allocated by this function to create a sound type.

@codef[snd-fromarraystream(@pragma(defn)@index(snd-fromarraystream)@i(t0), @i(sr), @i(object))] @c{[sal]}@*
@altdef{@code[(snd-fromarraystream @i(t0) @i(sr) @i(object))] @c{[lisp]}}@\Creates a sound for which samples come from
@i(object). The starting time is @i(t0) (a @code(FLONUM)), and the sample rate is
@i(sr). The @i(object) is an XLISP object (see Section @ref(objects-sec) for
information on objects.) A sound is returned.  When the sound needs samples,
they are generated by sending the message @code(:next) to @i(object). If
@i(object) returns @code(NIL), the sound terminates. Otherwise, @i(object)
must return an array of @code(FLONUM)s.  The values in these arrays are
concatenated to form the samples of the resulting sound.
There is no provision for @i(object) to specify the
logical stop time of the sound, so the logical stop time is the termination
time. 

@codef[snd-fromobject(@pragma(defn)@index(snd-fromobject)@index(sound from Lisp data)@i(t0), @i(sr), @i(object))] @c{[sal]}@*
@altdef{@code[(snd-fromobject @i(t0) @i(sr) @i(object))] @c{[lisp]}}@\Creates a sound for which samples come from
@i(object). The starting time is @i(t0) (a @code(FLONUM)), and the sample rate is
@i(sr). The @i(object) is an XLISP object (see Section @ref(objects-sec) for
information on objects. A sound is returned.  When the sound needs samples,
they are generated by sending the message @code(:next) to @i(object). If
@i(object) returns @code(NIL), the sound terminates. Otherwise, @i(object)
must return a @code(FLONUM).  There is no provision for @i(object) to specify the
logical stop time of the sound, so the logical stop time is the termination
time.

@codef[snd-extent(@pragma(defn)@index(snd-extent)@i(sound), @i(maxsamples))] @c{[sal]}@*
@altdef{@code[(snd-extent @i(sound) @i(maxsamples))] @c{[lisp]}}@\Returns a list of two numbers: the starting time of @i(sound) and the terminate time of @i(sound).  Finding the terminate time requires that samples be computed.  Like most Nyquist functions, this is non-destructive, so memory will be allocated to preserve the sound samples.  If the sound is very long or infinite, this may exhaust all memory, so the @i(maxsamples) parameter specifies a limit on how many samples to compute.  If this limit is reached, the terminate time will be (incorrectly) based on the sound having @i(maxsamples) samples.  This function is safe for ordinary use.

@codef[snd-fetch(@index(access samples)@index(samples,
reading)@pragma(defn)@index(snd-fetch)@index(read samples)@i(sound))] @c{[sal]}@*
@altdef{@code[(snd-fetch @i(sound))] @c{[lisp]}}@\Reads samples
sequentially from @i(sound). This returns a @code(FLONUM) after each call, or
@code(NIL) when @i(sound) terminates. @p(Note:) @code(snd-fetch) modifies
@i(sound); it is strongly recommended to copy @i(sound) using
@code(snd-copy) and access only the copy with @code(snd-fetch).

@codef[snd-fetch-array(@pragma(defn)@index(snd-fetch-array)@i(sound), @i(len),
@i(step))] @c{[sal]}@*
@altdef{@code[(snd-fetch-array @i(sound) @i(len) @i(step))]
@c{[lisp]}}@\Reads 
sequential arrays of samples from @i(sound), returning either an array
of @code(FLONUM)s or @code(NIL) when the sound terminates. The @i(len)
parameter, a @code(FIXNUM), indicates how many samples should be
returned in the result array.  After the array is returned, @i(sound)
is modified by skipping over @i(step) (a @code(FIXNUM)) samples. If
@i(step) equals @i(len), then every sample is returned once.  If
@i(step) is less than @i(len), each returned array will overlap the
previous one, so some samples will be returned more than once. If
@i(step) is greater than @i(len), then some samples will be skipped
and not returned in any array. The @i(step) and @i(len) may change at
each call, but in the current implementation, an internal buffer is
allocated for @i(sound) on the first call, so subsequent calls may not
specify a greater @i(len) than the first. When an array is returned,
it will have @i(len) samples. If necessary, @code(snd-fetch-array)
will read zeros beyond the end of the sound to fill the array. When
this happens, @code(*rslt*) is set to a FIXNUM number of samples in
the array that were read from the sound before the physical stop time 
of the sound. If all samples in the array are ``valid'' samples from
the sound (coming from the sound before the sound
terminates), @code(*rslt*) is set to @code(NIL). The @code(*rslt*)
variable is global and used to return extra results from other
functions, so programs should not assume @code(*rslt*) is valid after
subsequent function calls. @p(Note:) @code(snd-fetch-array) modifies
@i(sound); it is strongly recommended to copy @i(sound) using
@code(snd-copy) and access only the copy with @code(snd-fetch-array).

@codef[snd-flatten(@pragma(defn)@index(snd-flatten)@i(sound), @i(maxlen))] @c{[sal]}@*
@altdef{@code[(snd-flatten @i(sound) @i(maxlen))] @c{[lisp]}}@\This function is identical 
to @code(snd-length). You would use this function to force samples to be computed in memory. Normally, this is not a good thing to do, but here is one appropriate use: In the case of sounds intended for wavetables, the unevaluated 
sound may be larger than the evaluated (and typically short) one. 
Calling @code(snd-flatten) will compute the samples and allow the unit generators to be freed in the next garbage collection. @p(Note:) If a sound is computed from many instances of table-lookup oscillators, calling @code(snd-flatten) will free the oscillators and their tables. Calling @code[(stats)] will print how many total bytes have been allocated to tables.

 @codef[snd-length(@pragma(defn)@index(snd-length)@i(sound), @i(maxlen))] @c{[sal]}@*
@altdef{@code[(snd-length @i(sound) @i(maxlen))] @c{[lisp]}}@\Counts the
number of samples in @i(sound) up to the physical stop time.  If the sound
has more than @i(maxlen) samples, @i(maxlen) is returned.  Calling this
function will cause all samples of the sound to be computed and saved in
memory (about 4 bytes per sample).  Otherwise, this function is safe for ordinary use.

 @codef[snd-maxsamp(@pragma(defn)@index(snd-maxsamp)@i(sound))] @c{[sal]}@*
@altdef{@code[(snd-maxsamp @i(sound))] @c{[lisp]}}@\Computes the maximum of
the absolute value of the samples in @i(sound).  Calling this function will
cause samples to be computed and saved in memory.  (This function should
have a @i(maxlen) parameter to allow self-defense against sounds that would
exhaust available memory.)  Otherwise, this function is safe for ordinary use.
This function will probably be removed in a future version.  See @code(peak), a replacement (@pragma(startref) page @pageref(peak-sec)).


 @codef[snd-play(@pragma(defn)@index(snd-play)@i(expression))] @c{[sal]}@*
@altdef{@code[(snd-play @i(expression))] @c{[lisp]}}@\Evaluates @i(expression)
to obtain a sound or array of sounds, computes all of the samples (without
retaining them in memory), and returns. Originally, this was a placeholder
for a facility to play samples directly to an audio output device, but
playback is now accomplished by @code(s-save).
Meanwhile, since this
function does not save samples in memory or write them to a disk, it is
useful in determining how much time is spent calculating samples.  See
@code(s-save) (Section @ref(s-save-sec)) for saving samples to a file, and
 @code(play) (Section @ref(play-sec)) to play a sound.  This function is
safe for ordinary use.

@codef[snd-print-tree(@pragma(defn)@index(snd-print-tree)@i(sound))] @c{[sal]}@*
@altdef{@code[(snd-print-tree @i(sound))] @c{[lisp]}}@\Prints an ascii
representation of the internal data structures representing a sound.  This
is useful for debugging Nyquist.  This function is
safe for ordinary use.

 @codef[snd-samples(@index(samples)@pragma(defn)@index(snd-samples)@index(array from sound)@index(convert sound to array)@i(sound), @i(limit))] @c{[sal]}@*
@altdef{@code[(snd-samples @i(sound) @i(limit))] @c{[lisp]}}@\Converts the
samples into a lisp array.  The data is taken directly from the samples,
ignoring shifts.  For example, if the sound starts at 3.0 seconds, the first
sample will refer to time 3.0, not time 0.0.  A maximum of @i(limit) samples
is returned.  This function is safe for ordinary use, but like
@code(snd-from-array), it requires a total of slightly over 18 bytes per
sample.

 @codef[snd-srate(@pragma(defn)@index(snd-srate)@i(sound))] @c{[sal]}@*
@altdef{@code[(snd-srate @i(sound))] @c{[lisp]}}@\Returns the sample rate of
the sound. Safe for ordinary use.

@begin(comment)
 @codef[snd-show(@pragma(defn)@index(snd-show)@i(sound))] @c{[sal]}@*
@altdef{@code[(snd-show @i(sound))] @c{[lisp]}}@\Print the entire (internal)
structure of the sound for debugging.  Safe for ordinary use.
@end(comment)

@codef[snd-time(@pragma(defn)@index(snd-time)@i(sound))] @c{[sal]}@*
@altdef{@code[(snd-time @i(sound))] @c{[lisp]}}@\Returns the start time of the
sound.  This will probably go away in a future version, so use @code(snd-t0)
instead.

@codef[snd-t0(@pragma(defn)@index(snd-t0)@i(sound))] @c{[sal]}@*
@altdef{@code[(snd-t0 @i(sound))] @c{[lisp]}}@\Returns the time of the
first sample of the sound.  Note that Nyquist operators such as add always
copy the sound and are allowed to shift the copy up to one half sample
period in either direction to align the samples of two operands.  Safe for
ordinary use.

@codef[snd-print(@pragma(defn)@index(snd-print)@i(expression), @i(maxlen))] @c{[sal]}@*
@altdef{@code[(snd-print @i(expression) @i(maxlen))] @c{[lisp]}}@\Evaluates
@i(expression) to yield a sound or an array of sounds, then prints up to
@i(maxlen) samples to the screen (stdout).  This is similar to
@code(snd-save), but samples appear in text on the screen instead of in
binary in a file.  This function is intended for debugging@index(debugging).
Safe for ordinary use.

 @codef[snd-set-logical-stop(@pragma(defn)@index(snd-set-logical-stop)@i(sound),
@i(time))] @c{[sal]}@*
@altdef{@code[(snd-set-logical-stop @i(sound) @i(time))] @c{[lisp]}}@\Returns a sound which is 
@i(sound), except that the logical stop of the sound occurs at @i(time).
 @p(Note:) do not call this function.  When defining a behavior, use
@code(set-logical-stop) or @code(set-logical-stop-abs) instead.

@codef[snd-sref(@pragma(defn)@index(snd-sref)@i(sound), @i(time))] @c{[sal]}@*
@altdef{@code[(snd-sref @i(sound) @i(time))] @c{[lisp]}}@\Evaluates @i(sound) 
at the global time given by @i(time).  Safe for ordinary use, but normally, you should
call @code(sref) instead.

 @codef[snd-stop-time(@pragma(defn)@index(snd-stop-time)@i(sound))] @c{[sal]}@*
@altdef{@code[(snd-stop-time @i(sound))] @c{[lisp]}}@\Returns the stop time of @i(sound).
Sounds can be ``clipped'' or truncated at a particular time.  This function
returns that time or MAX-STOP-TIME if he programmer has not specified a stop
time for the sound.  Safe for ordinary use.

@codef[soundp(@pragma(defn)@index(soundp)@i(sound))] @c{[sal]}@*
@altdef{@code[(soundp @i(sound))] @c{[lisp]}}@\Returns true iff @i(sound) is a
SOUND.  Safe for ordinary use.

@codef[stats(@pragma(defn)@index(stats)@index(memory 
usage)@index(table memory))] @c{[sal]}@*
@altdef{@code[(stats)] @c{[lisp]}}@\Prints the memory usage status.  
See also the 
XLISP @code(mem) function.  Safe for ordinary use. This is the only way to find out how much memory is being used by table-lookup oscillator instances.

@end(fndefs)

@subsection(Miscellaneous Functions)
These are all safe and recommended for ordinary use.

@begin(fndefs)
@codef[db-to-linear(@pragma(defn)@index(db-to-linear)@i(x))] @c{[sal]}@*
@altdef{@code[(db-to-linear @i(x))] @c{[lisp]}}@\Returns the
conversion of @i(x) from decibels to linear.  0dB is converted to 1.
20dB represents a linear factor of 10. If @i(x) is a sound, each
sample is converted and a sound is returned.  If @i(x) is a
multichannel sound, each channel is converted and a multichannel sound
(array) is returned.  @p(Note:) With sounds, conversion is only
performed on actual samples, not on the implicit zeros before the
beginning and after the termination of the sound.  Sample rates, start
times, etc. are taken from @i(x). 

@codef{db-to-vel(@pragma(defn)@index(db-to-vel)@i(x) [, @i(float)])} @c{[sal]}@*
@altdef{@code{(db-to-vel @i(x) [@i(float)])} @c{[lisp]}}@\Returns the 
conversion of @i(x) from decibels to MIDI velocity using a rule that
maps -60 dB to 1 and 0 dB to 127. The MIDI velocity varies linearly 
with the square root of amplitude. The default value of @i(float) is
@code(nil) and the result is a @code(FIXNUM) clipped to fall in the
legal range of 1-127, but if a non-@code(nil) value
is provided, the result is a @code(FLONUM) that is not 
rounded or clipped. The input parameter must be a @code(FIXNUM) or
@code(FLONUM). Sounds are not allowed.

@codef[follow(@pragma(defn)@index(follow)@index(envelope follower)@index(compressor)@index(limiter)@i(sound), @i(floor), @i(risetime), @i(falltime), @i(lookahead))] @c{[sal]}@*
@altdef{@code[(follow @i(sound) @i(floor) @i(risetime) @i(falltime) @i(lookahead))] @c{[lisp]}}@\An envelope follower intended as a commponent for compressor and limiter functions. The basic goal of this function is to generate a smooth signal 
that rides on the peaks of the input signal. The usual objective is to 
produce an amplitude envelope given a low-sample rate (control rate) 
signal representing local RMS measurements. The first argument is the 
input signal. The @i(floor) is the minimum output value. The @i(risetime) 
is the time (in seconds) it takes for the output to rise (exponentially) 
from @i(floor) to unity (1.0) and the @i(falltime) is the time it takes 
for the output to fall (exponentially) from unity to @i(floor). The 
algorithm looks ahead for peaks and will begin to increase the output 
signal according to @i(risetime) in anticipation of a peak. The amount 
of anticipation (in seconds) is given by @i(lookahead).  The algorithm 
is as follows: the output value is allowed to increase according to 
@i(risetime) or decrease according to @i(falltime). If the next input 
sample is in this range, that sample is simply output as the next output 
sample.  If the next input sample is too large, the algorithm goes back in 
time as far as necessary to compute an envelope that rises according to 
@i(risetime) to meet the new value. The algorithm will only work backward 
as far as @i(lookahead).  If that is not far enough, then there is a final 
forward pass computing a rising signal from the earliest output sample. In 
this case, the output signal will be at least momentarily less than the 
input signal and will continue to rise exponentially until it intersects 
the input signal. If the input signal falls faster than indicated by 
@i(falltime), the output fall rate will be limited by @i(falltime), 
and the fall in output will stop when the output reaches @i(floor). 
This algorithm can make two passes througth the buffer on sharply rising 
inputs, so it is not particularly fast. With short buffers and low sample 
rates this should not matter. See @code(snd-avg) for a function that 
can help to generate a low-sample-rate input for @code(follow). 
See @code(snd-chase) in Section @ref(snd-chase-sec) for a related filter.

@label(gate-sec)
@codef[gate(@pragma(defn)@index(gate)@index(noise-gate)@i(sound), 
@i(lookahead), @i(risetime), @i(falltime), @i(floor), 
@i(threshold))] @c{[sal]}@*
@altdef{@code[(gate @i(sound) @i(lookahead) @i(risetime) @i(falltime) @i(floor) @i(threshold))] @c{[lisp]}}@\Generate an exponential rise and decay intended 
for noise gate implementation. The decay starts when the signal drops 
below @i(threshold) and stays there for longer than @i(lookahead) (a 
@code(FLONUM) in seconds). (The signal begins to drop when the signal 
crosses @i(threshold), not after @i(lookahead).) Decay continues until 
the value reaches @i(floor) (a @code(FLONUM)), at which point the decay 
stops and the output value is held constant. Either during the decay or 
after the floor is reached, if the signal goes above @i(threshold), then 
the ouptut value will rise to unity (1.0) at the point the signal crosses 
the threshold. Because of internal lookahead, the signal actually begins 
to rise before the signal crosses @i(threshold). The rise is a 
constant-rate exponential and set so that a rise from @i(floor) to unity 
occurs in @i(risetime). Similary, the fall is a constant-rate exponential 
such that a fall from unity to @i(floor) takes @i(falltime).

@codef[hz-to-step(@pragma(defn)@index(hz-to-step)@i(freq))] @c{[sal]}@*
@altdef{@code[(hz-to-step @i(freq))] @c{[lisp]}}@\Returns a step number for @i(freq) (in hz), which can be either a number of a @code(SOUND). The result has the same type as the argument. See also @code(step-to-hz) (below).

@codef[linear-to-db(@pragma(defn)@index(linear-to-db)@i(x))] @c{[sal]}@*
@altdef{@code[(linear-to-db @i(x))] @c{[lisp]}}@\Returns the conversion of @i(x) from linear to decibels.  1 is converted to 0.  0 is converted to -INF (a special IEEE floating point value.)  A factor of 10 represents a 20dB change.  If @i(x) is a sound,  each sample is converted and a sound is returned.  If @i(x) is a multichannel sound, each channel is converted and a multichannel sound (array) is returned.  @p(Note:) With sounds, conversion is only performed on actual samples, not on the implicit zeros before the beginning and after the termination of the sound.  Start times, sample rates, etc. are taken from @i(x).

@codef{linear-to-vel(@pragma(defn)@index(linear-to-vel)@i(x) [, @i(float)])} @c{[sal]}@*
@altdef{@code{(linear-to-vel @i(x) [@i(float)])} @c{[lisp]}}@\Returns the 
conversion of @i(x) from linear amplitude to MIDI velocity using a rule that
maps -60 dB to 1 and 0 dB to 127. The MIDI velocity varies linearly 
with the square root of amplitude. The default value of @i(float) is
@code(nil) and the result is a @code(FIXNUM) clipped to fall in the
legal range of 1-127, but if a non-@code(nil) value
is provided, the result is a @code(FLONUM) that is not 
rounded or clipped. The input parameter must be a @code(FIXNUM) or
@code(FLONUM). Sounds are not allowed.

@codef[log(@index(log function)@pragma(defn)@index(log)@i(x))] @c{[sal]}@*
@altdef{@code[(log @i(x))] @c{[lisp]}}@\Calculates the natural log of @i(x) (a @code(FLONUM)). (See @code(s-log) for a version that operates on signals.)

@codef[set-control-srate(@pragma(defn)@index(set-control-srate)@index(sampling rate)@i(rate))] @c{[sal]}@*
@altdef{@code[(set-control-srate @i(rate))] @c{[lisp]}}@\Sets the default sampling rate for control signals to @i(rate) by setting @code(*default-control-srate*) and reinitializing the environment.  Do not call this within any synthesis function (see the @code(control-srate-abs) transformation, Section @ref(control-srate-abs-sec)).

@codef[set-sound-srate(@pragma(defn)@index(set-sound-srate)@index(sampling rate)@i(rate))] @c{[sal]}@*
@altdef{@code[(set-sound-srate @i(rate))] @c{[lisp]}}@\Sets the default sampling rate for audio signals to @i(rate) by setting @code(*default-sound-srate*) and reinitializing the environment.  Do not call this within any synthesis function (see the @code(sound-srate-abs) transformation, Section @ref(sound-srate-abs-sec)).

@codef[set-pitch-names(@pragma(defn)@index(set-pitch-names))] @c{[sal]}@*
@altdef{@code[(set-pitch-names)] @c{[lis]}}@\Initializes pitch 
variables (@code(c0), @code(cs0), @code(df0), @code(d0), ... @code(b0), 
 @code(c1), ... @code(b7)).  A440 (the default tuning) is represented by
 the step 69.0, so the variable @code(a4) (fourth octave A) is set to 69.0.  
You can change the tuning by 
setting @code(*A4-Hertz*)@index(tuning)@index(A440)@index(*A4-Hertz*) to a 
value (in Hertz) and calling @code(set-pitch-names) to reinitialize the pitch 
variables.  Note that this will result in non-integer step values.  It does not
alter the mapping from step values to frequency.  There is no built-in 
provision for stretched scales or non-equal temperament, although users 
can write or compute any desired fractional step values.

 @codef[step-to-hz(@pragma(defn)@index(step-to-hz)@i(pitch))] @c{[sal]}@*
@altdef{@code[(step-to-hz @i(pitch))] @c{[lisp]}}@\Returns a frequency in hz
for @i(pitch), a step number or a @code(SOUND) type representing a time-varying step number. The result is a @code(FLONUM) if @i(pitch) is a number, and a @code(SOUND) if @i(pitch) is a @code(SOUND). See also @code(hz-to-step) (above).


@codef{get-duration(@pragma(defn)@index(get-duration)@i(dur))} @c{[sal]}@*
@altdef{@code[(get-duration @i(dur))] @c{[lisp]}}@\Gets the actual duration of of something starting at a local time of 0 and ending at a local time of @i(dur) times the current sustain. For convenience,  @code(*rslt*) is set to the global time corresponding to local time zero.

@codef{get-loud(@pragma(defn)@index(get-loud))} @c{[sal]}@*
@altdef{@code[(get-loud)] @c{[lisp]}}@\Gets the current value of the @code(*loud*) environment variable.  If @code(*loud*) is a signal, it is evaluated at local time 0 and a number (@code(FLONUM)) is returned.

@codef{get-sustain(@pragma(defn)@index(get-sustain))} @c{[sal]}@*
@altdef{@code[(get-sustain)] @c{[lisp]}}@\Gets the current value of the @code(*sustain*) environment variable.  If @code(*sustain*) is a signal, it is evaluated at local time 0 and a number (@code(FLONUM)) is returned.

@codef{get-transpose(@pragma(defn)@index(get-transpose))} @c{[sal]}@*
@altdef{@code[(get-transpose)] @c{[lisp]}}@\Gets the current value of the @code(*transpose*) environment variable.  If @code(*transpose*) is a signal, it is evaluated at local time 0 and a number (@code(FLONUM)) is returned.

@codef{get-warp(@pragma(defn)@index(get-warp))} @c{[sal]}@*
@altdef{@code[(get-warp)] @c{[lisp]}}@\Gets a function corresponding to 
the current value of the @code(*warp*) environment variable.  For 
efficiency, @code(*warp*) is stored in three parts representing a shift,
 a scale factor, and a continuous warp function.  @code(Get-warp) is used
 to retrieve a signal that maps logical time to real time.  This signal 
combines the information of all three components of @code(*warp*) into 
a single signal.  If the continuous warp function component is not present
 (indicating that the time warp is a simple combination of @code(at)
 and @code(stretch) transformations), an error is raised.  This 
function is mainly for internal system use.  In the future,
 @code(get-warp) will probably be reimplemented to always return
 a signal and never raise an error.

@CODEF{LOCAL-to-global(@pragma(defn)@index(local-to-global)@i(local-time))} @c{[sal]}@*
@altdef{@code[(local-to-global @i(local-time))] @c{[lisp]}}@\Converts a score (local) time to a real (global) time according to the current environment.

@codef{osc-enable(@pragma(defn)@index(osc-enable)@index(osc)@index(open sound control)@i(flag))} @c{[sal]}@*
@altdef{@code[(osc-enable @i(flag))] @c{[lisp]}}@\Enable or disable Open Sound Control. 
(See Appendix @ref(osc-app).)
Enabling creates a socket and a service that listens for UDP 
packets on port 7770. Currently, only two messages are accepted 
by Nyquist. The first is of the form @code(/slider)
with an integer index and a floating point value. These set internal 
slider values accessed by the @code(snd-slider) 
function. The second is of the form @code(/wii/orientation) with
two floating point values. This message is a special case to 
support the DarwiinRemoteOsc@index(DarwiinRemoteOsc) program
 which can relay data from
a Nintendo@index(Nintendo WiiMote) WiiMote@index(Wii Controller)
 device to Nyquist via OSC. The two orientation
values control sliders 0 and 1.
Disabling terminates the service (polling for messages) 
and closes the socket. The @i(previous) state of enablement
is returned, e.g. if OSC is enabled and @i(flag) is @i(nil), 
OSC is disabled and @code(T) (true) is returned because OSC 
was enabled at the time of the call. This function only exists 
if Nyquist is compiled with the compiler flag @code(OSC). 
Otherwise, the function 
exists but always returns the symbol @code(DISABLED). Consider 
lowering the audio latency using @code(snd-set-latency).
@i(Warning:) there is the potential for 
network-based attacks using OSC. It is tempting to add the 
ability to evaluate XLISP expressions sent via OSC, but 
this would create
unlimited and unprotected access to OSC clients. For now, 
it is unlikely that an attacker could do more than 
manipulate slider values.

@codef{snd-set-latency(@index(latency)@pragma(defn)@index(snd-set-latency)@i(latency))} @c{[sal]}@*
@altdef{@code[(snd-set-latency @i(latency))] @c{[lisp]}}@\Set the latency requested when Nyquist plays sound to
 @i(latency), a @code(FLONUM). The previous value is returned. The default is 0.3 seconds. To avoid glitches, the latency should be 
greater than the time required for garbage collection and message printing and any other system activity external to Nyquist.

@codef[vel-to-db(@pragma(defn)@index(vel-to-db)@i(x))] @c{[sal]}@*
@altdef{@code[(vel-to-db @i(x))] @c{[lisp]}}@\Returns the conversion
of @i(x) from MIDI velocity to decibels using a rule that maps MIDI
velocity 1 to -60 dB and 127 to 0 dB. The amplitude is proportional to
the square of MIDI velocity. The input @i(x) can be a @code(FIXNUM) or
@code(FLONUM) but not a sound. The result is a @code[FLONUM].

@code[vel-to-linear(@pragma(defn)@index(vel-to-linear)@i(x))]
@c{[sal]}@* @altdef{@code[(vel-to-linear @i(x))] @c{[lisp]}}@\Returns
the conversion of @i(x) from MIDI velocity to decibels using a rule
that maps MIDI 
velocity 1 to -60 dB and 127 to 0 dB. The amplitude is proportional to
the square of MIDI velocity. The input @i(x) can be a @code(FIXNUM) or
@code(FLONUM) but not a sound. The result is a @code[FLONUM].
@end(fndefs)

@section(Behaviors)@index(Behaviors)
@label(behavior-sec)

@subsection(Using Previously Created Sounds)
@label(use-sounds-sec)
These behaviors take a sound and transform that sound according to the
environment.  These are useful when writing code to make
a high-level function from a low-level function, or when cuing sounds
which were previously created:
@begin(fndefs)
@codef[cue(@pragma(defn)@index(cue)@i(sound))] @c{[sal]}@*
@altdef{@code[(cue @i(sound))] @c{[lisp]}}@\Applies @code(*loud*), the starting time from @code(*warp*), @code(*start*),
 and @code(*stop*) to @i(sound).

@codef[cue-file(@pragma(defn)@index(cue-file)@i(filename))] @c{[sal]}@*
@altdef{@code[(cue-file @i(filename))] @c{[lisp]}}@\Same as @code(cue), except
the sound comes from the named file, samples from which are coerced to the current default @code(*sound-srate*) sample rate.

@codef[sound(@pragma(defn)@index(sound)@i(sound))] @c{[sal]}@*
@altdef{@code[(sound @i(sound))] @c{[lisp]}}@\Applies @code(*loud*), @code(*warp*), 
@code(*start*), and @code(*stop*) to @i(sound).

@codef[control(@pragma(defn)@index(control)@i(sound))] @c{[sal]}@*
@altdef{@code[(control @i(sound))] @c{[lisp]}}@\This function is identical to
@code(sound), but by convention is used when @i(sound) is a control signal
rather than an audio signal.
@end(fndefs)

@subsection(Sound Synthesis)

These functions provide musically interesting creation behaviors that
react to their environment; these are the ``unit generators'' of Nyquist:

@begin(fndefs)
@codef{const(@pragma(defn)@index(const)@index(constant function)@i(value) [, @i(duration)])} @c{[sal]}@*
@altdef{@code{(const @i(value) [@i(duration)])} @c{[lisp]}}@\Creates a constant function at the @code(*control-srate*).  Every sample has the given @i(value), and the default @i(duration) is 1.0.  See also @code(s-rest), which is equivalent to calling @code(const) with zero, and note that you can pass scalar constants (numbers) to @code(sim), @code(sum), and @code(mult) where they are handled more efficiently than constant functions.

@codef{env(@pragma(defn)@index(env)@i(t@-[1]), @i(t@-[2]), @i(t@-[4]), @i(l@-[1]), @i(l@-[2]), @i(l@-[3]), 
[@i(dur)])} @c{[sal]}@*
@altdef{@code[(env @i(t@-[1]) @i(t@-[2]) @i(t@-[4]) @i(l@-[1]) @i(l@-[2]) @i(l@-[3]) @i(dur))] @c{[lisp]}}@\Creates a 4-phase envelope.
@i(t@-[@i(i)]) is the duration of phase @i(i), and @i(l@-[@i(i)]) 
is the final level of phase @i(i).  @i(t@-[3]) is implied by the duration
@i(dur), and @i(l@-[4]) is @code(0.0).  If @i(dur) is not supplied, then
@code(1.0) is assumed.  The envelope duration is the product of @i(dur),
@code(*stretch*), and @code(*sustain*).  If 
@i(t@-[1]) + @i(t@-[2]) + 2ms + @i(t@-[4]) is greater than the envelope 
duration, then a two-phase envelope is
substituted that has an attack/release time ratio of @i(t@-[1])/@i(t@-[4]).
The sample rate of the returned sound is @code(*control-srate*).  (See
@code(pwl) for a more general piece-wise linear function generator.)
The effect of time warping is to warp the starting time and ending time.
The intermediate breakpoints are then computed as described above.


@codef{exp-dec(@pragma(defn)@index(exp-dec)@index(exponential envelope)@i(hold), @i(halfdec), @i(length))} @c{[sal]}@*
@altdef{@code[(exp-dec @i(hold) @i(halfdec) @i(length))] @c{[lisp]}}@\This convenient envelope shape is a special case of @code(pwev) (see Section @ref(pwev-sec)). The envelope starts at 1 and is constant for @i(hold) seconds. It then decays with a half life of @i(halfdec) seconds until @i(length). (The total duration is @i(length).) In other words, the amplitude falls by half each @i(halfdec) seconds. When stretched, this envelope scales linearly, which means the hold time increases and the half decay time increases.


@label(force-srate-sec)
@codef{force-srate(@pragma(defn)@index(force-srate)@index(sample rate, forcing)@index(resampling)@i(srate), @i(sound))} @c{[sal]}@*
@altdef{@code[(force-srate @i(srate) @i(sound))] @c{[lisp]}}@\Returns a sound which is up- or
down-sampled to @i(srate).  Interpolation is linear, and no prefiltering is
applied in the down-sample case, so aliasing may occur. See also
@code(resample).


@codef{lfo(@pragma(defn)@index(lfo)@index(low-frequency oscillator)@i(freq) [, @i(duration), @i(table), @i(phase)])} @c{[sal]}@*
@altdef{@code[(lfo @i(freq) @i(duration) @i(table) @i(phase))] @c{[lisp]}}@\Just
like @code(osc) (below)
except this computes at the @code(*control-srate*) and frequency
is specified in Hz.  Phase is specified in degrees.
 The @code(*transpose*) and @code(*sustain*) is not
applied.  The effect of time warping is to warp the starting and ending
times.  The signal itself will have a constant unwarped frequency.

@codef{fmlfo(@pragma(defn)@index(fmlfo)@i(freq) [, @i(table), @i(phase)])} @c{[sal]}@*
@altdef{@code{(fmlfo @i(freq) [@i(table) @i(phase)])} @c{[lisp]}}@\A low-frequency oscillator
that computes at the @code(*control-srate*) using a sound to specify a time-varying 
frequency in Hz. Phase is a @code(FLONUM) in degrees. The duration of the result is determined by @i(freq).

@codef{maketable(@pragma(defn)@index(maketable)@label(maketable)@i(sound))} @c{[sal]}@*
@altdef{@code[(maketable @i(sound))] @c{[lisp]}}@\Assumes that
the samples in @i(sound) constitute one period of a wavetable, and returns a wavetable
suitable for use as the @i(table) argument to the @code(osc) function (see
below).  Currently, tables are limited to 1,000,000 samples.  This limit is the compile-time constant @code(max_table_len) set in @code(sound.h).

@codef{build-harmonic(@pragma(defn)@index(build-harmonic)@index(harmonic)@i(n), @i(table-size))} @c{[sal]}@*
@altdef{@code[(build-harmonic @i(n) @i(table-size))] @c{[lisp]}}@\Intended for
constructing wavetables@index(wavetables)@index(waveforms), this function returns a sound of length @i(table-size)
samples containing @i(n) periods of a sinusoid.  These can be scaled and
summed to form a waveform with the desired harmonic content.  See @pragma(startref) page @pageref(build-harmonic-example) for an example.

@codef{control-warp(@pragma(defn)@index(control-warp)@i(warp-fn), @i(signal),   [@i(wrate)])} @c{[sal]}@*
@altdef{@code[(control-warp @i(warp-fn) @i(signal) @i(wrate))] @c{[lisp]}}@\Applies a
warp function @i(warp-fn) to @i(signal) using function composition.  If @i(wrate) is omitted, linear
interpolation is used.  @i(warp-fn) is a mapping from score (logical) time
to real time, and @i(signal) is a function from score time to real values.
The result is a function from real time to real values at a sample rate of
@code(*control-srate*). See @code(sound-warp) for an explanation of
@i(wrate) and high-quality warping.

@label(mult-sec)
@codef{mult(@pragma(defn)@index(mult)@i(beh@-[1]), @i(beh@-[2]), @r(...))} @c{[sal]}@*
@altdef{@code[(mult @i(beh@-[1]) @i(beh@-[2] @r(...)))] @c{[lisp]}}@\Returns the product of
behaviors.  The arguments may also be numbers, in which case simple multiplication is performed.  If a number and sound are mixed, the @code(scale) function is used to scale the sound by the number. When sounds are multiplied, the resulting sample rate is the maximum sample rate of the factors.

@codef{prod(@pragma(defn)@index(prod)@i(beh@-[1]), @i(beh@-[2]), @r(...))} @c{[sal]}@*
@altdef{@code[(prod @i(beh@-[1]) @i(beh@-[2]) @r(...))] @c{[lisp]}}@\Same as @code(mult).

@label(pan-sec)
@codef{pan(@pragma(defn)@index(pan)@index(stereo panning)@i(sound), @i(where))} @c{[sal]}@*
@altdef{@code[(pan @i(sound) @i(where))] @c{[lisp]}}@\Pans @i(sound) (a behavior) according  to @i(where) (another behavior or a number). @i(Sound) must be monophonic. @i(Where) may be a monophonic sound (e.g. @code[(ramp)] or simply a number (e.g. @code(0.5)). In either case, @i(where) should range from 0 to 1, where 0 means pan completely left, and 1 means pan completely right. For intermediate values, the sound to each channel is scaled linearly. Presently, @code(pan) does not check its arguments carefully.

@codef{prod(@pragma(defn)@index(prod)@i(beh@-[1]), @i(beh@-[2]), @r(...))} @c{[sal]}@*
@altdef{@code[(prod @i(beh@-[1]) @i(beh@-[2]) @r(...))] @c{[lisp]}}@\Same as @code(mult).

@label(resample-sec)
@codef{resample(@pragma(defn)@index(resample)@i(sound), @i(srate))} @c{[sal]}@*
@altdef{@code[(resample @i(sound) @i(srate))] @c{[lisp]}}@\Similar to @code(force-srate), except
high-quality interpolation is used to prefilter and reconstruct the signal
at the new sample rate. Also, the result is scaled by 0.95 to reduce problems with
clipping. (See also @code(sound-warp).)

@label(scale-sec)
@codef{scale(@pragma(defn)@index(scale)@i(scale), @i(sound))} @c{[sal]}@*
@altdef{@code[(scale @i(scale) @i(sound))] @c{[lisp]}}@\Scales the amplitude of @i(sound) by the factor @i(scale).  Identical function to @code(snd-scale), except that it handles multichannel sounds.  Sample rates, start times, etc. are taken from @i(sound).

@codef{scale-db(@pragma(defn)@index(scale-db)@i(db), @i(sound))} @c{[sal]}@*
@altdef{@code[(scale-db @i(db) @i(sound))] @c{[lisp]}}@\Scales the amplitude of @i(sound) by the factor @i(db), expressed in decibels.  Sample rates, start times, etc. are taken from @i(sound).

@codef[scale-srate(@pragma(defn)@index(scale-srate)@i(sound), @i(scale))] @c{[sal]}@*
@altdef{@code[(scale-srate @i(sound) @i(scale))] @c{[lisp]}}@\Scales the sample rate of @i(sound) by @i(scale) factor.  This has the effect of linearly shrinking or stretching time (the sound is not upsampled or downsampled).  This is a special case of @code(snd-xform) (see Section @ref(snd-xform-sec)).

@codef[shift-time(@pragma(defn)@index(shift-time)@i(sound), @i(shift))] @c{[sal]}@*
@altdef{@code[(shift-time @i(sound) @i(shift))] @c{[lisp]}}@\Shift @i(sound)
by @i(shift) seconds.  If the sound is 
@pragma(startscribe)
@math[f(t)], then the result is
@pragma(endscribe)
@html[f(t), then the result is]
@pragma(startscribe)
@math[f(t - shift)].  
@pragma(endscribe)
@html[f(t - shift).]
See Figure @ref(shift-time-fig).  This is a special
case of @code(snd-xform) (see Section @ref(snd-xform-sec)).
@end(fndefs)

@begin(figure)
@center(@graphic((height = 3 in, width = 4.5 in, magnify = 0.75,
		postscript = "shifttimefig.ps"))
@html(<img src="fig5.gif"><br><br>)
@fillcaption(The @code(shift-time) function shifts a sound in time
according to its @i(shift) argument.)
@tag(shift-time-fig)
@end(figure)

@begin(fndefs)
@codef{sound-warp(@pragma(defn)@index(sound-warp)@i(warp-fn), @i(signal) [, @i(wrate)])} @c{[sal]}@*
@altdef{@code{(sound-warp @i(warp-fn) @i(signal) [@i(wrate)])} @c{[lisp]}}@\Applies a
warp function @i(warp-fn) to @i(signal) using function composition.  If the optional parameter @i(wrate) is omitted or NIL, linear
interpolation is used.  Otherwise, high-quality sample interpolation is used, and the
result is scaled by 0.95 to reduce problems with clipping (interpolated samples can
exceed the peak values of the input samples.) 
@i(warp-fn) is a mapping from score (logical) time
to real time, and @i(signal) is a function from score time to real values.
The result is a function from real time to real values at a sample rate of @code(*sound-srate*).
See also @code(control-warp).
@blankspace(1)
If @i(wrate) is not NIL, it must be a number. The parameter indicates that
high-quality resampling should be used and specifies the sample rate for the
inverse of @i(warp-fn). Use the lowest number you can.
 (See below for details.) Note that high-quality resampling is
much slower than linear interpolation. 
@blankspace(1)
To perform high-quality resampling by a fixed ratio, as opposed to a
variable ratio allowed in @code(sound-warp), use @code(scale-srate) to
stretch or shrink the sound, and then @code(resample) to restore the
original sample rate.
@blankspace(1)
@code(Sound-warp) and @code(control-warp) both take the inverse of
@i(warp-fn) to get a function from real time to score time. Each sample
of this inverse is thus a score time; @i(signal) is evaluated at each of
these score times to yield a value, which is the desired result. The 
sample rate of the inverse warp function is somewhat arbitrary. With linear
interpolation, the inverse warp function sample rate is taken to be the
output sample rate. Note, however, that the samples of the inverse warp
function are stored as 32-bit floats, so they have limited precision. Since
these floats represent sample times, rounding can be a problem. Rounding
in this case is equivalent to adding jitter to the sample times. Nyquist
ignores this problem for ordinary warping, but for high-quality warping, the
jitter cannot be ignored. 
@blankspace(1)
The solution is to use a rather low sample rate
for the inverse warp function. @code(Sound-warp) can then linearly
interpolate this signal using double-precision floats to minimize jitter
between samples. The sample rate is a compromise: a low sample rate
minimizes jitter, while a high sample rate does a better job of capturing
detail (e.g. rapid fluctuations) in the warp function. A good rule of thumb
is to use at most 1,000 to 10,000 samples for the inverse warp function. For
example, if the result will be 1 minute of sound, use a sample rate of
3000 samples / 60 seconds = 50 samples/second. Because Nyquist has no
advance information about the warp function, the inverse warp function
sample rate must be provided as a parameter.  When in doubt, just try
something and let your ears be the judge.

@codef[integrate(@pragma(defn)@index(integrate)@index(smooth)@i(signal))] @c{[sal]}@*
@altdef{@code[(integrate @i(signal))] @c{[lisp]}}@\Computes the integral of @i(signal). The start time, sample rate, etc. are taken from @i(signal).

@codef[slope(@pragma(defn)@index(slope)@index(derivative)@index(first derivative)@i(signal))] @c{[sal]}@*
@altdef{@code[(slope @i(signal))] @c{[lisp]}}@\Computes the first derivative (slope) of @i(signal).  The start time, sample rate, etc. are taken from @i(signal).
@end(fndefs)

@paragraph(Oscillators)
@label(osc-sec)
@begin(fndefs)
@codef{osc(@pragma(defn)@index(osc)@i(pitch) [, @i(duration), @i(table), @i(phase)])} @c{[sal]}@*
@altdef{@code{(osc @i(pitch) [@i(duration) @i(table) @i(phase)])} @c{[lisp]}}@\Returns 
a sound which
is the @i(table) oscillated at @i(pitch) for the given @i(duration),
starting with the @i(phase) (in degrees).  
Defaults are:  @i(duration) @code(1.0) 
(second), @i(table) @code(*table*),
@i(phase) @code(0.0).  The default value of @code(*table*) is a sinusoid. Duration is stretched by @code(*warp*) and 
@code(*sustain*), amplitude is nominally 1, but scaled by @code(*loudness*), the start time is logical time 0, transformed by @code(*warp*), and the sample rate is @code(*sound-srate*).
The effect of time-warping is to warp the starting and ending times only; the
signal has a constant unwarped frequency.
 @p(Note 1:) @i(table) is a list of the form
@begin(display)
(@i(sound) @i(pitch-number) @i(periodic))
@end(display)
where the first element is a sound, the second is the pitch of the sound 
(this is not redundant, because the sound may represent any number of
periods), and the third element is @code(T) if the sound is one period of
a periodic signal, or @code(nil) if the sound is a sample that should not
be looped.  The maximum table size is set by @code(max_table_len) in @code(sound.h), and is currently set to 1,000,000.
@p(Note 2:) in the current implementation, it is assumed that the
output should be periodic.  See @code(snd-down) and @code(snd-up) for resampling one-shot sounds to a desired sample rate.  A future version of @code(osc)
will handle both cases.  
@p(Note 3:) When @code(osc) is called, memory is allocated for the table, and samples are copied from the sound (the first element of the list which is the @i(table) parameter) to the memory.  Every instance of @code(osc) has a private copy of the table, so the total storage can become large in some cases, for example in granular synthesis with many instances of @code(osc). In some cases, it may make sense to use @code(snd-flatten) (see Section @ref(flatten-sec)) to cause the sound to be fully realized, after which the @code(osc) and its table memory can be reclaimed by garbage collection. The @code(partial) function (see below) does not need a private table and does not use much space.

@label(partial-sec)
@codef{partial(@pragma(defn)@index(partial)@i(pitch), @i(env))} @c{[sal]}@*
@altdef{@code[(partial @i(pitch) @i(env))] @c{[lisp]}}@\Returns a sinusoid at
the indicated pitch; the sound is multiplied by @i(env).  The start time and
duration are taken from @i(env), which is of course subject to
transformations.  The sample rate is @code(*sound-srate*).  The @code(partial)
function is faster than @code(osc).

@label(sine-sec)
@codef{sine(@pragma(defn)@index(sine)@i(pitch) [, @i(duration)])} @c{[sal]}@*
@altdef{@code{(sine @i(pitch) [@i(duration)])} @c{[lisp]}}@\Returns a sinusoid at
the indicated pitch.  The sample rate is @code(*sound-srate*).  
This function is like @code(osc) with
respect to transformations.  The @code(sine) function is faster than
@code(osc).

@codef{hzosc(@pragma(defn)@index(hzosc)@i(hz) [, @i(table), @i(phase)])} @c{[sal]}@*
@altdef{@code{(hzosc @i(hz) [@i(table) @i(phase)])} @c{[lisp]}}@\Returns a sound which is the @i(table) oscillated at @i(hz) starting at @i(phase) degrees. The default @i(table) is @code(*table*) and the default @i(phase) is @i(0.0). The default duration is @code(1.0), but this is stretched as in @code(osc) (see above). The @i(hz) parameter may be a @code(SOUND), in which case the duration of the result is the duration of @i(hz). The sample rate is @code(*sound-srate*).

@codef{osc-saw(@pragma(defn)@index(osc-saw)@index(sawtooth oscillator)@i(hz))} @c{[sal]}@*
@altdef{@code[(osc-saw @i(hz))] @c{[lisp]}}@\Returns a sawtooth waveshape at the indicated frequency (in Hertz). The sample rate is @code(*sound-srate*). The @i(hz) parameter may be a sound as in @i(hzosc) (see above).

@codef{osc-tri(@pragma(defn)@index(osc-tri)@index(triangle oscillator)@i(hz))} @c{[sal]}@*
@altdef{@code[(osc-tri @i(hz))] @c{[lisp]}}@\Returns a triangle waveshape at the indicated frequency (in Hertz). The sample rate is @code(*sound-srate*). The @i(hz) parameter may be a sound as in @i(hzosc) (see above).

@codef{osc-pulse(@pragma(defn)@index(osc-pulse)@index(square oscillator)@index(pulse oscillator)@index(pulse-width modulation)@i(hz), @i(bias) [, @i(compare-shape)])} @c{[sal]}@*
@altdef{@code{(osc-pulse @i(hz) @i(bias) [@i(compare-shape)])} @c{[lisp]}}@\Returns a square pulse with variable width at the indicated frequency (in Hertz). The @i(bias) parameter controls the pulse width and should be between @code(-1) and @code(+1), giving a pulse width from 0% (always at @code(-1)) to 100% (always at @code(+1)). When bias is zero, a square wave is generated. Bias may be a @code(SOUND) to create varying pulse width. If bias changes rapidly, strange effects may occur. The optional @i(compare-shape) defaults to a hard step at zero, but other shapes may be used to achieve non-square pulses. The @code(osc-pulse) behavior is written in terms of other behaviors and defined in the file @code(nyquist.lsp) using just a few lines of code. Read the code for the complete story.

@label(amosc-sec)
@codef{amosc(@pragma(defn)@index(amosc)@i(pitch), @i(modulation) [, @i(table),
@i(phase)])} @c{[sal]}@*
@altdef{@code{(amosc @i(pitch) @i(modulation) [@i(table) @i(phase)])} @c{[lisp]}}@\Returns a
sound which is @i(table) oscillated at @i(pitch).  The output
is multiplied by @i(modulation)
for the duration of the sound @i(modulation).  
@i(osc-table) defaults to
@code(*table*), and @i(phase) is the starting phase (default 0.0 degrees)
within @i(osc-table).  The sample rate is @code(*sound-srate*).  

@label(fmosc-sec)
@codef{fmosc(@pragma(defn)@index(fmosc)@i(pitch), @i(modulation) [, @i(table),
@i(phase)])} @c{[sal]}@*
@altdef{@code{(fmosc @i(pitch) @i(modulation) [@i(table) @i(phase)])} @c{[lisp]}}@\Returns a
sound which is @i(table) oscillated at @i(pitch) plus @i(modulation)
for the duration of the sound @i(modulation).  
@i(osc-table) defaults to
@code(*table*), and @i(phase) is the starting phase (default 0.0 degrees)
within @i(osc-table).  The @i(modulation)
is expressed in hz, e.g. a sinusoid modulation signal with an
amplitude of 1.0 (2.0 peak to peak), will cause a +/@subtract 1.0 hz 
frequency deviation in @i(sound).  Negative frequencies are correctly
handled.  The sample rate is @code(*sound-srate*).  

@label(fmfb-sec)
@codef{fmfb(@pragma(defn)@index(fmfb)@index(Feedback FM Oscillator)@i(pitch), @i(index) [, @i(dur)])} @c{[sal]}@*
@altdef{@code{(fmfb @i(pitch) @i(index) [@i(dur)])} @c{[lisp]}}@\Returns
a sound generated by feedback FM synthesis. The @i(pitch) parameter
(given in the usual half-step units) 
controls the fundamental frequency. The @i(index) is the amount of
feedback, which may be a @code(SOUND) or a @code(FLONUM). If @i(index) is
a @code(FLONUM), @i(dur) must be provided (a @code(FLONUM)) to specify
the duration. Otherwise, @i(dur) is ignored if present and the duration is
determined by that of @i(index). The sample rate is @code(*sound-srate*).
A sinusoid table is used.
If @i(index) is below 1.1, this generates a sawtooth-like waveform.

@label(buzz-sec)
@codef{buzz(@pragma(defn)@index(buzz)@i(n), @i(pitch), @i(modulation))} @c{[sal]}@*
@altdef{@code[(buzz @i(n) @i(pitch) @i(modulation))] @c{[lisp]}}@\Returns a
sound with @i(n) harmonics of equal amplitude and a total amplitude
of 1.0, using a well-known function of two cosines. If @i(n) (an integer)
is less than 1, it is set to 1. Aliasing will occur if @i(n) is too large.
The duration is
determined by the duration of the sound @i(modulation), which is a
frequency modulation term expressed in Hz (see Section @ref(fmosc-sec)).
Negative frequencies are correctly handled.
The sample rate is @code(*sound-srate*).

@label(pluck-sec)
@codef{pluck(@pragma(defn)@index(pluck)@index(Karplus-Strong)@index(string synthesis)@index(plucked string)@i(pitch) [, @i(duration), @i(final-amplitude)])} @c{[sal]}@*
@altdef{@code{(pluck @i(pitch) [@i(duration) @i(final-amplitude)])} @c{[lisp]}}@\Returns a sound at the 
given @i(pitch) created using a modified Karplus-Strong plucked string
algorithm. The tone decays from an amplitude of about 1.0 to about
@i(final-amplitude) in @i(duration) seconds. The default values are to
decay to 0.001 (-60dB) in 1 second. The sample rate is @code(*sound-srate*).

@label(siosc-sec)
@codef{siosc(@pragma(defn)@index(siosc)@index(spectral interpolation)@i(pitch),
@i(modulation), @i(tables))} @c{[sal]}@*
@altdef{@code[(siosc @i(pitch) @i(modulation) @i(tables))] @c{[lisp]}}@\Returns a sound constructed by
interpolating through a succession of periodic waveforms. The frequency is
given (in half steps) by @i(pitch) to which a @i(modulation) signal (in hz)
is added, exactly as in @code(fmosc). The @i(tables) specify a list of
waveforms as follows: (@i(table0) @i(time1) @i(table2) ... @i(timeN)
@i(tableN)), where each @i(table) is a sound representing one period. Each
@i(time) is a time interval measured from the starting time. The time is
scaled by the nominal duration (computed using @code[(local-to-global
(get-sustain))]) to get the actual time. Note that this implies linear
stretching rather than continuous timewarping of the interpolation or the
breakpoints. The waveform is @i(table0) at the starting time, @i(table1)
after @i(time1) (scaled as described), and so on. The duration and logical
stop time is given by @i(modulation). If @i(modulation) is shorter than
@i(timeN), then the full sequence of waveforms is not used.  If
@i(modulation) is longer than @i(timeN), @i(tableN) is used after @i(timeN)
without further interpolation.


@label(sampler-sec)
@codef{sampler(@pragma(defn)@index(sampler)@i(pitch), @i(modulation)
 [, @i(sample), @i(npoints)])} @c{[sal]}@*
@altdef{@code{(sampler @i(pitch) @i(modulation) [@i(sample) @i(npoints)])} @c{[lisp]}}@\Returns a sound constructed by reading a sample from 
beginning to end and then splicing on copies of the same sound from 
a loop point to the end.  
The @i(pitch) and @i(modulation) parameters are used as in @code(fmosc)
described above.  The optional @i(sample) (which defaults to the global
variable @code(*table*) is a list of the form
@begin(display)
(@i(sound) @i(pitch-number) @i(loop-start))
@end(display)
where the first element is a sound containing the sample, the second is the
pitch of the sample, and the third element is the time of the loop point. If
the loop point is not in the bounds of the sound, it is set to zero.
The optional @i(npoints) specifies how many points should be used for sample
interpolation.  Currently this parameter defaults to 2 and only 2-point
(linear) interpolation is implemented.  It is an error to modulate such that the frequency
is negative. Note also that the loop point may be fractional.
The sample rate is @code(*sound-srate*).  
@end(fndefs)

@paragraph(Piece-wise Approximations)
@index(piece-wise)@index(approximation)@index(splines)
There are a number of related behaviors for piece-wise approximations to functions.  The simplest of these, @code(pwl) was mentioned earlier in the manual.  It takes a list of breakpoints, assuming an initial point at (0, 0), and a final value of 0.  An analogous piece-wise exponential function, @code(pwe), is provided. Its implicit starting and stopping values are 1 rather than 0.  Each of these has variants.   You can specify the initial and final values (instead of taking the default).  You can specify time in intervals rather than cummulative time.  Finally, you can pass a list rather than an argument list.  This leads to 16 versions:
@pragma(startscribe)
@begin(display)
@tabclear
@tabset(0.4 inches, 0.8 inches, 1.2 inches)
Piece-wise Linear Functions:
@\Cummulative Time:
@\@\Default initial point at (0, 0), final value at 0:
@\@\@\@code(pwl)
@\@\@\@code(pwl-list)
@\@\Explicit initial value:
@\@\@\@code(pwlv)
@\@\@\@code(pwlv-list)
@\Relative Time:
@\@\Default initial point at (0, 0), final value at 0:
@\@\@\@code(pwlr)
@\@\@\@code(pwlr-list)
@\@\Explicit initial value:
@\@\@\@code(pwlvr)
@\@\@\@code(pwlvr-list)
Piece-wise Exponential Functions:
@\Cummulative Time:
@\@\Default initial point at (0, 1), final value at 1:
@\@\@\@code(pwe)
@\@\@\@code(pwe-list)
@\@\Explicit initial value:
@\@\@\@code(pwev)
@\@\@\@code(pwev-list)
@\Relative Time:
@\@\Default initial point at (0, 1), final value at 1:
@\@\@\@code(pwer)
@\@\@\@code(pwer-list)
@\@\Explicit initial value:
@\@\@\@code(pwevr)
@\@\@\@code(pwevr-list)
@end(display)
@pragma(endscribe)
@html[<pre><b>Piece-wise Linear Functions:</b>
	<i>Cummulative Time:</i>
		<i>Default initial point at (0, 0), final value at 0:</i>
			pwl
			pwl-list
		<i>Explicit initial value:</i>
			pwlv
			pwlv-list
	<i>Relative Time:</i>
		<i>Default initial point at (0, 0), final value at 0:</i>
			pwlr
			pwlr-list
		<i>Explicit initial value:</i>
			pwlvr
			pwlvr-list

<b>Piece-wise Exponential Functions:</b>
	<i>Cummulative Time:</i>
		<i>Default initial point at (0, 1), final value at 1:</i>
			pwe
			pwe-list
		<i>Explicit initial value:</i>
			pwev
			pwev-list
	<i>Relative Time:</i>
		<i>Default initial point at (0, 1), final value at 1:</i>
			pwer
			pwer-list
		<i>Explicit initial value:</i>
			pwevr
			pwevr-list
</pre>]
All of these functions are implemented in terms of @code(pwl) (see @code(nyquist.lsp) for the implementations.  There are infinite opportunities for errors in these functions: if you leave off a data point, try to specify points in reverse order, try to create an exponential that goes to zero or negative values, or many other bad things, the behavior is not well-defined.  Nyquist should not crash, but Nyquist does not necessarily attempt to report errors at this time.

@begin(fndefs)
@label(pwl-sec)
@codef{pwl(@pragma(defn)@index(pwl)@i(t@-[1]), @i(l@-[1]), @i(t@-[2]), @i(l@-[2]), @r(...) @i(t@-[n]))} @c{[sal]}@*
@altdef{@code[(pwl @i(t@-[1]) @i(l@-[1]) @i(t@-[2]) @i(l@-[2]) @r(...) @i(t@-[n]))] @c{[lisp]}}@\Creates
a piece-wise linear envelope with breakpoints at (0, 0), (@i(t@-[1]),
@i(l@-[1])), (@i(t@-[2]), @i(l@-[2])), ... (@i(t@-[n]), 0).  The breakpoint
times are scaled linearly by the value of @code(*sustain*) (if
@code(*sustain*) is a @code(SOUND), it is evaluated once at the starting
time of the envelope).  Each breakpoint time is then mapped according to
@code(*warp*).  The result is a linear interpolation (unwarped) between 
the breakpoints.  The sample rate is @code(*control-srate*).  Breakpoint
times are quantized to the nearest sample time.  If you specify one or more
breakpoints withing one sample period, @code(pwl) attempts to give a good
approximation to the specified function.  In particular, if two breakpoints
are simultaneous, @code(pwl) will move one of them to an adjacent sample,
producing a steepest possible step in the signal.  The exact details of this
``breakpoint munging'' is subject to change in future versions.  Please report
any cases where breakpoint lists give unexpected behaviors.  The author will
try to apply the ``principle of least surprise'' to the design.  Note that
the times are relative to 0; they are not durations of each envelope
segment.

@codef{pwl-list(@pragma(defn)@index(pwl-list)@i(breakpoints))} @c{[sal]}@*
@altdef{@code[(pwl-list @i(breakpoints))] @c{[lisp]}}@\If you have a list of
breakpoints, you can use @code(apply) to apply the @code(pwl) function to
the breakpoints, but if the list is very long (hundreds or thousands of
points), you might get a stack overflow because XLISP has a fixed-size
argument stack.  Instead, call @code(pwl-list), passing one argument, the
list of breakpoints.

@codef{pwlv(@pragma(defn)@index(pwlv)@i(l@-[1]), @i(t@-[2]), @i(l@-[2]), @i(t@-[3]), @i(t@-[3]), ... @i(t@-[n]), @i(l@-[n]))} @c{[sal]}@*
@altdef{@code[(pwlv @i(l@-[1]) @i(t@-[2]) @i(l@-[2]) @i(t@-[3]) @i(t@-[3]) @r(...) @i(t@-[n]) @i(l@-[n]))] @c{[lisp]}}@\Creates
a piece-wise linear envelope with breakpoints at (0, l@-[1]), (@i(t@-[2]), @i(l@-[2])), etc., ending with (@i(t@-[n], @i(l@-[n])).  Otherwise, the behavior is like that of @code(pwl).

@codef{pwlv-list(@pragma(defn)@index(pwlv-list)@i(breakpoints))} @c{[sal]}@*
@altdef{@code[(pwlv-list @i(breakpoints))] @c{[lisp]}}@\A version of @code(pwlv) that takes a single list of breakpoints as its argument.  See @code(pwl-list) above for the rationale.

@codef{pwlr(@pragma(defn)@index(pwlr)@i(i@-[1]), @i(l@-[1]), @i(i@-[2]), @i(l@-[2]), ... @i(i@-[n]))} @c{[sal]}@*
@altdef{@code[(pwlr @i(i@-[1]) @i(l@-[1]) @i(i@-[2]) @i(l@-[2]) @r(...) @i(i@-[n]))] @c{[lisp]}}@\Creates
a piece-wise linear envelope with breakpoints at (0, 0), (@i(t@-[1]),
@i(l@-[1])), (@i(t@-[2]), @i(l@-[2])), ... (@i(t@-[n]), 0), where @i(t@-[j]) is the sum of @i(i@-[1]) through @i(i@-[j]).  In other words, the breakpoint times are specified in terms of intervals rather than cummulative time.   Otherwise, the behavior is like that of @code(pwl).

@codef{pwlr-list(@pragma(defn)@index(pwlr-list)@i(breakpoints))} @c{[sal]}@*
@altdef{@code[(pwlr-list @i(breakpoints))] @c{[lisp]}}@\A version of @code(pwlr) that takes a single list of breakpoints as its argument.  See @code(pwl-list) above for the rationale.

@codef{pwlvr(@pragma(defn)@index(pwlvr)@i(l@-[1]), @i(i@-[2]), @i(l@-[2]), @i(i@-[3]), @i(i@-[3]), ... @i(i@-[n]), @i(l@-[n]))} @c{[sal]}@*
@altdef{@code[(pwlvr @i(l@-[1]) @i(i@-[2]) @i(l@-[2]) @i(i@-[3]) @i(i@-[3]) @r(...) @i(i@-[n]) @i(l@-[n]))] @c{[lisp]}}@\Creates
a piece-wise linear envelope with breakpoints at (0, l@-[1]), (@i(t@-[2]), @i(l@-[2])), etc., ending with (@i(t@-[n], @i(l@-[n])), where @i(t@-[j]) is the sum of @i(i@-[2]) through @i(i@-[j]).  In other words, the breakpoint times are specified in terms of intervals rather than cummulative time.   Otherwise, the behavior is like that of @code(pwlv).

@codef{pwlvr-list(@pragma(defn)@index(pwlvr-list)@i(breakpoints))} @c{[sal]}@*
@altdef{@code[(pwlvr-list @i(breakpoints))] @c{[lisp]}}@\A version of @code(pwlvr) that takes a single list of breakpoints as its argument.  See @code(pwl-list) above for the rationale.

@codef{pwe(@pragma(defn)@index(pwe)@i(t@-[1]), @i(l@-[1]), @i(t@-[2]), @i(l@-[2]), @r(...) @i(t@-[n]))} @c{[sal]}@*
@altdef{@code[(pwe @i(t@-[1]) @i(l@-[1]) @i(t@-[2]) @i(l@-[2]) @r(...) @i(t@-[n]))] @c{[lisp]}}@\Creates
a piece-wise exponential envelope with breakpoints at (0, 1), (@i(t@-[1]),
@i(l@-[1])), (@i(t@-[2]), @i(l@-[2])), ... (@i(t@-[n]), 1).  Exponential segments means that the ratio of values from sample to sample is constant within the segment.  (The current implementation actually takes the log of each value, computes a piece-wise exponential from the points using @code(pwl), then exponentiates each resulting sample.  A faster implementation is certainly possible!)  Breakpoint values (@i(l@-[j])) must be greater than zero.  Otherwise, this function is similar to @code(pwl), including stretch by @code(*sustain*), mapping according to @code(*warp*), sample rate based on @code(*control-srate*), and "breakpoint munging" (see @code(pwl) described above).  @i(Default initial and final values are of dubious value with exponentials.  See @code(pwev) below for the function you are probably looking for.)

@codef{pwe-list(@pragma(defn)@index(pwe-list)@i(breakpoints))} @c{[sal]}@*
@altdef{@code[(pwe-list @i(breakpoints))] @c{[lisp]}}@\A version of @code(pwe) that takes a single list of breakpoints as its argument.  See @code(pwl-list) above for the rationale.

@label(pwev-sec)
@codef{pwev(@pragma(defn)@index(pwev)@i(l@-[1]), @i(t@-[2]), @i(l@-[2]), @i(t@-[3]), @i(t@-[3]), @r(...) @i(t@-[n]), @i(l@-[n]))} @c{[sal]}@*
@altdef{@code[(pwev @i(l@-[1]) @i(t@-[2]) @i(l@-[2]) @i(t@-[3]) @i(t@-[3]) @r(...) @i(t@-[n]) @i(l@-[n]))] @c{[lisp]}}@\Creates
a piece-wise exponential envelope with breakpoints at (0, @i(l@-[1])), (@i(t@-[2]), @i(l@-[2])), etc., ending with (@i(t@-[n]), @i(l@-[n])).  Otherwise, the behavior is like that of @code(pwe).  

@codef{pwev-list(@pragma(defn)@index(pwev-list)@i(breakpoints))} @c{[sal]}@*
@altdef{@code[(pwev-list @i(breakpoints))] @c{[lisp]}}@\A version of @code(pwev) that takes a single list of breakpoints as its argument.  See @code(pwl-list) above for the rationale.

@codef{pwer(@pragma(defn)@index(pwer)@i(i@-[1]), @i(l@-[1]), @i(i@-[2]), @i(l@-[2]), @r(...) @i(i@-[n]))} @c{[sal]}@*
@altdef{@code[(pwer @i(i@-[1]) @i(l@-[1]) @i(i@-[2]) @i(l@-[2]) @r(...) @i(i@-[n]))] @c{[lisp]}}@\Creates
a piece-wise exponential envelope with breakpoints at (0, 1), (@i(t@-[1]),
@i(l@-[1])), (@i(t@-[2]), @i(l@-[2])), ... (@i(t@-[n]), 1), where @i(t@-[j]) is the sum of @i(i@-[1]) through @i(i@-[j]).  In other words, the breakpoint times are specified in terms of intervals rather than cummulative time.   Otherwise, the behavior is like that of @code(pwe).  Consider using @code(pwerv) instead of this one.

@codef{pwer-list(@pragma(defn)@index(pwer-list)@i(breakpoints))} @c{[sal]}@*
@altdef{@code[(pwer-list @i(breakpoints))] @c{[lisp]}}@\A version of @code(pwer) that takes a single list of breakpoints as its argument.  See @code(pwl-list) above for the rationale.

@codef{pwevr(@index(GEN05)@pragma(defn)@index(pwevr)@i(l@-[1]), @i(i@-[2]), @i(l@-[2]), @i(i@-[3]), @i(i@-[3]), @r(...) @i(i@-[n]), @i(l@-[n]))} @c{[sal]}@*
@altdef{@code[(pwevr @i(l@-[1]) @i(i@-[2]) @i(l@-[2]) @i(i@-[3]) @i(i@-[3]) @r(...) @i(i@-[n]) @i(l@-[n]))] @c{[lisp]}}@\Creates
a piece-wise exponential envelope with breakpoints at (0, l@-[1]), (@i(t@-[2]), @i(l@-[2])), etc., ending with (@i(t@-[n], @i(l@-[n])), where @i(t@-[j]) is the sum of @i(i@-[2]) through @i(i@-[j]).  In other words, the breakpoint times are specified in terms of intervals rather than cummulative time.   Otherwise, the behavior is like that of @code(pwev).  Note that this is similar to the csound GEN05 generator.  Which is uglier, @i(GEN05) or @i(pwevr)?

@codef{pwevr-list(@pragma(defn)@index(pwevr-list)@i(breakpoints))} @c{[sal]}@*
@altdef{@code[(pwevr-list @i(breakpoints))] @c{[lisp]}}@\A version of @code(pwevr) that takes a single list of breakpoints as its argument.  See @code(pwl-list) above for the rationale.
@end(fndefs)
@paragraph(Filter Behaviors)
@begin(fndefs)
@label(alpass-sec)
@codef{alpass(@index(all pass filter)@index(alpass filter)@pragma(defn)@index(alpass)@i(sound), @i(decay), @i(hz) [, @i(minhz)])} @c{[sal]}@*
@altdef{@code{(alpass @i(sound) @i(decay) @i(hz) [@i(minhz)])} @c{[lisp]}}@\Applies an all-pass filter to @i(sound).  This all-pass filter creates a delay effect without the resonances of a comb filter. The decay time of the filter is given by @i(decay).  The @i(hz) parameter must be a number or sound greater than zero.  It is used to compute delay, which is then rounded to the nearest integer number of samples (so the frequency is not always exact.  Higher sampling rates yield better delay resolution.)  The @i(decay) may be a sound or a number.  In either case, it must also be positive.  (Implementation note: an exponentiation is needed to convert @i(decay) into the @i(feedback) parameter, and exponentiation is typically more time-consuming than the filter operation itself.  To get high performance, provide @i(decay) at a low sample rate.)  The resulting sound will have the start time, sample rate, etc. of @i(sound). If @i(hz) is of type @code(SOUND), the delay may be time-varying. Linear interpolation is then used for fractional sample delay, but it should be noted that linear interpolation implies a low-pass transfer function. Thus, this filter may behave differently with a constant @code(SOUND) than it does with a @code(FLONUM) value for @i(hz). In addition, if @i(hz) is of type @code(SOUND), then @i(minhz) is required. The @i(hz) parameter will be clipped to be greater than @i(minhz), placing an upper bound on the delay buffer length.

@label(comb-sec)
@codef{comb(@pragma(defn)@index(comb)@index(comb filter)@i(sound), @i(decay), @i(hz))} @c{[sal]}@*
@altdef{@code[(comb @i(sound) @i(decay) @i(hz))] @c{[lisp]}}@\Applies a comb filter to @i(sound).  A comb filter emphasizes (resonates at) frequencies that are multiples of a @i(hz). The decay time of the resonance is given by @i(decay).  This is a variation on @code(feedback-delay) (see below).  The @i(hz) parameter must be a number greater than zero.  It is used to compute delay, which is then rounded to the nearest integer number of samples (so the frequency is not always exact.  Higher sampling rates yield better delay resolution.)  The @i(decay) may be a sound or a number.  In either case, it must also be positive.  (Implementation note: an exponentiation is needed to convert @i(decay) into the @i(feedback) parameter for @code(feedback-delay), and exponentiation is typically more time-consuming than the filter operation itself.  To get high performance, provide @i(decay) at a low sample rate.)  The resulting sound will have the start time, sample rate, etc. of @i(sound).

@label(congen-sec)
@codef{congen(@pragma(defn)@index(congen)@index(contour generator)@index(envelope generator)@i(gate), @i(risetime), @i(falltime))} @c{[sal]}@*
@altdef{@code[(congen @i(gate) @i(risetime) @i(falltime))] @c{[lisp]}}@\Implements an analog synthesizer-style contour generator. The input @i(gate) normally goes from 0.0 to 1.0 to create an attack and from 1.0 to 0.0 to start a release.  During the attack (output is increasing), the output converges half-way to @i(gate) in @i(risetime) (a @code(FLONUM)) seconds. During the decay, the half-time is @i(falltime) seconds. The sample rate, start time, logical stop, and terminate time all come from @i(gate). If you want a nice decay, be sure that the @i(gate) goes to zero and stays there for awhile before @i(gate) terminates, because @code(congen) (and all Nyquist sounds) go immediately to zero at termination time.  For example, you can use @code(pwl) to build a pulse followed by some zero time:
@begin(example)
(pwl 0 1 duty 1 duty 0 1)
@end(example)
Assuming @i(duty) is less than 1.0, this will be a pulse of duration @i(duty) followed by zero for a total duration of 1.0.
@begin(example)
(congen (pwl 0 1 duty 1 duty 0 1) 0.01 0.05)
@end(example)
will have a duration of 1.0 because that is the termination time of the @code(pwl) input. The decaying release of the resulting envelope will be truncated to zero at time 1.0. (Since the decay is theoretically infinite, there is no way to avoid truncation, although you could multiply by another envelope that smoothly truncates to zero in the last millisecond or two to get both an exponential decay and a smooth final transition to zero.)

@label(convolve-sec)
@codef{convolve(@pragma(defn)@index(convolve)@index(convolution)@index(FIR filter)@i(sound),
@i(response))} @c{[sal]}@*
@altdef{@code[(convolve @i(sound) @i(response))] @c{[lisp]}}@\Convolves two signals. The first can be any length, but the
computation time per sample and the total space required are proportional to
the length of @i(response). The start time, logical stop time, and sample 
rate of the output match those of the input @i(sound). The physical stop 
time of the result is the physical stop time of @i(sound) plus the duration 
of the @i(response) so that the result sound includes the ``tail'' of the 
filter response. The response is assumed to have the same sample rate as @i(sound). The samples are used as is without resampling.

@label(feedback-delay-sec)
@codef{feedback-delay(@pragma(defn)@index(feedback-delay)@index(delay)@index(echo)@i(sound), @i(delay), @i(feedback))} @c{[sal]}@*
@altdef{@code[(feedback-delay @i(sound) @i(delay) @i(feedback))] @c{[lisp]}}@\Applies feedback delay to @i(sound).  The @i(delay) must be a number (in seconds).  It is rounded to the nearest sample to determine the length of the delay.  The sample rate is the maximum from @i(sound) and @i(feedback) (if feedback is also a sound).  The amound of @i(feedback) should be less than one to avoid an exponential increase in amplitude.  The start time and stop time, and logical stop time are taken from @i(sound).  Since output is truncated at the stop time of @i(sound), you may want to append some silence to @i(sound) to give the filter time to decay.

@label(lp-sec)
@codef{lp(@pragma(defn)@index(lp)@index(low-pass filter)@i(sound), @i(cutoff))} @c{[sal]}@*
@altdef{@code[(lp @i(sound) @i(cutoff))] @c{[lisp]}}@\Filters @i(sound)
using a first-order Butterworth low-pass filter.  @i(Cutoff) may be a float
or a signal (for time-varying filtering) and expresses hertz.  Filter
coefficients (requiring trig functions) are recomputed at the sample rate of
@i(cutoff).  The resulting sample rate, start time, etc. are taken from @i(sound).

@codef{tone(@pragma(defn)@index(tone)@i(sound), @i(cutoff))} @c{[sal]}@*
@altdef{@code[(tone @i(sound) @i(cutoff))] @c{[lisp]}}@\No longer defined; use @code(lp) instead, or define it by adding @code[(setfn tone lp)] to your program.


@label(hp-sec)
@codef{hp(@pragma(defn)@index(hp)@index(high-pass filter)@i(sound), @i(cutoff))} @c{[sal]}@*
@altdef{@code[(hp @i(sound) @i(cutoff))] @c{[lisp]}}@\Filters @i(sound)
using a first-order Butterworth high-pass filter.  @i(Cutoff) may be a
float or a signal (for time-varying filtering) and expresses hertz.  Filter
coefficients (requiring trig functions) are recomputed at the sample rate of
@i(cutoff).  This filter is an exact complement of @code(lp).

@codef{atone(@pragma(defn)@index(atone)@i(sound), @i(cutoff))} @c{[sal]}@*
@altdef{@code[(atone @i(sound) @i(cutoff))] @c{[lisp]}}@\No longer defined; use @code(hp) instead, or define it by adding @code[(setfn atone hp)] to your program.

@label(reson-sec)
@codef{reson(@pragma(defn)@index(reson)@index(bandpass filter)@i(sound), @i(center), @i(bandwidth), @i(n))} @c{[sal]}@*
@altdef{@code[(reson @i(sound) @i(center) @i(bandwidth) @i(n))] @c{[lisp]}}@\Apply
a resonating filter to @i(sound) with center frequency @i(center) (in hertz),
which may be a float or a signal.  @i(Bandwidth) is the filter bandwidth (in
hertz), which may also be a signal.  Filter coefficients (requiring trig
functions) are recomputed at each new sample of either @i(center) or
@i(bandwidth), and coefficients are @i(not) interpolated.  The last
parameter @i(n) specifies the type of normalization as in Csound: A value of 1 specifies a peak amplitude
response of 1.0; all frequencies other than @i(hz) are attenuated.  A
value of 2 specifies the overall RMS value of the amplitude response
is 1.0; thus filtered white noise would retain the same power.  A value of
zero specifies no scaling.  The resulting sample rate, start time, etc. are taken from @i(sound).

One application of @code(reson) is to simulate resonances in the human vocal tract.
See @code(demos/voice_synthesis.htm)@index(voice synthesis)@index(demos, voice synthesis)
for sample code and documentation.

@label(areson-sec)
@codef{areson(@pragma(defn)@index(areson)@index(notch filter)@i(sound), @i(center), @i(bandwidth), @i(n))} @c{[sal]}@*
@altdef{@code[(areson @i(sound) @i(center) @i(bandwidth) @i(n))] @c{[lisp]}}@\The @code(areson) filter is an exact
complement of @code(reson) such that if both are applied to the
same signal with the same parameters, the sum of the results yeilds
the original signal.

@label(shape-sec)
@codef{shape(@pragma(defn)@index(shape)@index(waveshaping)@index(table)@i(signal), @i(table), @i(origin))} @c{[sal]}@*
@altdef{@code[(shape @i(signal) @i(table) @i(origin))] @c{[lisp]}}@\A waveshaping function.  Use @i(table) as a function; apply the function to each sample of @i(signal) to yield a new sound.  @i(Signal) should range from -1 to +1.  Anything beyond these bounds is clipped.  @i(Table) is also a sound, but it is converted into a lookup table (similar to table-lookup oscillators).  The @i(origin) is a @code(FLONUM) and gives the time which should be considered the origin of @i(table).  (This is important because @i(table) cannot have values at negative times, but @i(signal) will often have negative values.  The @i(origin) gives an offset so that you can produce suitable tables.)  The output at time @i(t) is:
@begin(display)
@i(table)(@i(origin) + clip(@i(signal)(@i(t)))
@end(display)
where clip(@i(x)) = @i(max)(1, @i(min)(-1, @i(x))).
(E.g. if @i(table) is a signal defined over the interval [0, 2], then @i(origin) should be 1.0.  The value of @i(table) at time 1.0 will be output when the input signal is zero.)  The output has the same start time, sample rate, etc. as @i(signal).  The @code(shape) function will also accept multichannel @i(signal)s and @i(table)s.

Further discussion and examples can be found in 
@code(demos/distortion.htm)@index(distortion tutorial)@index(demos, distortion). 
The @code(shape)
function is also used to map frequency to amplitude to achieve a spectral envelope for
Shepard tones in @code(demos/shepard.lsp).@index(Shepard tones)@index(demos, Shepard tones)

@label(biquad-sec)
@codef{biquad(@pragma(defn)@index(biquad)@i(signal), @i(b0), @i(b1), @i(b2), @i(a0), @i(a1), @i(a2))} @c{[sal]}@*
@altdef{@code[(biquad @i(signal) @i(b0) @i(b1) @i(b2) @i(a0) @i(a1) @i(a2))] @c{[lisp]}}@\A fixed-parameter biquad filter. All filter coefficients are @code(FLONUM)s. See also @code(lowpass2), @code(highpass2), @code(bandpass2), @code(notch2), @code(allpass2), @code(eq-lowshelf), @code(eq-highshelf), @code(eq-band), @code(lowpass4), @code(lowpass6), @code(highpass4), and @code(highpass8) in this section for convenient variations based on the same filter. The equations for the filter are: z@-[n] = s@-[n] + a1 * z@-[n-1] + a2 * z@-[n-2], and y@-[n] = z@-[n] * b0 + z@-[n-1] * b1 + z@-[n-2] * b2.

@label(biquad-m-sec)
@codef{biquad-m(@pragma(defn)@index(biquad-m)@i(signal), @i(b0), @i(b1), @i(b2), @i(a0), @i(a1), @i(a2))} @c{[sal]}@*
@altdef{@code[(biquad-m @i(signal) @i(b0) @i(b1) @i(b2) @i(a0) @i(a1) @i(a2))] @c{[lisp]}}@\A fixed-parameter biquad filter with Matlab sign conventions for @i(a0), @i(a1), and @i(a2). All filter coefficients are @code(FLONUM)s.

@label(lowpass2-sec)
@codef{lowpass2(@pragma(defn)@index(lowpass2)@i(signal), @i(hz) [, @i(q)])} @c{[sal]}@*
@altdef{@code{(lowpass2 @i(signal) @i(hz) [@i(q)])} @c{[lisp]}}@\A fixed-parameter, second-order lowpass filter based on @code(snd-biquad). The cutoff frequency is given by @i(hz) (a @code(FLONUM)) and an optional Q factor is given by @i(q) (a @code(FLONUM)).

@label(highpass2-sec)
@codef{highpass2(@pragma(defn)@index(highpass2)@i(signal), @i(hz) [, @i(q)])} @c{[sal]}@*
@altdef{@code{(highpass2 @i(signal) @i(hz) [@i(q)])} @c{[lisp]}}@\A fixed-parameter, second-order highpass filter based on @code(snd-biquad). The cutoff frequency is given by @i(hz) (a @code(FLONUM)) and an optional Q factor is given by @i(q) (a @code(FLONUM)).

@label(bandpass2-sec)
@codef{bandpass2(@pragma(defn)@index(bandpass2)@i(signal), @i(hz) [, @i(q)])} @c{[sal]}@*
@altdef{@code{(bandpass2 @i(signal) @i(hz) [@i(q)])} @c{[lisp]}}@\A fixed-parameter, second-order bandpass filter based on @code(snd-biquad). The center frequency is given by @i(hz) (a @code(FLONUM)) and an optional Q factor is given by @i(q) (a @code(FLONUM)).

@label(notch2-sec)
@codef{notch2(@pragma(defn)@index(notch2)@i(signal), @i(hz) [, @i(q)])} @c{[sal]}@*
@altdef{@code{(notch2 @i(signal) @i(hz) [@i(q)])} @c{[lisp]}}@\A fixed-parameter, second-order notch filter based on @code(snd-biquad). The center frequency is given by @i(hz) (a @code(FLONUM)) and an optional Q factor is given by @i(q) (a @code(FLONUM)).

@label(allpass2-sec)
@codef{allpass2(@pragma(defn)@index(allpass2)@i(signal), @i(hz) [, @i(q)])} @c{[sal]}@*
@altdef{@code{(allpass2 @i(signal) @i(hz) [@i(q)])} @c{[lisp]}}@\A fixed-parameter, second-order allpass filter based on @code(snd-biquad). The frequency is given by @i(hz) (a @code(FLONUM)) and an optional Q factor is given by @i(q) (a @code(FLONUM)).

@label(eq-lowshelf-sec)
@codef{eq-lowshelf(@pragma(defn)@index(eq-lowshelf)@index(equalization)@i(signal), @i(hz), @i(gain) [, @i(slope)])} @c{[sal]}@*
@altdef{@code{(eq-lowshelf @i(signal) @i(hz) @i(gain) [@i(slope)])} @c{[lisp]}}@\A fixed-parameter, second-order bass shelving equalization (EQ) filter based on @code(snd-biquad). The @i(hz) parameter (a @code(FLONUM))is the halfway point in the transition, and @i(gain) (a @code(FLONUM)) is the bass boost (or cut) in dB. The optional @i(slope) (a @code(FLONUM)) is 1.0 by default, and response becomes peaky at values greater than 1.0.

@label(eq-highshelf-sec)
@codef{eq-highshelf(@pragma(defn)@index(eq-highshelf)@index(equalization)@i(signal), @i(hz), @i(gain) [, @i(slope)])} @c{[sal]}@*
@altdef{@code{(eq-highshelf @i(signal) @i(hz) @i(gain) [@i(slope)])} @c{[lisp]}}@\A fixed-parameter, second-order treble shelving equalization (EQ) filter based on @code(snd-biquad). The @i(hz) parameter (a @code(FLONUM))is the halfway point in the transition, and @i(gain) (a @code(FLONUM)) is the treble boost (or cut) in dB. The optional @i(slope) (a @code(FLONUM)) is 1.0 by default, and response becomes peaky at values greater than 1.0.

@label(eq-band-sec)
@codef{eq-band(@pragma(defn)@index(eq-band)@index(equalization)@i(signal), @i(hz), @i(gain), @i(width))} @c{[sal]}@*
@altdef{@code[(eq-band @i(signal) @i(hz) @i(gain) @i(width))] @c{[lisp]}}@\A fixed- or variable-parameter, second-order midrange equalization (EQ) filter based on @code(snd-biquad), @code(snd-eqbandcv) and @code(snd-eqbandvvv). The @i(hz) parameter (a @code(FLONUM)) is the center frequency, @i(gain) (a @code(FLONUM)) is the boost (or cut) in dB, and @i(width) (a @code(FLONUM)) is the half-gain width in octaves. Alternatively, @i(hz), @i(gain), and @i(width) may be @code(SOUND)s, but they must all have the same sample rate, e.g. they should all run at the control rate or at the sample rate.

@label(lowpass4-sec)
@codef{lowpass4(@pragma(defn)@index(lowpass4)@i(signal), @i(hz))} @c{[sal]}@*
@altdef{@code[(lowpass4 @i(signal) @i(hz))] @c{[lisp]}}@\A four-pole Butterworth lowpass filter. The cutoff frequency is @i(hz) (a @code(FLONUM)).

@label(lowpass6-sec)
@codef{lowpass6(@pragma(defn)@index(lowpass6)@i(signal), @i(hz))} @c{[sal]}@*
@altdef{@code[(lowpass6 @i(signal) @i(hz))] @c{[lisp]}}@\A six-pole Butterworth lowpass filter. The cutoff frequency is @i(hz) (a @code(FLONUM)).

@label(lowpass8-sec)
@codef{lowpass8(@pragma(defn)@index(lowpass8)@i(signal), @i(hz))} @c{[sal]}@*
@altdef{@code[(lowpass8 @i(signal) @i(hz))] @c{[lisp]}}@\An eight-pole Butterworth lowpass filter. The cutoff frequency is @i(hz) (a @code(FLONUM)).

@label(highpass4-sec)
@codef{highpass4(@pragma(defn)@index(highpass4)@i(signal), @i(hz))} @c{[sal]}@*
@altdef{@code[(highpass4 @i(signal) @i(hz))] @c{[lisp]}}@\A four-pole Butterworth highpass filter. The cutoff frequency is @i(hz) (a @code(FLONUM)).

@label(highpass6-sec)
@codef{highpass6(@pragma(defn)@index(highpass6)@i(signal), @i(hz))} @c{[sal]}@*
@altdef{@code[(highpass6 @i(signal) @i(hz))] @c{[lisp]}}@\A six-pole Butterworth highpass filter. The cutoff frequency is @i(hz) (a @code(FLONUM)).

@label(highpass8-sec)
@codef{highpass8(@pragma(defn)@index(highpass8)@i(signal), @i(hz))} @c{[sal]}@*
@altdef{@code[(highpass8 @i(signal) @i(hz))] @c{[lisp]}}@\An eight-pole Butterworth highpass filter. The cutoff frequency is @i(hz) (a @code(FLONUM)).

@label(tapv-sec)
@codef{tapv(@pragma(defn)@index(tapv)@index(variable delay)@index(tapped delay)@i(sound), @i(offset), 
@i(vardelay), @i(maxdelay))} @c{[sal]}@*
@altdef{@code[(tapv @i(sound) @i(offset) @i(vardelay) @i(maxdelay))] @c{[lisp]}}@\A delay line with a variable position tap. 
Identical to @code(snd-tapv). See it for details (@ref(snd-tapv-sec)).

@end(fndefs)

@paragraph(Effects)
@begin(fndefs)
@label(stkrev-sec)
@codef{nrev(@pragma(defn)@index(nrev)@index(reverb)@index(effect, 
reverberation)@index(STK nreverb)@i(sound), @i(decay), @i(mix))} @c{[sal]}@*
@altdef{@code[(nrev @i(sound) @i(decay) @i(mix))] @c{[lisp]}}

@codef{jcrev(@pragma(defn)@index(jcrev)@index(reverb)@index(effect,
 reverberation)@index(STK jcreverb)@i(sound), @i(decay), @i(mix))} @c{[sal]}@*
@altdef{@code[(jcrev @i(sound) @i(decay) @i(mix))] @c{[lisp]}}

@codef{prcrev(@pragma(defn)@index(prcrev)@index(reverb)@index(effect,
 reverberation)@index(STK prcreverb)@i(sound), @i(decay), @i(mix))} @c{[sal]}@*
@altdef{@code[(prcrev @i(sound) @i(decay) @i(mix))] @c{[lisp]}}
These reverbs (@code(nrev), @code(jcrev), and @code(prcrev)) are implemented 
in STK (running within Nyquist). @code(nrev) derives from Common Music's 
NRev,  which consists of 6 comb filters followed by 3 allpass filters, a
 lowpass filter, and another allpass in series followed by two allpass 
filters in parallel. @code(jcrev) is the John Chowning 
reverberator which is based on the use of networks of simple allpass 
and comb delay filters. This reverb implements three series allpass units, 
followed by four parallel comb filters, and two decorrelation delay 
lines in parallel at the output. @code(prcrev) is a Perry Cook's 
reverberator which is based on the Chowning/Moorer/Schroeder 
reverberators using networks of simple allpass and comb delay filters. 
This one implements two series allpass units and two parallel comb filters.
The @i(sound) input may be single or multi-channel. The @i(decay) time is
in seconds, and @i(mix) sets the mixture of input sound reverb sound, 
where 0.0 means input only (dry) and 1.0 means reverb only (wet).

@label(stkchorus-sec)
@codef{stkchorus(@pragma(defn)@index(stkchorus)@index(chorus)@index(effect, chorus)@index(STK chorus)@i(sound), @i(depth), @i(freq), @i(mix) [, @i(delay)])} @c{[sal]}@*
@altdef{@code{(stkchorus @i(sound) @i(depth) @i(freq) @i(mix) [@i(delay)])} @c{[lisp]}}@\Chorus 
implemented in STK. The input @i(sound) can be single or multi-channel.
The @code(FLONUM) parameters @i(depth) and @i(freq) set 
the modulation 
depth from 0 to 1 
and modulation frequency (in Hz), and @i(mix) sets the mixture
of input sound and chorused sound, where 0.0 means input sound only (dry)
and 1.0 means chorused sound only (wet). The parameter @i(delay) is a
 @code(FIXNUM) representing the median desired delay length in samples. 

@label(stkpitshift-sec)
@codef{pitshift(@pragma(defn)@index(pitshift)@index(pitch shift)@index(effect, pitch shift)@index(STK pitch shift)@i(sound), @i(shift), @i(mix))} @c{[sal]}@*
@altdef{@code[(pitshift @i(sound) @i(shift) @i(mix))] @c{[lisp]}}@\A pitch
 shifter implemented in STK. The input @i(sound), a single-channel
 or multi-channel @code(SOUND) is pitch-shifted by @i(shift), 
a @code(FLONUM) ratio. A value of 1.0 means no shift.  The parameter @i(mix)
 sets the mixture of input and shifted sounds. A value of 0.0 
means input only (dry) 
and a value of 1.0 means shifted sound only (wet).
@end(fndefs)

@paragraph(Physical Models)
@begin(fndefs)
@label(clarinet-sec)
@codef{clarinet(@pragma(defn)@index(clarinet)@index(stk clarinet)@i(step), @i(breath-env))} @c{[sal]}@*
@altdef{@code[(clarinet @i(step) @i(breath-env))] @c{[lisp]}}@\A 
physical model of a clarinet from STK. The @i(step) parameter is a @code(FLONUM) 
that controls the tube length, and the @i(breath-env) (a @code(SOUND)) 
controls the air pressure
and also determines the length of the resulting sound. The @i(breath-env) signal
should range from zero to one.

@codef{clarinet-freq(@index(clarinet)@pragma(defn)@index(clarinet-freq)@index(stk clarinet)@i(step), @i(breath-env), @i(freq-env))} @c{[sal]}@*
@altdef{@code[(clarinet-freq @i(step) @i(breath-env) @i(freq-env))] @c{[lisp]}}@\A variation of @code(clarinet)
that includes a variable frequency control, @i(freq-env), which specifies
frequency deviation in Hz. The duration of the resulting sound is the minimum
duration of @i(breath-env) and @i(freq-env). These parameters may be of type
@code(FLONUM) or @code(SOUND). @code(FLONUM)s are coerced into @code(SOUND)s
with a nominal duration arbitrarily set to 30.

@codef{clarinet-all(@index(clarinet)@pragma(defn)@index(clarinet-all)@index(stk clarinet)@i(step), @i(breath-env), @i(freq-env), @i(vibrato-freq), @i(vibrato-gain), 
@i(reed-stiffness), @i(noise))} @c{[sal]}@*
@altdef{@code[(clarinet-all @i(step) @i(breath-env) @i(freq-env) @i(vibrato-freq) @i(vibrato-gain) @i(reed-stiffness) @i(noise))] @c{[lisp]}}@\A variation of @code(clarinet-freq)
that includes controls @i(vibrato-freq) (a @code(FLONUM) for vibrato frequency in Hertz), 
@i(vibrato-gain) (a @code(FLONUM) for the amount of amplitude vibrato),
@i(reed-stiffness) (a @code(FLONUM) or @code(SOUND) controlling reed stiffness in the clarinet
model), and @i(noise) (a @code(FLONUM) or @code(SOUND) controlling noise amplitude in the input
air pressure). The @i(vibrato-gain) is a number from zero to one, where zero
indicates no vibrato, and one indicates a plus/minus 50% change in breath
envelope values. Similarly, the @i(noise) parameter ranges from zero to one where 
zero means no noise and one means white noise with a peak amplitude of
plus/minus 40% of the @i(breath-env). The @i(reed-stiffness) parameter varies
from zero to one.
The duration of the resulting sound is the minimum duration of
@i(breath-env), @i(freq-env), @i(reed-stiffness), and @i(noise). As with
@code(clarinet-freq), these parameters may be either @code(FLONUM)s or 
@code(SOUND)s, and @code(FLONUM)s are coerced to sounds with a nominal 
duration of 30.

@label(sax-sec)
@codef{sax(@pragma(defn)@index(sax)@index(stk sax)@i(step), @i(breath-env))} @c{[sal]}@*
@altdef{@code[(sax @i(step) @i(breath-env))] @c{[lisp]}}@\A 
physical model of a sax from STK. The @i(step) parameter is a @code(FLONUM) 
that controls the tube length, and the @i(breath-env) controls the air pressure
and also determines the length of the resulting sound. The @i(breath-env) signal
should range from zero to one.

@codef{sax-freq(@pragma(defn)@index(sax)@index(sax-freq)@index(stk sax)@i(step), @i(breath-env), @i(freq-env))} @c{[sal]}@*
@altdef{@code[(sax-freq @i(step) @i(breath-env) @i(freq-env))] @c{[lisp]}}@\A variation of @code(sax)
that includes a variable frequency control, @i(freq-env), which specifies
frequency deviation in Hz. The duration of the resulting sound is the minimum
duration of @i(breath-env) and @i(freq-env). These parameters may be of type
@code(FLONUM) or @code(SOUND). @code(FLONUM)s are coerced into @code(SOUND)s
with a nominal duration arbitrarily set to 30.

@codef{sax-all(@pragma(defn)@index(sax)@index(sax-all)@index(stk sax)@i(step), @i(breath-env), @i(freq-env), @i(vibrato-freq), @i(vibrato-gain), 
@i(reed-stiffness), @i(noise), @i(blow-pos), @i(reed-table-offset))} @c{[sal]}@*
@altdef{@code[(sax-all @i(step) @i(breath-env) @i(freq-env) @i(vibrato-freq) @i(vibrato-gain) @i(reed-stiffness) @i(noise) @i(blow-pos) @i(reed-table-offset))] @c{[lisp]}}@\A variation of
 @code(sax-freq)
that includes controls @i(vibrato-freq) (a @code(FLONUM) for vibrato frequency in Hertz), 
@i(vibrato-gain) (a @code(FLONUM) for the amount of amplitude vibrato),
@i(reed-stiffness) (a @code(SOUND) controlling reed stiffness in the sax
model), @i(noise) (a @code(SOUND) controlling noise amplitude in the input
air pressure), @i(blow-pos) (a @code(SOUND) controlling the point of excitation
of the air column), and @i(reed-table-offset) (a @code(SOUND) controlling a
parameter of the reed model). The @i(vibrato-gain) is a number from zero to one, where zero
indicates no vibrato, and one indicates a plus/minus 50% change in breath
envelope values. Similarly, the @i(noise) parameter ranges from zero to one where 
zero means no noise and one means white noise with a peak amplitude of
plus/minus 40% of the @i(breath-env). The @i(reed-stiffness), @i(blow-pos), and
@i(reed-table-offset) parameters all vary from zero to one.
The duration of the resulting sound is the minimum duration of
@i(breath-env), @i(freq-env), @i(reed-stiffness), @i(noise), @i(breath-env),
 @i(blow-pos), and @i(reed-table-offset). As with
@code(sax-freq), these parameters may be either @code(FLONUM)s or 
@code(SOUND)s, and @code(FLONUM)s are coerced to sounds with a nominal 
duration of 30.

@label(flute-sec)
@codef{flute(@pragma(defn)@index(flute)@index(STK flute)@i(step), @i(breath-env))} @c{[sal]}@*
@altdef{@code[(flute @i(step) @i(breath-env))] @c{[lisp]}}@\A physical model of a flute from STK. 
The @i(step) parameter is a @code(FLONUM) that controls the tube 
length, and the @i(breath-env)
controls the air pressure and also determines the starting time and
length of the resulting sound. The @i(breath-env) signal should
 range from zero to one.

@codef{flute-freq(@pragma(defn)@index(flute-freq)@index(STK flute)@i(step), @i(breath-env), @i(freq-env))} @c{[sal]}@*
@altdef{@code[(flute-freq @i(step) @i(breath-env) @i(freq-env))] @c{[lisp]}}@\A variation of @code(flute)
 that includes a variable frequency control, @i(freq-env), which
 specifies frequency deviation in Hz. The duration of the 
resulting sound is the minimum duration of @i(breath-env) and 
@i(freq-env). These parameters may be of type @code(FLONUM) or
 @code(SOUND). @code(FLONUM)s are coerced into SOUNDs with a 
nominal duration arbitrary set to 30.

@codef{flute-all(@pragma(defn)@index(flute-all)@index(STK flute)@i(step), @i(breath-env), @i(freq-env), @i(vibrato-freq),
 @i(vibrato-gain), @i(jet-delay), @i(noise))} @c{[sal]}@*
@altdef{@code[(flute-all @i(step) @i(breath-env) @i(freq-env) @i(vibrato-freq) @i(vibrato-gain) @i(jet-delay) @i(noise))] @c{[lisp]}}@\A variation of 
@code(clarinet-freq) that includes controls @i(vibrato-freq) (a 
@code(FLONUM) for vibrato frequency in Hz), @i(vibrato-gain) (a
 @code(FLONUM) for the amount of amplitude vibrato), @i(jet-delay)
 (a @code(FLONUM) or @code(SOUND) controlling jet delay in the
 flute model), and 
noise (a @code(FLONUM) or @code(SOUND) controlling noise amplitude
in the input air pressure). The @i(vibrato-gain) is a number from zero
 to one where zero means no vibrato, and one indicates a plus/minus 
50% change in breath envelope values. Similarly, the @i(noise) parameter
 ranges from zero to one, where zero means no noise and one means white 
noise with a peak amplitude of
 plus/minus 40% of the @i(breath-env). The @i(jet-delay) is a ratio
 that controls a delay length from the flute model, and therefore it 
changes the pitch of the resulting sound. A value of 0.5 will maintain 
the pitch indicated by the step parameter. The duration of the 
resulting sound is the minimum duration of @i(breath-env), @i(freq-env), 
@i(jet-delay), and @i(noise). These parameters may be either 
@code(FLONUM)s or @code(SOUND)s, and @code(FLONUM)s are coerced
 to sounds with a nominal duration of 30. 
  
@label(bowed-sec)
@codef{bowed(@pragma(defn)@index(bowed)@index(STK bowed string)@i(step), @i(bowpress-env))} @c{[sal]}@*
@altdef{@code[(bowed @i(step) @i(bowpress-env))] @c{[lisp]}}@\A physical model of a bowed string
 instrument from STK. The @i(step) parameter is a @code(FLONUM)
 that controls the string length,
 and the @i(bowpress-env) controls the bow pressure and also 
determines the duration of the resulting sound. The @i(bowpress-env)
 signal should range from zero to one.

@codef{bowed-freq(@pragma(defn)@index(bowed-freq)@index(STK bowed-freq)@i(step), @i(bowpress-env), @i(freq-env))} @c{[sal]}@*
@altdef{@code[(bowed-freq @i(step) @i(bowpress-env) @i(freq-env))] @c{[lisp]}}@\A variation of @code(bowed)
 that includes a variable frequency control, @i(freq-env), which
 specifies frequency deviation in Hz. The duration of the resulting
 sound is the minimum duration of @i(bowpress-env) and @i(freq-env). 
These parameters may be of type @code(FLONUM) or @code(SOUND). 
@code(FLONUM)s are coerced into @code(SOUND)s
 with a nominal duration arbitrarily set to 30s.

@label(mandolin-sec)
@codef{mandolin(@pragma(defn)@index(mandolin)@index(STK mandolon)@i(step), @i(dur), &optional @i(detune))} @c{[sal]}@*
@altdef{@code[(mandolin @i(step) @i(dur) @i(detune))] @c{[lisp]}}@\A physical model of a
 plucked double-string instrument from STK. The @i(step) parameter
 is a @code(FLONUM) wich specifies the desired pitch, @i(dur)
 means the duration of the resulting sound and detune is a 
@code(FLONUM) that controls the relative detune of the two strings. 
A value of 1.0 means unison. The default value is 4.0.
Note: @i(body-size) (see @code(snd-mandolin) does not seem to
 work correctly, so a default value is always used
 by @code(mandolin).

@label(bandedwg-sec)
@codef{wg-uniform-bar(@pragma(defn)@index(wg-uniform-bar)@index(STK uniform bar)@i(step), @i(bowpress-env))} @c{[sal]}@*
@altdef{@code[(wg-uniform-bar @i(step) @i(bowpress-env))] @c{[lisp]}}

@codef{wg-tuned-bar(@pragma(defn)@index(wg-tuned-bar)@index(STK tuned bar)@i(step), @i(bowpress-env))} @c{[sal]}@*
@altdef{@code[(wg-tuned-bar @i(step) @i(bowpress-env))] @c{[lisp]}}

@codef{wg-glass-harm(@pragma(defn)@index(wg-glass-harm)@index(STK glass harmonica)@i(step), @i(bowpress-env))} @c{[sal]}@*
@altdef{@code[(wg-glass-harm @i(step) @i(bowpress-env))] @c{[lisp]}}

@codef{wg-tibetan-bowl(@pragma(defn)@index(wg-tibetan-bowl)@index(STK tibetan bowl)@i(step), @i(bowpress-env))} @c{[sal]}@*
@altdef{@code[(wg-tibetan-bowl @i(step) @i(bowpress-env))] @c{[lisp]}}@\These 
sounds are presets for a Banded Wave Guide Percussion instrument implemented in STK.
The parameter @i(step) is a @code(FLONUM)
 that controls the resultant pitch, and @i(bowpress-env) is a @code(SOUND) ranging 
from zero to one that controls a parameter of the model. In addition, 
@i(bowpress-env) determines the duration of the resulting sound.
(Note: The @i(bowpress-env) does not seems influence the timbral 
quality of the resulting sound).

@label(modalbar-sec)
@codef{modalbar(@pragma(defn)@index(modalbar)@index(STK modal bar)@i(preset), @i(step), @i(dur))} @c{[sal]}@*
@altdef{@code[(modalbar @i(preset) @i(step) @i(dur))] @c{[lisp]}}@\A physical model of a struck bar
 instrument implemented in STK. The parameter @i(preset) is one of the
symbols
@code(MARIMBA), @code(VIBRAPHONE), @code(AGOGO), @code(WOOD1), 
@code(RESO), @code(WOOD2), @code(BEATS), @code(TWO-FIXED), or
@code(CLUMP). The symbol must be quoted, e.g. for SAL syntax use
@code[quote(marimba)], and for Lisp syntax use @code('marimba).
The parameter @i(step) is a @code(FLONUM) that 
sets the pitch (in steps), and @i(dur) is the duration in seconds.

@label(sitar-sec)
@codef{sitar(@pragma(defn)@index(sitar)@index(STK sitar)@i(step), @i(dur))} @c{[sal]}@*
@altdef{@code[(sitar @i(step) @i(dur))] @c{[lisp]}}@\A sitar physical model implemented in STK. 
The parameter @i(step) is a @code(FLONUM) that sets the pitch,
 and @i(dur) is the duration.
@end(fndefs)

@paragraph(More Behaviors)
@begin(fndefs)
@label(clip-sec)
@codef{clip(@pragma(defn)@index(clip)@index(limit)@i(sound), @i(peak))} @c{[sal]}@*
@altdef{@code[(clip @i(sound) @i(peak))] @c{[lisp]}}@\Hard limit @i(sound) 
to the given @i(peak), a positive number. The samples of @i(sound) are constrained between an upper value
of @i(peak) and a lower value of @subtract()@i(peak). If @i(sound) is a number, @code(clip) will return @i(sound) limited by @i(peak).  If @i(sound) is a multichannel sound, @code(clip) returns a multichannel sound where each channel is clipped.  The result has the type, sample rate, starting time, etc. of @i(sound).

@label(s-abs-sec)
@codef{s-abs(@pragma(defn)@index(s-abs)@index(absolute value)@i(sound))} @c{[sal]}@*
@altdef{@code[(s-abs @i(sound))] @c{[lisp]}}@\A generalized absolute value function. If @i(sound) is a @code(SOUND), compute the absolute value of each sample. If @i(sound) is a number, just compute the absolute value. If @i(sound) is a multichannel sound, return a multichannel sound with @code(s-abs) applied to each element. The result has the type, sample rate, starting time, etc. of @i(sound).

@label(s-sqrt-sec)
@codef{s-sqrt(@pragma(defn)@index(s-sqrt)@index(square root)@i(sound))} @c{[sal]}@*
@altdef{@code[(s-sqrt @i(sound))] @c{[lisp]}}@\A generalized square root function. If @i(sound) is a @code(SOUND), compute the square root of each sample. If @i(sound) is a number, just compute the square root. If @i(sound) is a multichannel sound, return a multichannel sound with @code(s-sqrt) applied to each element. The result has the type, sample rate, starting time, etc. of @i(sound). In taking square roots, if an input sample is less than zero, the corresponding output sample is zero. This is done because the square root of a negative number is undefined.

@label(s-exp-sec)
@codef{s-exp(@pragma(defn)@index(s-exp)@index(exponential)@i(sound))} @c{[sal]}@*
@altdef{@code[(s-exp @i(sound))] @c{[lisp]}}@\A generalized exponential function.  If @i(sound) is a @code(SOUND), compute @i(e)@+(@i(x)) for each sample @i(x).  If @i(sound) is a number @i(x), just compute @i(e)@+(@i(x)).  If @i(sound) is a multichannel sound, return a multichannel sound with @code(s-exp) applied to each element.  The result has the type, sample rate, starting time, etc. of @i(sound).

@label(s-log-sec)
@codef{s-log(@pragma(defn)@index(s-log)@index(logorithm)@index(natural log)@i(sound))} @c{[sal]}@*
@altdef{@code[(s-log @i(sound))] @c{[lisp]}}@\A generalized natural log function.  If @i(sound) is a @code(SOUND), compute @i(ln)(@i(x)) for each sample @i(x).  If @i(sound) is a number @i(x), just compute @i(ln)(@i(x)).  If @i(sound) is a multichannel sound, return a multichannel sound with @code(s-log) applied to each element.  The result has the type, sample rate, starting time, etc. of @i(sound).  Note that the @i(ln) of 0 is undefined (some implementations return negative infinity), so use this function with care.

@label(s-max-sec)
@codef{s-max(@pragma(defn)@index(s-max)@index(maximum)@i(sound1), @i(sound2))} @c{[sal]}@*
@altdef{@code[(s-max @i(sound1) @i(sound2))] @c{[lisp]}}@\Compute the maximum of two functions, @i(sound1) and @i(sound2). This function also accepts numbers and multichannel sounds and returns the corresponding data type. The start time of the result is the maximum of the start times of @i(sound1) and @i(sound2). The logical stop time and physical stop time of the result is the minimum of the logical stop and physical stop times respectively of @i(sound1) and @i(sound2). Note, therefore, that the result value is zero except within the bounds of @i(both) input sounds.

@codef{s-min(@pragma(defn)@index(s-min)@index(minimum)@i(sound1), @i(sound2))} @c{[sal]}@*
@altdef{@code[(s-min @i(sound1) @i(sound2))] @c{[lisp]}}@\Compute the minimum of two functions, @i(sound1) and @i(sound2). This function also accepts numbers and multichannel sounds and returns the corresponding data type. The start time of the result is the maximum of the start times of @i(sound1) and @i(sound2). The logical stop time and physical stop time of the result is the minimum of the logical stop and physical stop times respectively of @i(sound1) and @i(sound2). Note, therefore, that the result value is zero except within the bounds of @i(both) input sounds.

@codef{osc-note(@pragma(defn)@index(osc-note)@i(pitch) [, @i(duration), @i(env), @i(loud), 
@i(table)])} @c{[sal]}@*
@altdef{@code{(osc-note @i(pitch) [@i(duration) @i(env) @i(loud) @i(table)])} @c{[lisp]}}@\Same as @code(osc), but @code(osc-note)
multiplies the result by @i(env).  The @i(env) may be a sound, 
or a list supplying (@i(t@-[1]) @i(t@-[2]) 
@i(t@-[4]) @i(l@-[1]) @i(l@-[2]) @i(l@-[3])).  The result has a sample rate of @code(*sound-srate*).

@label(quantize-sec)
@codef{quantize(@pragma(defn)@index(quantize)@i(sound), @i(steps))} @c{[sal]}@*
@altdef{@code[(quantize @i(sound) @i(steps))] @c{[lisp]}}@\Quantizes @i(sound) as follows: @i(sound) is multiplied by @i(steps) and rounded to the nearest integer. The result is then divided by @i(steps). For example, if @i(steps) is 127, then a signal that ranges from -1 to +1 will be quantized to 255 levels (127 less than zero, 127 greater than zero, and zero itself). This would match the quantization Nyquist performs when writing a signal to an 8-bit audio file. The @i(sound) may be multi-channel.

@codef{ramp(@pragma(defn)@index(ramp)[@i(duration)])} @c{[sal]}@*
@altdef{@code{(ramp [@i(duration)])} @c{[lisp]}}@\Returns a
linear ramp from 0 to 1
over @i(duration) (default is 1).  The function actually reaches 1 at
@i(duration), and therefore has one extra sample, making the total duration
be @i(duration) + 1/@code(*Control-srate*).  See Figure @ref(ramp-fig) for
more detail.  Ramp is unaffected by the @code(sustain) transformation.  The
effect of time warping is to warp the starting and ending times only.  The
ramp itself is unwarped (linear).  The sample rate is @code(*control-srate*).

@label(rms-sec)
@codef{rms(@pragma(defn)@index(rms)@i(sound) [, @i(rate), @i(window-size)])} @c{[sal]}@*
@altdef{@code{(rms @i(sound) [@i(rate) @i(window-size)])} @c{[lisp]}}@\Computes the RMS of @i(sound) using a square window of size @i(window-size). The result has a sample rate of @i(rate). The default value of @i(rate) is 100 Hz, and the default window size is 1/rate seconds (converted to samples). The @i(rate) is a @code(FLONUM) and @i(window-size) is a @code(FIXNUM).
@end(fndefs)

@begin(figure)
@center(@graphic((height = 2.37 in, width = 4.5 in, magnify = 0.75,
		postscript = "rampfig.ps"))
@html(<img src="fig6.gif"><br><br>)
@fillcaption[Ramps generated by @code(pwl) and @code(ramp) functions.  The
@code(pwl) version ramps toward the breakpoint (1, 1), but in order to ramp
back to zero at breakpoint (1, 0), the function never reaches an amplitude
of 1.  If used at the beginning of a @code(seq) construct, the next sound
will begin at time 1.  The @code(ramp) version actually reaches breakpoint
(1, 1); notice that it is one sample longer than the @code(pwl) version.  If
used in a sequence, the next sound after @code(ramp) would start at time 1 +
@i(P), where @i(P) is the sample period.]
@tag(ramp-fig)
@end(figure)

@begin(fndefs)
@label(recip-sec)
@codef{recip(@pragma(defn)@index(recip)@index(reciprocal)@index(division)@i(sound))} @c{[sal]}@*
@altdef{@code[(recip @i(sound))] @c{[lisp]}}@\A generalized reciprocal function.  
If @i(sound) is a @code(SOUND), compute 1/@i(x) for each sample @i(x).  If @i(sound) is a number @i(x), just compute 1/@i(x).  If @i(sound) is a multichannel sound, return a multichannel sound with @code(recip) applied to each element.  The result has the type, sample rate, starting time, etc. of @i(sound).  Note that the reciprocal of 0 is undefined (some implementations return  infinity), so use this function with care on sounds.  Division of sounds is accomplished by multiplying by the reciprocal.  Again, be careful not to divide by zero.

@codef{s-rest(@index(rest)@pragma(defn)@index(s-rest)[@i(duration)])} @c{[sal]}@*
@altdef{@code{(s-rest [@i(duration)])} @c{[lisp]}}@\Create silence (zero samples)
for the given 
@i(duration) at the sample rate @code(*sound-srate*).  
Default duration is 1.0 sec, and the sound is transformed in time according
to @code[*warp*].  @p(Note:) @code(rest) is a Lisp function that is equivalent to @code(cdr).  Be careful to use @code(s-rest) when you need a sound!

@label(noise-sec)
@codef{noise(@pragma(defn)@index(noise)[@i(duration)])} @c{[sal]}@*
@altdef{@code[(noise @i(duration))] @c{[lisp]}}@\Generate noise with the given 
@i(duration).  Duration (default is 1.0) 
is transformed according to @code[*warp*].  The
sample rate is @code(*sound-srate*) and the amplitude is +/- @code(*loud*).

@label(yin-sec)
@codef{yin(@pragma(defn)@index(yin)@index(pitch detection)@index(fundamenal frequency
estimation)@index(estimate frequency)@index(frequency analysis)@index(period
estimation)@i(sound), @i(minstep), @i(maxstep), @i(stepsize))} @c{[sal]}@*
@altdef{@code[(yin @i(sound) @i(minstep) @i(maxstep) @i(stepsize))] @c{[lisp]}}@\Fundamental 
frequency estimation (pitch detection. Use the YIN algorithm to estimate
the fundamental frequency of @i(sound), which must be a @code(SOUND). 
The @i(minstep), a @code(FLONUM), is the minimum frequency considered (in steps), 
@i(maxstep), a @code(FLONUM), is the maximum frequency considered (in steps), and 
@i(stepsize), a @code(FIXNUM), is the desired hop size.  The result is 
a ``stereo'' signal,
i.e. an array of two @code(SOUND)s, both at the same sample rate, which is 
approximately the sample rate of @i(sound) divided by @i(stepsize). 
The first @code(SOUND) consists of frequency estimates (in units of
steps, i.e. middle C = 60). The second sound consists
of values that measure the confidence or reliability of the frequency estimate.
A small value (less than 0.1) indicates fairly high confidence. A larger value
indicates lower confidence. This number can also be thought of as a ratio of 
non-periodic power to periodic power. When the number is low, it means the signal
is highly periodic at that point in time, so the period estimate will be 
reliable.
Hint #1: See 
Alain de Cheveigne and Hideki Kawahara's article "YIN, a Fundamental Frequency
Estimator for Speech and Music" in the Journal of the 
Acoustic Society of America, April 2002 for details on the yin algorithm.  
Hint #2: Typically, the @i(stepsize) should be at least the expected number
of samples in one period so that the 
fundamental frequency estimates are calculated at a rate far below
the sample rate of the signal. Frequency does not change rapidly and
the yin algorithm is fairly slow. To optimize speed, 
you may want to use less than 44.1 kHz sample rates for input sounds. Yin
uses interpolation to achieve potentially fractional-sample-accurate estimates, 
so higher sample rates do not necessarily help the algorithm and definitely
slow it down. The computation time is O(@i(n)@+(2)) per estimate, 
where @i(n) is the number
of samples in the longest period considered. Therefore, each increase
of @i(minstep) by 12 (an octave) gives you a factor of 4 speedup, and 
each decrease of the sample rate of @i(sound) by a factor of
two gives you another factor of 4 speedup. Finally, the number of estimates is
inversely proportional to @i(stepsize).
Hint #3: Use @code(snd-srate) (see Section @ref(snd-srate-sec)) to get
the exact sample rate of the result, which will be the sample rate of
 @i(sound) divided by @i(stepsize). 
E.g. @code{(snd-srate (aref yin-output 0))},
where @code(yin-output) is a result returned by @code(yin), will be the 
sample rate of the estimates. 

@end(fndefs)

@section(Transformations)@index(Transformations)
@label(transformations-sec)
These functions change the environment that is seen by other high-level
functions.  Note that these changes are usually relative to the
current environment.  There are also ``absolute'' versions of each
transformation function, with the exception of @code(seq),
 @code(seqrep), @code(sim), and @code(simrep).  The
``absolute'' versions (starting or ending with ``abs'') do not look at the
current environment, but rather set an environment variable to a specific value.
In this way, sections of code can be insulated from external
transformations.

@begin(fndefs)
@codef{abs-env(@pragma(defn)@index(abs-env)@i(beh))} @c{[sal]}@*
@altdef{@code[(abs-env @i(beh))] @c{[lisp]}}@\Compute @i(beh) in the default environment.  
This is useful for computing waveform tables and signals that are 
``outside'' of
time.  For example, @code[(at 10.0 (abs-env (my-beh)))] is equivalent to
@code[(abs-env (my-beh))] because @code(abs-env) forces the default environment. Or in SAL, we would say @code[abs-env(my-beh()) @@ 10] is equivalent to @code[abs-env(my-beh())].

@codef{at(@pragma(defn)@index(at)@i(time), @i(beh))} @c{[sal]}@*
@altdef{@code[(at @i(time) @i(beh))] @c{[lisp]}}@\Evaluate @i(beh) with
@code(*warp*@index(*warp*)) shifted by @i(time). In SAL, you can use the infix
operator @code(@@) as in @code[@i(beh) @@ @i(time)]. To discover how the
environment is shifting time, use @code[local-to-global(@i(time))]. Most
commonly, you call @code[local-to-global(0)] to find when a sound created
in the current environment will start, expressed in absolute (global) terms.  
This can be regarded as the ``current time.''

@codef{at-abs(@pragma(defn)@index(at-abs)@i(time), @i(beh))} @c{[sal]}@*
@altdef{@code[(at-abs @i(time) @i(beh))] @c{[lisp]}}@\Evaluate @i(beh) with
@code(*warp*@index(*warp*)) shifted so that local time 0 maps to @i(time). In SAL, you can use the infix operator @code[@@@@] as in @code[@i(beh) @@@@ @i(time)].

@label(continuous-control-warp)
@codef{continuous-control-warp(@pragma(defn)@index(continuous-control-warp)@i(beh))} @c{[sal]}@*
@altdef{@code[(continuous-control-warp @i(beh))] @c{[lisp]}}@\Applies the current warp environment to the signal returned by @i(beh). The result has the default control sample rate @code(*control-srate*). Linear interpolation is currently used. Implementation: @i(beh) is first evaluated without any shifting, stretching, or warping. The result is functionally composed with the inverse of the environment's warp function.

@label(continuous-sound-warp)
@codef{continuous-sound-warp(@pragma(defn)@index(continuous-sound-warp)@i(beh))} @c{[sal]}@*
@altdef{@code[(continuous-sound-warp @i(beh))] @c{[lisp]}}@\Applies the current warp environment to the signal returned by @i(beh). The result has the default sound sample rate @code(*sound-srate*). Linear interpolation is currently used. See @code(continuous-control-warp) for implementation notes.

@label(control-srate-abs-sec)
@codef{control-srate-abs(@pragma(defn)@index(control-srate-abs)@i(srate), 
@i(beh))} @c{[sal]}@*
@altdef{@code[(control-srate-abs @i(srate) @i(beh))] @c{[lisp]}}@\Evaluate @i(beh) with @code(*control-srate*@index(*control-srate*))
set to sample rate @i(srate).  @p(Note:) there is no ``relative'' version of 
this function.

@codef{extract(@pragma(defn)@index(extract)@i(start), @i(stop), @i(beh))} @c{[sal]}@*
@altdef{@code[(extract @i(start) @i(stop) @i(beh))] @c{[lisp]}}@\Returns a sound 
which is the portion of
@i(beh) between @i(start) and @i(stop).  Note that this is done 
relative to the current @code(*warp*).  The result is shifted
to start according to @code(*warp*), so normally the result will start without a delay of @i(start).

@codef{extract-abs(@pragma(defn)@index(extract-abs)@i(start), @i(stop), @i(beh))} @c{[sal]}@*
@altdef{@code[(extract-abs @i(start) @i(stop) @i(beh))] @c{[lisp]}}@\Returns a sound which
is the portion of
@i(beh) between @i(start) and @i(stop), independent of the 
current @code(*warp*).  The result is shifted
to start according to @code(*warp*).

@codef{loud(@pragma(defn)@index(loud)@i(volume), @i(beh))} @c{[sal]}@*
@altdef{@code[(loud @i(volume) @i(beh))] @c{[lisp]}}@\Evaluates @i(beh) with @code(*loud*)
incremented by @i(volume). (Recall that @code(*loud*) is in decibels, so increment is the proper operation.)

@codef{loud-abs(@pragma(defn)@index(loud-abs)@i(volume), @i(beh))} @c{[sal]}@*
@altdef{@code[(loud-abs @i(volume) @i(beh))] @c{[lisp]}}@\Evaluates @i(beh) with @code(*loud*)
set to @i(volume).

@label(sound-srate-abs-sec)
@codef{sound-srate-abs(@pragma(defn)@index(sound-srate-abs)@i(srate), @i(beh))} @c{[sal]}@*
@altdef{@code[(sound-srate-abs @i(srate) @i(beh))] @c{[lisp]}}@\Evaluate @i(beh) with @code(*sound-srate*@index(*sound-srate*)) set to sample rate @i(srate).  @p(Note:) there is no ``relative'' version of this function.  

@codef{stretch(@pragma(defn)@index(stretch)@i(factor), @i(beh))} @c{[sal]}@*
@altdef{@code[(stretch @i(factor) @i(beh))] @c{[lisp]}}@\Evaluates @i(beh) with
@code(*warp*) scaled by @i(factor).  The effect is to ``stretch'' the result
of @i(beh) (under the current environment) by @i(factor).  See Chapter
@ref(warp-chap) for more information. Use @code[get-duration(@i(dur))] to 
get the nominal actual duration of a behavior that locally has a duration
of @i(dur). Here, ``nominal'' means what would be expected if the behavior
obeys the shift, stretch, and warp components of the environment. (Any 
behavior is free to deviate from the nominal timing. For example, a percussion
sound might have a fixed duration independent of the stretch factor.) Also,
``actual'' means global or absolute time, and ``locally'' means within the
environment where @code[get-duration] is called. @code[get-duration] works
by mapping the current time (local time 0) using @code[local-to-global] to
obtain an actual start time, and mapping @i(dur) to obtain an actual end time.
The difference is returned.

@codef{stretch-abs(@pragma(defn)@index(stretch-abs)@i(factor), @i(beh))} @c{[sal]}@*
@altdef{@code[(stretch-abs @i(factor) @i(beh))] @c{[lisp]}}@\Evaluates @i(beh) with @code(*warp*) set to a linear time transformation where each unit of logical time maps to @i(factor) units of real time.  The effect is to stretch the nominal behavior of @i(beh) (under the default global environment) by @i(factor).  See Chapter @ref(warp-chap) for more information.

@codef{sustain(@pragma(defn)@index(sustain)@index(legato)@index(overlap)@index(stacatto)@i(factor), @i(beh))} @c{[sal]}@*
@altdef{@code[(sustain @i(factor) @i(beh))] @c{[lisp]}}@\Evaluates @i(beh) with @code(*sustain*) scaled by @i(factor). The effect is to ``stretch'' the result of @i(beh) (under the current environment) by @i(factor); however, the logical stop times are not stretched. Therefore, the overall duration of a sequence is not changed, and sounds will tend to overlap if @code(*sustain*) is greater than one (legato) and be separated by silence if @code(*sustain*) is less than one.

@codef{sustain-abs(@pragma(defn)@index(sustain-abs)@i(factor), @i(beh))} @c{[sal]}@*
@altdef{@code[(sustain-abs @i(factor) @i(beh))] @c{[lisp]}}@\Evaluates @i(beh) with @code(*sustain*) set to @i(factor). (See @code(sustain), above.)

@codef{transpose(@pragma(defn)@index(transpose)@i(amount), @i(beh))} @c{[sal]}@*
@altdef{@code[(transpose @i(amount) @i(beh))] @c{[lisp]}}@\Evaluates @i(beh) with 
@code(*transpose*) shifted by @i(amount).  The effect is relative transposition by @i(amount) semitones.

@codef{transpose-abs(@pragma(defn)@index(transpose-abs)@i(amount), @i(beh))} @c{[sal]}@*
@altdef{@code[(transpose-abs @i(amount) @i(beh))] @c{[lisp]}}@\Evaluates @i(beh) with 
@code(*transpose*) set to @i(amount).  The effect is the transposition of the nominal pitches in @i(beh) (under the default global environment) by @i(amount).

@codef{warp(@pragma(defn)@index(warp)@i(fn), @i(beh))} @c{[sal]}@*
@altdef{@code[(warp @i(fn) @i(beh))] @c{[lisp]}}@\Evaluates @i(beh) with @code(*warp*) modified by @i(fn).  The idea is that @i(beh) and @i(fn) are written in the same time system, and @i(fn) warps that time system to local time.  The current environment already contains a mapping from local time to global (real) time.  The value of @code(*warp*) in effect when @i(beh) is evaluated is the functional composition of the initial @code(*warp*) with @i(fn).

@codef{warp-abs(@pragma(defn)@index(warp-abs)@i(fn), @i(beh))} @c{[sal]}@*
@altdef{@code[(warp-abs @i(fn) @i(beh))] @c{[lisp]}}@\Evaluates @i(beh) with @code(*warp*) set to @i(fn).  In other words, the current @code(*warp*) is ignored and not composed with @i(fn) to form the new @code(*warp*).
@end(fndefs)

@section(Combination and Time Structure)@index(Combination)@index(Time Structure)
These behaviors combine component behaviors into structures, including
sequences (melodies), simultaneous sounds (chords), and structures based
on iteration.

@begin(fndefs)
@label(seq-sec)
 @codef{seq(@pragma(defn)@index(seq)@i(beh@-[1]) [, @i(beh@-[2]), @r(...)])} @c{[sal]}@*
@altdef{@code{(seq @i(beh@-[1]) [@i(beh@-[2]) @r(...)])} @c{[lisp]}}@\Evaluates the first behavior
@i(beh@-[1]) according to @code(*time*) and each successive behavior at the
@code(logical-stop) time of the previous one.  The results are summed to form a
sound whose @code(logical-stop) is
the @code(logical-stop) of the last behavior in the sequence.  Each behavior
can result in a multichannel sound, in which case, the logical stop time is
considered to be the maximum logical stop time of any channel.  The number
of channels in the result is the number of channels of the first behavior.
If other behaviors return fewer channels, new channels are created containing
constant zero signals until the required number of channels is obtained.  If
other behaviors return a simple sound rather than multichannel sounds, the
sound is automatically assigned to the first channel of a multichannel sound
that is then filled out with zero signals.  If another behavior returns more
channels than the first behavior, the error is reported and the computation
is stopped.  Sample rates are converted up or down to match the sample rate of the first sound in a sequence.

@codef{seqrep(@pragma(defn)@index(seqrep)@i(var), @i(limit), @i(beh))} @c{[sal]}@*
@altdef{@code[(seqrep @i(var) @i(limit) @i(beh))] @c{[lisp]}}@\Iteratively 
evaluates @i(beh) with the atom
@i(var) set with values from 0 to @i(limit)-1, inclusive.  These sounds
are placed sequentially in time as if by @code(seq). The symbol @i(var) is
a @i(read-only) local variable to @i(beh). Assignments are not restricted
or detected, but may cause a run-time error or crash. In LISP, the syntax is
 @code[(seqrep (@i(var) @i(limit)) @i(beh))].

@label(sim-sec)
@codef{sim(@pragma(defn)@index(sim)[@i(beh@-[1]), @i(beh@-[2]), @r(...)])} @c{[sal]}@*
@altdef{@code{(sim [@i(beh@-[1]) @i(beh@-[2]) @r(...)])} @c{[lisp]}}@\Returns a sound which is the 
sum of the given behaviors evaluated with current value of @code(*warp*).
If behaviors return multiple channel sounds, the corresponding channels are
added.  If the number of channels does not match, the result has the
maximum.  For example, if a two-channel sound [L, R] is added to a four-channel
sound [C1, C2, C3, C4], the result is [L + C1, R + C2, C3, C4].  Arguments to @code(sim) may also be numbers.  If all arguments are numbers, @code(sim) is equivalent (although slower than) the @code(+) function.  If a number is added to a sound, @code(snd-offset) is used to add the number to each sample of the sound.  The result of adding a number to two or more sounds with different durations is not defined.  Use @code(const) to coerce a number to a sound of a specified duration.  An important limitation of @code(sim) is that it cannot handle hundreds of behaviors due to a stack size limitation in XLISP.  To compute hundreds of sounds (e.g. notes) at specified times, see @code(timed-seq), below.
See also @code(sum) below.

@codef{simrep(@pragma(defn)@index(simrep)@i(var), @i(limit), @i(beh))} @c{[sal]}@*
@altdef{@code[(simrep @i(var) @i(limit) @i(beh))] @c{[lisp]}}@\Iteratively 
evaluates @i(beh) with the atom
@i(var) set with values from 0 to @i(limit)-1, inclusive.  These sounds
are then placed simultaneously in time as if by @code(sim).
In LISP, the syntax is
 @code[(seqrep (@i(var) @i(limit)) @i(beh))].

@label(trigger-sec)
@codef[trigger(@pragma(defn)@index(trigger)@i(s), @i(beh))] @c{[sal]}@*
@altdef{@code[(trigger @i(s) @i(beh))] @c{[lisp]}}@\Returns a sound which is the
sum of instances of the behavior @i(beh). One instance is created each time
@code(SOUND) @i(s) makes a transition from less than or equal to zero to
greater than zero. (If the first sample of @i(s) is greater than zero, an
instance is created immediately.) The sample rate of @i(s) and all behaviors
must be the same, and the behaviors must be (monophonic) @code(SOUND)s.
This function is particularly designed to allow behaviors to be invoked
in real time by making @i(s) a function of a Nyquist slider, which can be
controlled by a graphical interface or by OSC messages. See @code(snd-slider)
in Section @ref(snd-slider-sec).

@codef[set-logical-stop(@pragma(defn)@index(set-logical-stop)@i(beh), @i(time))] @c{[sal]}@*
@altdef{@code[(set-logical-stop @i(beh) @i(time))] @c{[lisp]}}@\Returns a sound with @i(time) as 
the logical stop time.

@codef{sum(@pragma(defn)@index(sum)@index(mix)@i(a) [, @i(b), @r(...)])} @c{[sal]}@*
@altdef{@code{(sum @i(a) [@i(b) @r(...)])} @c{[lisp]}}@\Returns the sum of @i(a), @i(b), ..., allowing mixed addition of sounds, multichannel sounds and numbers.  Identical to @i(sim). In SAL, use the infix ``+'' operator.

@codef{mult(@pragma(defn)@index(mult)@index(product)@index(multiply signals)@i(a) [, @i(b), @r(...)])} @c{[sal]}@*
@altdef{@code{(mult @i(a) [@i(b) @r(...)])} @c{[lisp]}}@\Returns the product of @i(a), @i(b), ..., allowing mixed multiplication of sounds, multichannel sounds and numbers.

@codef{diff(@pragma(defn)@index(diff)@index(difference of sounds)@i(a), @i(b))} @c{[sal]}@*
@altdef{@code[(diff @i(a) @i(b))] @c{[lisp]}}@\Returns the difference between @i(a) and @i(b). This function is defined as @code[(sum a (prod -1 b))].

@label(timed-seq-sec)
@codef{timed-seq(@pragma(defn)@index(timed-seq)@index(score)@index(note list)@i(score))} @c{[sal]}@*
@altdef{@code[(timed-seq @i(score))] @c{[lisp]}}@\Computes sounds from a note list or ``score.'' The @i(score) 
is of the form: @code[`((@i(time1) @i(stretch1) @i(beh1)) (@i(time2) 
@i(stretch2) @i(beh2)) @r(...))], where @i(timeN) is the starting time, 
@i(stretchN) is the stretch factor, and @i(behN) is the behavior. Note 
that @i(score) is normally a @i(quoted list)! The times must be in 
increasing order, and each @i(behN) is evaluated using lisp's @code(eval), 
so the @i(behN) behaviors cannot refer to local parameters or local 
variables. The advantage of this form over @code(seq) is that the 
behaviors are evaluated one-at-a-time which can take much less stack 
space and overall memory. One special ``behavior'' expression is
interpreted directly by @code(timed-seq): @code[(SCORE-BEGIN-END)] 
is ignored, not evaluated as a function. Normally, this special
behavior is placed at time 0 and has two parameters: the score
start time and the score end time. These are used in Xmusic
functions. If the behavior has a @code(:pitch) keyword parameter
which is a list, the list represents a chord, and the expression is 
replaced by a set of behaviors, one for each note in the chord. 
It follows that if @code(:pitch) is @code(nil), the behavior 
represents a rest and is ignored.

@end(fndefs)


@section(Sound File Input and Output)
@index(sound file I/O)
@begin(fndefs)
@label(play-sec)
@codef[play @pragma(defn)@index(play)@i(sound)] @c{[sal]}@*
@altdef{@code[(play @i(sound))] @c{[lisp]}}@\Play the sound 
through the DAC.  
Note that @code(play) is a command in SAL. In XLISP, it is a function,
so the syntax is @code[(play @i(sound))], and in SAL, you can call the
XLISP function as @code[#play(@i(sound))].
The @code(play) command or function 
writes a file and plays it.  The details of this
are system-dependent, but @code(play) is defined in the file
@code(system.lsp).  The variable @code(*default-sf-dir*)@index(sound file directory default)@index(directory, default sound file)@index(default sound file directory)@index(temporary sound files directory)
@index(*default-sf-dir*) names a directory into which to save a sound file. Be careful not to call @code(play) or @code(sound-play) within a function and then
invoke that function from another @code(play) command.

By default, Nyquist will try to normalize sounds using the method named by 
@code(*autonorm-type*), which is @code('lookahead) by default. 
The @i(lookahead) method precomputes and buffers @code(*autonorm-max-samples*)
samples, finds the peak value, and normalizes accordingly. The 
@code('previous) method bases the normalization of the current sound on the peak value of the (entire) previous sound. This might be good if you are working with long sounds that start rather softly. See Section @ref(peak-ex-sec) for more details.

If you want precise control over output levels, you should turn this feature off by typing (using SAL syntax):
@begin(example)
autonorm-off()@index(autonorm-off)
@end(example)
Reenable the automatic normalization feature by typing:
@begin(example)
autonorm-on()@index(autonorm-on)
@end(example)
Play normally produces real-time output.  The default is to send audio data to the DAC as it is computed in addition to saving samples in a file.  If computation is slower than real-time, output will be choppy, but since the samples end up in a file, you can type @code[(r)] to replay the stored sound. Real-time playback can be disabled by (using SAL syntax):
@begin(example)
sound-off()@index(sound-off)
@end(example)
and reenabled by:
@begin(example)
sound-on()@index(sound-on)
@end(example)
Disabling real-time playback has no effect on @code[(play-file)] or @code[(r)].

While sounds are playing, typing control-A@index(control-A) to Nyquist will push the estimated
elapsed@index(elapsed audio time) audio time onto the head of the list 
stored in @code(*audio-markers*).
@index(*audio-markers*)@index(audio markers)@index(markers, audio)
Because samples are computed in blocks and because there is latency 
between sample computation and sample playback, the elapsed time may not
be too accurate, and the computed elapsed time may not advance after all
samples have been computed but the sound is still playing.

@codef[play-file(@pragma(defn)@index(play-file)@i(filename))] @c{[sal]}@*
@altdef{@code[(play-file @i(filename))] @c{[lisp]}}@\Play the contents of a sound file named by @i(filename). The @code(s-read) function is used to read the file, and unless 
@i(filename) specifies an absolute path or starts with ``.'', it will be read from 
@code(*default-sf-dir*).

@codef[autonorm-on(@pragma(defn)@index(autonorm-on))] @c{[sal]}@*
@altdef{@code[(autonorm-on)] @c{[lisp]}}@\Enable automatic adjustment of a scale factor applied to sounds computed using the @code(play) command.

@codef[autonorm-off(@pragma(defn)@index(autonorm-off))] @c{[sal]}@*
@altdef{@code[(autonorm-off)] @c{[lisp]}}@\Disable automatic adjustment of a scale factor applied to sounds computed using the @code(play) command.

@codef[sound-on(@pragma(defn)@index(sound-on))] @c{[sal]}@*
@altdef{@code[(sound-on)] @c{[lisp]}}@\Enable real-time audio output when sound is computed by the the @code(play) command.

@codef[sound-off(@pragma(defn)@index(sound-off))] @c{[sal]}@*
@altdef{@code[(sound-off)] @c{[lisp]}}@\Disable real-time audio output when sound is computed by the the @code(play) command.

@label(s-save-sec)
@codef{s-save(@pragma(defn)@index(s-save)@index(save samples to file)@index(write samples to file)@index(output samples to file)@i(expression), @i(maxlen),
@i(filename), format: @i(format), mode: @i(mode), bits: @i(bits), swap: @i(flag), play: @i(play))} @c{[sal]}@*
@altdef{@code{(s-save @i(expression) @i(maxlen) @i(filename) :format @i(format) :mode @i(mode) :bits @i(bits) :swap @i(flag) :play @i(play))} @c{[lisp]}}@\@label(s-save)Evaluates the @i(expression), which should result in a sound
or an array of sounds, and writes the result to the given @i(filename).  A
@code(FLONUM) is returned giving the maximum absolute value of all samples
written. (This is useful for normalizing sounds and detecting sample
overflow.) If @i(play) is not @code(NIL), the sound will be output
through the computer's audio output system. (@i(play:) @c{[sal]}
or @i(:play) @c{[lisp]} is not implemented on all systems; if it is implemented, and @i(filename) is @code(NIL), then this will play the file without also writing a file.)
The latency (length of audio buffering) used to play the sound is 0.3s by default, but see @code(snd-set-latency).
If
a multichannel sound (array) is written, the channels are up-sampled to the
highest rate in any channel so that all channels have the same sample rate.
The maximum number of samples written per channel is given by @i(maxlen),
which allows writing the initial part of a very long or infinite sound. A
header is written according to @i(format), samples are encoded according to
@i(mode), using @i(bits) bits/sample, and bytes are swapped if @i(flag) is not NIL.  Defaults for these are
@code(*default-sf-format*), @code(*default-sf-mode*), and
@code(*default-sf-bits*). The default for @i(flag) is NIL.
The @i(bits) parameter may be 8, 16, or 32.  The values for the @i(format) and @i(mode) options are described below:
@end(fndefs)
@b(Format)
@begin(description, leftmargin +2 in, indent -2 in)
@code(snd-head-none)@\The format is unknown and should be determined
by reading the file.

@code(snd-head-raw)@\A raw format file has no header. 

@code(snd-head-AIFF)@\AIFF format header.

@code(snd-head-IRCAM)@\IRCAM format header.

@code(snd-head-NeXT)@\1024-byte NeXT/SUN format header followed by IRCAM
header ala CMIX.  Note that the NeXT/SUN format has a header-length field,
so it really is legal to have a large header, even though the normal minimal
header is only 24 bytes.  The additional space leaves room for maximum
amplitudes, which can be used for normalizing floating-point soundfiles, and
for other data.  Nyquist follows the CMIX convention of placing an IRCAM
format header immediately after the NeXT-style header.

@code(snd-head-Wave)@\Microsoft Wave format header.

@code(snd-head-*)@\See sndfnint.lsp for more formats.
@end(description)

@b(Mode)
@begin(description, leftmargin +2 in, indent -2 in)
@code(snd-mode-adpcm)@\ADPCM mode (not supported).

@code(snd-mode-pcm)@\signed binary PCM mode.

@code(snd-mode-ulaw)@\8-bit U-Law mode.

@code(snd-mode-alaw)@\8-bit A-Law mode (not supported).

@code(snd-mode-float)@\32-bit floating point mode.

@code(snd-mode-upcm)@\unsigned binary PCM mode.

@code(snd-mode-*)@\See sndfnint.lsp for more modes.
@end(description)

The defaults for format, mode, and bits are as follows:
@begin(description, leftmargin +2 in, indent -2 in)
NeXT and Sun machines:@\@code(snd-head-NeXT), @code(snd-mode-pcm),
@code(16)

SGI and Macintosh machines:@\@code(snd-head-AIFF), @code(snd-mode-pcm), @code(16)

@end(description)

@begin(fndefs)
@label(s-read-sec)
@codef{s-read(@pragma(defn)@index(s-read)@index(read samples from file)@i(filename), time-offset: @i(offset), srate: @i(sr), dur: @i(dur), nchans: @i(chans),
 format: @i(format), mode: @i(mode), bits: @i(n), swap: @i(flag))} @c{[sal]}@*
@altdef{@code{(s-read @i(filename) :time-offset @i(offset) :srate @i(sr)
 :dur @i(dur) :nchans @i(chans) :format @i(format) :mode @i(mode) :bits @i(n)
 :swap @i(flag))} @c{[lisp]}}@\Reads a sound from
 @i(filename). The global @code(*default-sf-dir*) applies. If a header is
detected, the header is used to determine the format
of the file, and header information overrides format information provided by
keywords (except for @code(time-offset:) and @code(dur:)). 
@begin(example)
s-read("mysound.snd", srate: 44100)
@end(example)
specifies a sample rate of 44100 hz, but if the file has a header specifying 22050 hz, the resulting sample rate will be 22050.  The parameters are:
@begin(itemize)
 @i(offset) @itemsep the amount of time (in seconds) to skip from
the beginning of the file.  The default is 0.0.

@i(sr) @itemsep the sample rate of the samples in the file.  Default is
@code(*default-sf-srate*) @index(*default-sf-srate*), which is normally 44100.

 @i(dur) @itemsep the maximum duration in seconds to read.  Default is
10000.

 @i(chans) @itemsep the number of channels to read.  It is assumed that
samples from each channel are interleaved.  Default is 1.

 @i(format) @itemsep the header format.  See @code(s-save) for details.
Default is @code(*default-sf-format*), although this parameter is currently
ignored.

 @i(mode) @itemsep the sample representation, e.g. PCM or float.  See
@code(s-save) for details.  Default is @code(*default-sf-format*).

 @i(n) @itemsep the number of bits per sample.  See @code(s-save) for
details.  Default is @code(*default-sf-bits*).

 @i(flag) @itemsep (T or NIL) swap byte order of each sample. Default is NIL.
@end(itemize)
If there is an error, for example if @i(offset) is greater than the length of the file, then @code(NIL) is returned rather than a sound. Information about the sound is also returned by @code(s-read) through @code(*rslt*)@foot(Since XLISP does not support multiple value returns, multiple value returns are simulated by having the function assign additional return values in a list to the global variable @code(*rslt*). Since this is a global, it should be inspected or copied immediately after the function return to insure that return values are not overwritten by another function.). The list assigned to @code(*rslt*) is of the form: (@i(format) @i(channels) @i(mode) @i(bits) @i(samplerate) @i(duration) @i(flags) @i(byte-offset)), which are defined as follows:
@begin(itemize)
@i(format) @itemsep the header format. See @code(s-save) for details.

@i(channels) @itemsep the number of channels.

@i(mode) @itemsep the sample representation, e.g. PCM or float. See @code(s-save) for details.

@i(bits) @itemsep the number of bits per sample.

@i(samplerate) @itemsep the sample rate, expressed as a @code(FLONUM).

@i(duration) @itemsep the duration of the sound, in seconds.

@i(flags) @itemsep The values for @i(format), @i(channels), @i(mode), @i(bits), @i(samplerate), and @i(duration) are initially just the values passed in as parameters or default values to @code(s-read).  If a value is actually read from the sound file header, a flag is set.  The flags are: @code(snd-head-format), @code(snd-head-channels), @code(snd-head-mode), @code(snd-head-bits), @code(snd-head-srate), and @code(snd-head-dur).  For example,
@begin(example)
(let ((flags (caddr (cddddr  *rslt*))))
  (not (zerop (logand flags snd-head-srate))))
@end(example)
tells whether the sample rate was specified in the file. See also @code(sf-info) below.

@i(byte-offset) @itemsep the byte offset into the file of the first sample
to be read (this is used by the @code(s-overwrite) and @code(s-add-to)
functions).
@end(itemize)

@codef{s-add-to(@pragma(defn)@index(s-add-to)@index(add to file samples)@index(mix to file)@i(expression), @i(maxlen), 
@i(filename) [, @i(offset)])} @c{[sal]}@*
@altdef{@code{(s-add-to @i(expression) @i(maxlen) @i(filename) [@i(offset)])} @c{[lisp]}}@\@label(s-add-to-sec)Evaluates the @i(expression), which should result in a sound
or an array of sounds, and adds the result to the given @i(filename).  
The global @code(*default-sf-dir*) applies. A @code(FLONUM) is returned, 
giving the maximum absolute value of all samples written. The
sample rate(s) of @i(expression) must match those of the file.
The maximum number of samples written per channel is given by @i(maxlen),
which allows writing the initial part of a very long or infinite sound. 
If @i(offset) is specified, the new sound is added to the file beginning at
an @i(offset) from the beginning (in seconds).  The file is extended if
necessary to accommodate the new addition, but if @i(offset)
falls outside of the original file, the file is not modified. (If necessary,
use @code(s-add-to) to extend the file with zeros.) 
The file must be a recognized
sound file with a header (not a raw sound file).


@codef{s-overwrite(@pragma(defn)@index(s-overwrite)@index(replace file samples)@index(overwrite samples)@i(expression), @i(maxlen), @i(filename) [, @i(offset)])} @c{[sal]}@*
@altdef{@code{(s-overwrite @i(expression) @i(maxlen) @i(filename) [@i(offset)])} @c{[lisp]}}@\@label(s-overwrite-sec)Evaluates 
the @i(expression), which should result in a sound
or an array of sounds, and replaces samples in the given @i(filename).  
The global @code(*default-sf-dir*) applies.
A @code(FLONUM) is returned, giving the maximum absolute value of all 
samples written. The
sample rate(s) of @i(expression) must match those of the file.
The maximum number of samples written per channel is given by @i(maxlen),
which allows writing the initial part of a very long or infinite sound. 
If @i(offset) is specified, the new sound is written to the file beginning at
an @i(offset) from the beginning (in seconds). The file is extended if
necessary to accommodate the new insert, but if @i(offset) falls outside of
the original file, the file is not modified. (If necessary, use
@code(s-add-to) to extend the file with zeros.) The file must be a recognized
sound file with a header (not a raw sound file).

@codef{sf-info(@pragma(defn)@index(sf-info)@index(sound file info)@i(filename))} @c{[sal]}@*
@altdef{@code[(sf-info @i(filename))] @c{[lisp]}}@\Prints information about a sound file. The parameter @i(filename) is a string.  The file is assumed to be in *default-sf-dir* (see @code(soundfilename) below) unless the filename begins with ``.'' or ``/''. The source for this function is in the @code(runtime) and provides an example of how to determine sound file parameters. 

@codef{soundfilename(@pragma(defn)@index(soundfilename)@i(name))} @c{[sal]}@*
@altdef{@code[(soundfilename @i(name))] @c{[lisp]}}@\Converts a string @i(name) to a soundfile name.  If @i(name) begins with ``.'' or ``/'', the name is returned without alteration.  Otherwise,  a path taken from @code(*default-sf-dir*) is prepended to @i(name).  The @code(s-plot), @code(s-read), and @code(s-save) functions all use @code(soundfilename) translate filenames.

@codef{s-plot(@pragma(defn)@index(s-plot)@index(plot)@i(sound) 
 [, @i(dur), @i(n)])} @c{[sal]}@*
@altdef{@code{(s-plot @i(sound) 
 [@i(dur) @i(n)])} @c{[lisp]}}@\Plots sound in a window.  This function was designed to run a @code(plot) program on a Unix workstation, but now is
primarily used with @code(NyquistIDE), which has self-contained plotting. Normally,
time/value pairs in ascii are written to @code(points.dat) and system-dependent code
(or the @code(NyquistIDE) program) takes it from there. If the @i(sound) is 
longer than the optional @i(dur) (default is 2 seconds), only the 
first @i(dur) seconds are plotted. 
If there are more than @i(n) samples to be plotted, the signal is interpolated
to have @i(n) samples before plotting.
The data file used is @code(*default-plot-file*):

@codef(*default-plot-file*)@pragma(defn)@index(*default-plot-file*)@\The file containing the data points, defaults to "points.dat".

@codef{s-print-tree(@pragma(defn)@index(s-print-tree)@index(snd-print-tree)@i(sound))} @c{[sal]}@*
@altdef{@code[(s-print-tree @i(sound))] @c{[lisp]}}@\Prints an ascii
representation of the internal data structures representing a sound.  This
is useful for debugging@index(debugging) Nyquist.  Identical to @code(snd-print-tree).

@end(fndefs)

@section(Low-level Functions)
Nyquist includes many low-level functions that are used to implement the functions and behaviors described in previous sections. For completeness, these functions are described here.  Remember that
these are low-level functions that are not intended for normal use.  Unless
you are trying to understand the inner workings of Nyquist, you can skip this section.

@subsection(Creating Sounds)
The basic operations that create sounds are described here.  

@begin(fndefs)

@codef[snd-const(@pragma(defn)@index(snd-const)@i(value), @i(t0), @i(srate),
@i(duration))] @c{[sal]}@*
@altdef{@code[(snd-const @i(value) @i(t0) @i(srate) @i(duration))] @c{[lisp]}}@\Returns a sound with constant @i(value), starting at @i(t0)
with the given @i(duration), at the sample rate @i(srate).  You might want
to use @code(pwl) (see Section @ref(pwl-sec)) instead.

@codef[snd-read(@pragma(defn)@index(snd-read)@i(filename), @i(offset), @i(t0), @i(format),
@i(channels), @i(mode), @i(bits), @i(swap), @i(sr),
@i(dur))] @c{[sal]}@*
@altdef{@code[(snd-read @i(filename) @i(offset) @i(t0) @i(format) @i(channels) @i(mode) @i(bits) @i(swap) @i(sr) @i(dur))] @c{[lisp]}}@\Loads a sound from a file with name @i(filename).  Files are
assumed to consist of a header followed by frames consisting of one sample
from each channel.  The @i(format) specifies the type of header, but this
information is currently ignored.  Nyquist looks for a number of header
formats and automatically figures out which format to read.  If a header can
be identified, the header is first read from the file.  Then, the file
pointer is advanced by the indicated
@i(offset) (in seconds).  If there is an unrecognized header, Nyquist will
assume the file has no header.  If the header size is a multiple of the
frame size (bytes/sample * number-of-channels), you can use @i(offset) to
skip over the header.  To skip N bytes, use an @i(offset) of:
@begin(example)
(/ (float N) @i(sr) (/ @i(bits) 8) @i(channels))
@end(example)
If the header is not a multiple of the frame size, either write a header or
contact the author (dannenberg@@cs.cmu.edu) for assistance.  Nyquist will
round @i(offset) to the nearest sample.  The resulting sound will start at
time @i(t0).  If a header is found, the file will be interpreted according
to the header information.  If no header was found, @i(channels) tells how
many channels there are, the samples are encoded according to @i(mode), the
sample length is @i(bits), and @i(sr) is the sample rate.  The @i(swap) flag is 0 or 1, where 1 means to swap sample bytes. The duration to
be read (in seconds) is given by @i(dur).  If @i(dur) is longer than the
data in the file, then a shorter duration will be returned.  If the file
contains one channel, a sound is returned.  If the file contains 2 or more
channels, an array of sounds is returned.  @p(Note:) you probably want to
call @code(s-read) (see Section @ref(s-read-sec)) instead of
@code(snd-read).  Also, see Section @ref(s-read-sec) for information on the
@i(mode) and @i(format) parameters.

@codef[snd-save(@pragma(defn)@index(snd-save)@i(expression), @i(maxlen),
@i(filename), @i(format), @i(mode), @i(bits), @i(swap), @i(play))] @c{[sal]}@*
@altdef{@code[(snd-save @i(expression) @i(maxlen) @i(filename) @i(format) @i(mode) @i(bits) @i(swap) @i(play))] @c{[lisp]}}@\@label(snd-save)Evaluates 
the @i(expression), which should result in a sound
or an array of sounds, and writes the result to the given @i(filename).  If
a multichannel sound (array) is written, the channels are up-sampled to the
highest rate in any channel so that all channels have the same sample rate.
The maximum number of samples written per channel is given by @i(maxlen),
which allows writing the initial part of a very long or infinite sound. A
header is written according to @i(format), samples are encoded according to
@i(mode), using @i(bits) bits/sample, and swapping bytes if @i(swap) is 1 (otherwise it should be 0).  
If @i(play) is not null, the audio is played in real time (to the extent possible) as it is computed. The peak value of the sound is returned. In addition,
the symbol @code(*RSLT*) is bound to a list containing the sample rate, 
number of channels, and duration (in that order) of the saved sound.
@p(Note:) you probably want to call
@code(s-save) (see Section @ref(s-save-sec)) instead.  The @i(format) and
@i(mode) parameters are described in Section @ref(s-save-sec).

@codef[snd-overwrite(@pragma(defn)@index(snd-overwrite)@i(expression), @i(maxlen), @i(filename), @i(offset), @i(format), @i(mode), @i(bits), @i(swap))] @c{[sal]}@*
@altdef{@code[(snd-overwrite @i(expression) @i(maxlen) @i(filename) @i(offset) @i(format) @i(mode) @i(bits) @i(swap))] @c{[lisp]}}@\@label(snd-overwrite-sec)Evaluates 
the @i(expression), which should result in a sound
or an array of sounds, and replaces samples in the given @i(filename), 
writing the first frame at a time of @i(offset) seconds. The @i(offset) must 
be less than or equal to the duration of the existing file. The 
duration of the written samples may
be greater than that of the file, in which case the file is extended 
as necessary. The
sample rate(s) of @i(expression) and the number of channels 
must match those of the file. If @i(format) is
 @code(SND-HEAD-RAW), then the file
format is given by @i(mode) (see 
@code(snd-save), @i(bits) (per channel), @i(swap) (1 means to 
swap bytes and 0 means write them in the native byte order), and the 
number of channels and sample rate of the sound returned by evaluating
@i(expression). If the 
file is a known
audio file format, @i(format) should be @code(SND-HEAD-NONE), and the
other parameters are ignored. Up to a maximum of @i(maxlen)
samples will be written per channel. The peak value of the sound is returned.
In addition, the symbol @code(*RSLT*) is bound to a list containing the
duration of the written sound (which may not be the duration of the sound
file).
Use @code(s-add-to) (in Section @ref(s-add-to-sec) or
@code(s-overwrite) (in Section @ref(s-overwrite-sec) instead of this function.

@codef[snd-coterm(@pragma(defn)@index(snd-coterm)@index(co-termination)@index(duration of
another sound)@i(s1), @i(s2))] @c{[sal]}@*
@altdef{@code[(snd-coterm @i(s1) @i(s2))] @c{[lisp]}}@\Returns a copy of @i(s1), except the start
time is the maximum of the start times of @i(s1) and @i(s2), and the
termination time is the minimum of @i(s1) and @i(s2). (After the termination
time, the sound is zero as if @i(s1) is gated by @i(s2).)  Some rationale
follows: In order to implement @code(s-add-to), we need to read from the
target sound file, add the sounds to a new sound, and overwrite the result
back into the file.  We only want to write as many samples into the file as
there are samples in the new sound. However, if we are adding
in samples read from
the file, the result of a @code(snd-add) in Nyquist will have the maximum
duration of either sound.  Therefore, we may read to the end of the file.
What we need is a way to truncate the read, but we cannot easily do that,
because we do not know in advance how long the new sound will be. The
solution is to use @code(snd-coterm), which will allow us to truncate the
sound that is read from the file (@i(s1)) according to the duration of the
new sound (@i(s2)).  When this truncated sound is added to the new sound,
the result will have only the duration of the new sound, and this can be
used to overwrite the file. This function is used in the implementation of
@code(s-add-to), which is defined in @code(runtime/fileio.lsp).

@code[(snd-from-array @r(...))] @c{[sal]}@*
@altdef{@code[(snd-from-array @r(...))] @c{[lisp]}}@\See @pragma(startref) page @pageref(snd-from-array-sec).

@codef[snd-white(@pragma(defn)@index(snd-white)@i(t0), @i(sr), @i(d))] @c{[sal]}@*
@altdef{@code[(snd-white @i(t0) @i(sr) @i(d))] @c{[lisp]}}@\Generate white noise, starting at
@i(t0), with sample rate @i(sr), and duration @i(d).  You probably want to
use @code(noise) (see Section @ref(noise-sec)).

 @codef[snd-zero(@pragma(defn)@index(snd-zero)@i(t0), @i(srate))] @c{[sal]}@*
@altdef{@code[(snd-zero @i(t0) @i(srate))] @c{[lisp]}}@\Creates a sound that is
zero everywhere, starts at @i(t0), and has sample rate @i(srate).  The
logical stop time is immediate, i.e. also at @i(t0).  You probably want
to use @code(pwl) (see Section @ref(pwl-sec)) instead.

 @codef[get-slider-value(@pragma(defn)@index(get-slider-value)@i(index))] @c{[sal]}@*
@altdef{@code[(get-slider-value @i(index))] @c{[lisp]}}@\@label(get-slider-value-sec)Return the current value of the slider
named by @i(index) (an integer index into the array of sliders). 
Note that this ``slider'' is just a floating point
value in an array. Sliders can be changed by OSC messages (see @code(osc-enable)) and by sending character
sequences to Nyquist's standard input. (Normally, these character sequences would 
not be typed but generated by the NyquistIDE interactive development environment, which
runs Nyquist as a sub-process, and which present the user with graphical sliders.) 

@codef[snd-slider(@pragma(defn)@index(snd-slider)@i(index), @i(t0), @i(srate), @i(duration))] @c{[sal]}@*
@altdef{@code[(snd-slider @i(index) @i(t0) @i(srate) @i(duration))] @c{[lisp]}}@\@label(snd-slider-sec)Create
a sound controlled by the slider named by @i(index) (an integer 
index into the array of sliders; see @code(get-slider-value) for more information). 
The function returns a sound. Since Nyquist sounds are computed in blocks of samples, 
and each block is computed at once, each block will contain copies of the current slider
value. To obtain reasonable responsiveness, slider sounds should have high (audio) 
sample rates so that the block rate will be reasonably high. Also, consider lowering the audio
latency using @code(snd-set-latency). To ``trigger'' a Nyquist behavior using slider input, see the @code(trigger) function in Section @ref(trigger-sec).

@end(fndefs)

@subsection(Signal Operations)
This next set of functions take sounds as arguments, operate on them, and
return a sound.

@begin(fndefs)
@codef[snd-abs(@pragma(defn)@index(snd-abs)@index(absolute value)@i(sound))] @c{[sal]}@*
@altdef{@code[(snd-abs @i(sound))] @c{[lisp]}}@\Computes a new 
sound where each sample is the absolute value of the corresponding sample in
@i(sound). You should probably use @code(s-abs) instead. (See Section @ref(s-abs-sec).)

@codef[snd-sqrt(@pragma(defn)@index(snd-sqrt)@index(square root)@i(sound))] @c{[sal]}@*
@altdef{@code[(snd-sqrt @i(sound))] @c{[lisp]}}@\Computes a new 
sound where each sample is the square root of the corresponding sample in
@i(sound). If a sample is negative, it is taken to be zero to avoid raising a floating point error. You should probably use @code(s-sqrt) instead. (See Section @ref(s-sqrt-sec).)

 @codef[snd-add(@pragma(defn)@index(snd-add)@i(sound1), @i(sound))] @c{[sal]}@*
@altdef{@code[(snd-add @i(sound1) @i(sound))] @c{[lisp]}}@\Adds two sounds.  The
resulting start time is the minimum of the two parameter start times, the
logical stop time is the maximum of the two parameter stop times, and the
sample rate is the maximum of the two parameter sample rates.  Use
@code(sim) or @code(sum) instead of @code(snd-add) (see Section @ref(sim-sec)).

@codef[snd-offset(@pragma(defn)@index(snd-offset)@index(offset to a sound)@index(add 
offset to sound)@i(sound), @i(offset))] @c{[sal]}@*
@altdef{@code[(snd-offset @i(sound) @i(offset))] @c{[lisp]}}@\Add an offset to a sound. The 
resulting start time, logical stop time, stop time, and sample rate are 
those of @i(sound). Use @code(sum) instead (see Section @ref(sim-sec)).

@codef[snd-avg(@pragma(defn)@index(snd-avg)@index(moving average)@index(RMS)@index(average)@i(sound), @i(blocksize), @i(stepsize), @i(operation))] @c{[sal]}@*
@altdef{@code[(snd-avg @i(sound) @i(blocksize) @i(stepsize) @i(operation))] @c{[lisp]}}@\Computes the averages 
or peak values of blocks of samples. Each output sample is an average or 
peak of @i(blocksize) (a fixnum) adjacent samples from the input @i(sound). 
After each average or peak is taken, the input is advanced by @i(stepsize), 
a fixnum which may be greater or less than @i(blocksize).  The output 
sample rate is the @i(sound) (input) sample rate divided by
@i(stepsize). The duration of the output is the same (approximately,
due to rounding) as that of @i(sound). Notice however, that the
features of the input will appear earlier in the output by half the
window size. For example, a sharp peak in the input will result in a
smoothed peak (using @code(OP-AVERAGE)) one half @i(blocksize)
earlier. You can correct for this shift by inserting one half
@i(blocksize) of silence before @i(sound),
e.g. if @code(s) has a sample rate of 44100 Hz, then
@code[snd-avg(seq(s-rest(0.01), cue(s)), 882, 441, OP-AVERAGE)] will
shift @code(s) by 0.01 s to compensate for the shift introduced by a
smoothing window of size 0.02 s (882/44100).
This function is useful for computing low-sample-rate rms or peak 
amplitude signals for input to @code(snd-gate) or @code(snd-follow).  
To select the operation, @i(operation) should be one of @code(OP-AVERAGE) 
or @code(OP-PEAK).  (These are global lisp variables; the actual 
@i(operation) parameter is an integer.) For RMS computation, see 
@code(rms) in Section @ref(rms-sec).

@codef[snd-clip(@index(clip)@pragma(defn)@index(snd-clip)@i(sound), @i(peak))] @c{[sal]}@*
@altdef{@code[(snd-clip @i(sound) @i(peak))] @c{[lisp]}}@\Hard limit @i(sound) 
to the given @i(peak), a positive number. The samples of @i(sound) are constrained between an upper value
of @i(peak) and a lower value of @subtract()@i(peak). Use @code(clip) instead (see Section @ref(clip-sec)).

@codef[snd-compose(@index(compose)@index(signal composition)@pragma(defn)@index(snd-compose)@i(f), @i(g))] @c{[sal]}@*
@altdef{@code[(snd-compose @i(f) @i(g))] @c{[lisp]}}@\Compose two signals, i.e.
compute @i(f)(@i(g)(@i(t))), where @i(f) and @i(g) are sounds. This function
is used primarily to implement time warping, but it can be used in other
applications such as frequency modulation.  For each sample @i(x) in @i(g),
@i(snd-compose) looks up the value of @i(f)(@i(x)) using linear
interpolation.  The resulting sample rate, start time, etc. are taken from
@i(g).  The sound @i(f) is used in effect as a lookup table, but it is
assumed that @i(g) is non-decreasing, so that @i(f) is accessed in time
order.  This allows samples of @i(f) to be computed and discarded
incrementally.  If in fact @i(g) decreases, the current sample of @i(g)  is
replaced by the previous one, forcing @i(g) into compliance with the
non-decreasing restriction.  See also @code(sref), @code(shape), and
@code(snd-resample).  

For an extended example that uses @code(snd-compose) for variable pitch shifting,
see @code(demos/pitch_change.htm).@index(demos, pitch change)@index(pitch shifting)
@index(variable-resample function)@index(resampling)

@label(snd-tapv-sec)
@codef[snd-tapv(@pragma(defn)@index(snd-tapv)@index(tap)@index(variable delay)@index(delay, 
variable)@index(chorus)@i(sound), @i(offset), @i(vardelay), @i(maxdelay))] @c{[sal]}@*
@altdef{@code[(snd-tapv @i(sound) @i(offset) @i(vardelay) @i(maxdelay))] @c{[lisp]}}@\A 
variable delay: @i(sound) is delayed by the sum of @i(offset) (a @code(FIXNUM) or @code(FLONUM)) 
and @i(vardelay) (a @code(SOUND)).  The specified delay is adjusted to lie in the range 
of zero to @i(maxdelay) seconds to yield the actual delay, and the delay is 
implemented using linear interpolation. This function was designed specifically 
for use in a chorus effect: the @i(offset) is set to half of @i(maxdelay), and 
the @i(vardelay) input is a slow sinusoid. The maximum delay is limited to 
@i(maxdelay), which determines the length of a fixed-sized buffer. The function
@code(tapv) is equivalent and preferred (see Section @ref(tapv-sec)).

@codef[snd-tapf(@pragma(defn)@index(snd-tapf)@index(variable delay)@index(delay, variable)@i(sound), @i(offset), @i(vardelay), @i(maxdelay))] @c{[sal]}@*
@altdef{@code[(snd-tapf @i(sound) @i(offset) @i(vardelay) @i(maxdelay))] @c{[lisp]}}@\A
variable delay like @code(snd-tapv) except there is no linear interpolation. By 
eliminating interpolation, the output is an exact copy of the input with no filtering
or distortion. On the other hand, delays jump by samples causing samples to double or
skip even when the delay is changed smoothly.

@codef[snd-copy(@pragma(defn)@index(snd-copy)@i(sound))] @c{[sal]}@*
@altdef{@code[(snd-copy @i(sound))] @c{[lisp]}}@\Makes a copy of @i(sound).
Since operators always make (logical) copies of their sound parameters, this
function should never be needed.  This function is here for debugging@index(dubugging).

@codef[snd-down(@pragma(defn)@index(snd-down)@i(srate), @i(sound))] @c{[sal]}@*
@altdef{@code[(snd-down @i(srate) @i(sound))] @c{[lisp]}}@\Linear interpolation
of samples down to the given sample rate @i(srate), which must be lower than
the sample rate of @i(sound).  Do not call this function.  Nyquist performs
sample-rate conversion automatically as needed.  If you want to force a
conversion, call @code(force-srate) (see Section @ref(force-srate-sec)).

@codef[snd-exp(@pragma(defn)@index(snd-exp)@i(sound))] @c{[sal]}@*
@altdef{@code[(snd-exp @i(sound))] @c{[lisp]}}@\Compute the exponential of each sample of @i(sound). Use @code(s-exp) instead (see Section @ref(s-exp-sec)).

@label(snd-follow-sec)
@codef[snd-follow(@pragma(defn)@index(snd-follow)@index(follower)@index(envelope follower)@i(sound), @i(floor), @i(risetime), @i(falltime), @i(lookahead))] @c{[sal]}@*
@altdef{@code[(snd-follow @i(sound) @i(floor) @i(risetime) @i(falltime) @i(lookahead))] @c{[lisp]}}@\An envelope 
follower. The basic goal of this function is to generate a smooth signal 
that rides on the peaks of the input signal. The usual objective is to 
produce an amplitude envelope given a low-sample rate (control rate) 
signal representing local RMS measurements. The first argument is the 
input signal. The @i(floor) is the minimum output value. The @i(risetime) is the time (in seconds) it takes for the output to rise (exponentially) from @i(floor) to unity (1.0) and the @i(falltime) is the time it takes for the output to fall (exponentially) from unity to @i(floor). The algorithm looks ahead for peaks and will begin to increase the output signal according to @i(risetime) in anticipation of a peak. The amount of anticipation (in sampless) is given by @i(lookahead).  The algorithm is as follows: the output value is allowed to increase according to @i(risetime) or decrease according to @i(falltime). If the next input sample is in this range, that sample is simply output as the next output sample.  If the next input sample is too large, the algorithm goes back in time as far as necessary to compute an envelope that rises according to @i(risetime) to meet the new value. The algorithm will only work backward as far as @i(lookahead).  If that is not far enough, then there is a final forward pass computing a rising signal from the earliest output sample. In this case, the output signal will be at least momentarily less than the input signal and will continue to rise exponentially until it intersects the input signal. If the input signal falls faster than indicated by @i(falltime), the output fall rate will be limited by @i(falltime), and the fall in output will stop when the output reaches @i(floor). This algorithm can make two passes througth the buffer on sharply rising inputs, so it is not particularly fast. With short buffers and low sample rates this should not matter. See @code(snd-avg) above for a function that can help to generate a low-sample-rate input for @code(snd-follow). See @code(snd-chase) in Section @ref(snd-chase-sec) for a related filter.

@codef[snd-gate(@pragma(defn)@index(snd-gate)@index(noise gate)@index(gate)@i(sound), @i(lookahead), @i(risetime), @i(falltime), @i(floor), @i(threshold))] @c{[sal]}@*
@altdef{@code[(snd-gate @i(sound) @i(lookahead) @i(risetime) @i(falltime) @i(floor) @i(threshold))] @c{[lisp]}}@\This function generates an exponential rise and decay intended for noise gate implementation. The decay starts when the signal drops below threshold and stays there for longer than lookahead. Decay continues until the value reaches floor, at which point the decay stops and the output value is held constant. Either during the decay or after the floor is reached, if the signal goes above threshold, then the output value will rise to unity (1.0) at the point the signal crosses the threshold. Again, look-ahead is used, so the rise actually starts before the signal crosses the threshold. The rise is a constant-rate exponential and set so that a rise from @i(floor) to unity occurs in @i(risetime).  Similarly, the fall is a constant-rate exponential such that a fall from unity to @i(floor) takes @i(falltime). The result is delayed by @i(lookahead), so the output is not actually synchronized with the input. To compensate, you should drop the initial @i(lookahead) of samples. Thus, @code(snd-gate) is not recommended for direct use. Use @code(gate) instead (see Section @ref(gate-sec)).

@codef[snd-inverse(@index(inverse)@pragma(defn)@index(snd-inverse)@i(signal), @i(start), @i(srate))] @c{[sal]}@*
@altdef{@code[(snd-inverse @i(signal) @i(start) @i(srate))] @c{[lisp]}}@\Compute the function inverse of @i(signal), that is, compute @i(g)(@i(t)) such that @i(signal)(@i(g)(@i(t))) = @i(t).  This function assumes that @i(signal) is non-decreasing, it uses linear interpolation, the resulting sample rate is @i(srate), and the result is shifted to have a starting time of @i(start).  If @i(signal) decreases, the true inverse may be undefined, so we define @code(snd-inverse) operationally as follows: for each output time point @i(t), scan ahead in @i(signal) until the value of signal exceeds @i(t).  Interpolate to find an exact time point @i(x) from @i(signal) and output @i(x) at time @i(t).  This function is intended for internal system use in implementing time warps.

@codef[snd-log(@pragma(defn)@index(snd-log)@i(sound))] @c{[sal]}@*
@altdef{@code[(snd-log @i(sound))] @c{[lisp]}}@\Compute the natural logorithm of each sample of @i(sound). Use @code(s-log) instead (see Section @ref(s-log-sec)).

@label(peak-sec)
@codef[peak(@index(peak, maximum amplitude)@pragma(defn)@index(peak)@i(expression), @i(maxlen))] @c{[sal]}@*
@altdef{@code[(peak @i(expression) @i(maxlen))] @c{[lisp]}}@\Compute the maximum absolute value of the amplitude of a sound.  The sound is created by evaluating @i(expression) (as in @code(s-save)).  Only the first @i(maxlen) samples are evaluated. The @i(expression) is automatically quoted (@code(peak) is a macro), so do not quote this parameter.  If @i(expression) is a variable, then the @i(global binding) of that variable will be used.  Also, since the variable retains a reference to the sound, the sound will be evaluated and left in memory.  See Section @ref(peak-ex-sec) on @pragma(startref) page @pageref(peak-ex-sec) for examples.

@label(snd-max-sec)
@codef[snd-max(@pragma(defn)@index(snd-max)@index(maximum amplitude)@i(expression), @i(maxlen))] @c{[sal]}@*
@altdef{@code[(snd-max @i(expression) @i(maxlen))] @c{[lisp]}}@\Compute the maximum absolute value of the amplitude of a sound.  The sound is created by evaluating @i(expression) (as in @code(snd-save)), which is therefore normally quoted by the caller.  At most @i(maxlen) samples are computed.  The result is the maximum of the absolute values of the samples.  @p(Notes:) It is recommended to use @code(peak) (see above) instead.  If you want to find the maximum of a sound bound to a local variable and it is acceptable to save the samples in memory, then this is probably the function to call.  Otherwise, use @code(peak).

@codef[snd-maxv(@pragma(defn)@index(snd-maxv)@index(maximum of two sounds)@i(sound1), @i(sound2))] @c{[sal]}@*
@altdef{@code[(snd-maxv @i(sound1) @i(sound2))] @c{[lisp]}}@\Compute the maximum of @i(sound1) and @i(sound2) on a sample-by-sample basis. The resulting
sound has its start time at the maximum of the input start times and a logical stop
at the minimum logical stop of the inputs. The physical stop time is the minimum of 
the physical stop times of the two sounds. @i(Note that this violates the ``normal'' 
interpretation that sounds are zero outside their start and stop times. For
example, even if) sound1 @i(extends beyond) sound2 @i(and is greater than zero,
the result
value in this extension will be zero because it will be after the physical stop time,
whereas if we simply treated) sound2 @i(as zero in this region and took the maximum, we
would get a non-zero result.) Use @code(s-max) instead (see Section @ref(s-max-sec)).

@codef[snd-normalize(@pragma(defn)@index(snd-normalize)@i(sound))] @c{[sal]}@*
@altdef{@code[(snd-normalize @i(sound))] @c{[lisp]}}@\Internally, sounds
are stored with a scale factor that applies to all samples of the sound.
All operators that take sound arguments take this scale factor into account
(although it is not always necessary to perform an actual multiply per
sample), so you should never need to call this function.  This function
multiplies each sample of a sound by its scale factor, returning a sound
that represents the same signal, but whose scale factor is 1.0.  

@codef[snd-oneshot(@pragma(defn)@index(snd-oneshot)@index(oneshot)@index(threshold)@i(sound), @i(threshold), @i(ontime))] @c{[sal]}@*
@altdef{@code[(snd-oneshot @i(sound) @i(threshold) @i(ontime))] @c{[lisp]}}@\Computes a new sound that is zero
except where @i(sound) exceeds threshold. From these points, the result is 
1.0 until @i(sound) remains below @i(threshold) for @i(ontime) (in seconds).
The result has the same sample rate, start time, logical stop time, and 
duration as @i(sound).

@codef[snd-prod(@pragma(defn)@index(snd-prod)@index(signal multiplication)@index(multiplication)@i(sound1), @i(sound2))] @c{[sal]}@*
@altdef{@code[(snd-prod @i(sound1) @i(sound2))] @c{[lisp]}}@\Computes the
product of @i(sound1) and @i(sound2).  The resulting sound has its start
time at the maximum of the input start times and a logical stop at the minimum
logical stop of the inputs.  Do not use this function.  Use @code(mult) or
@code(prod) instead (see Section @ref(mult-sec)).  Sample rate, start time, etc. are taken from @i(sound).

@codef[snd-pwl(@pragma(defn)@index(snd-pwl)@index(piece-wise linear)@i(t0), @i(sr),
@i(lis))] @c{[sal]}@*
@altdef{@code[(snd-pwl @i(t0) @i(sr) @i(lis))] @c{[lisp]}}@\Computes a piece-wise linear function according to the breakpoints
in @i(lis).  The starting time is @i(t0), and the sample rate is @i(sr).
The breakpoints are passed in an XLISP list (of type @code(LVAL)) where the
list alternates sample numbers (@code(FIXNUM)s, computed in samples 
from the beginning of the pwl function) and values (the value of the pwl
function, given as a @code(FLONUM)).  There is an implicit starting
point of (0, 0).  The list must contain an odd number of points, the omitted 
last
value being implicitly zero (0).  The list is assumed to be well-formed.  Do
not call this function.  Use @code(pwl) instead (see Section @ref(pwl-sec)).

@codef[snd-quantize(@pragma(defn)@index(snd-quantize)@i(sound), @i(steps))] @c{[sal]}@*
@altdef{@code[(snd-quantize @i(sound) @i(steps))] @c{[lisp]}}@\Quantizes a sound. See Section
@ref(quantize-sec) for details.

@codef[snd-recip(@pragma(defn)@index(snd-recip)@i(sound))] @c{[sal]}@*
@altdef{@code[(snd-recip @i(sound))] @c{[lisp]}}@\Compute the reciprocal of each sample of @i(sound). Use @code(recip) instead (see Section @ref(recip-sec)).

@codef[snd-resample(@pragma(defn)@index(snd-resample)@index(sample interpolation)@i(f),
@i(rate))] @c{[sal]}@*
@altdef{@code[(snd-resample @i(f) @i(rate))] @c{[lisp]}}@\Resample sound @i(f) using high-quality interpolation, yielding
a new sound with the specified @i(rate). The result is scaled by 0.95 because often,
in resampling, interpolated values exceed the original sample values, and this 
could lead to clipping.
The resulting start time, etc. are taken from
@i(f). Use @code(resample) instead.

@codef[snd-resamplev(@pragma(defn)@index(snd-resamplev)@index(sample interpolation)@index(signal composition)@i(f), @i(rate), @i(g))] @c{[sal]}@*
@altdef{@code[(snd-resamplev @i(f) @i(rate) @i(g))] @c{[lisp]}}@\Compose two
signals, i.e.  compute @i(f)(@i(g)(@i(t))), where @i(f) and @i(g) are
sounds. The result has sample rate given by @i(rate).  At each time @i(t)
(according to the @i(rate)), @i(g) is linearly interpolated to yield an
increasing sequence of high-precision score-time values. @i(f) is then
interpolated at each value to yield a result sample. If in fact @i(g)
decreases, the current sample of @i(g)  is replaced by the previous one,
forcing @i(g) into compliance with the non-decreasing restriction. 
The result is scaled by 0.95 because often,
in resampling, interpolated values exceed the original sample values, and this 
could lead to clipping. Note that
if @i(g) has a high sample rate, this may introduce unwanted jitter into
sample times. See @code(sound-warp) for a detailed discussion. See
@code(snd-compose) for a fast, low-quality alternative to this function.
Normally, you should use @code(sound-warp) instead of this function.


@codef[snd-scale(@pragma(defn)@index(snd-scale)@i(scale), @i(sound))] @c{[sal]}@*
@altdef{@code[(snd-scale @i(scale) @i(sound))] @c{[lisp]}}@\Scales the amplitude of @i(sound) by the factor @i(scale).  Use @code(scale) instead (see Section
@ref(scale-sec)).

@codef{snd-shape(@pragma(defn)@index(snd-shape)@i(signal), @i(table), @i(origin))} @c{[sal]}@*
@altdef{@code[(snd-shape @i(signal) @i(table) @i(origin))] @c{[lisp]}}@\A waveshaping function.  This is the primitive upon which @code(shape) is based. The @code(snd-shape) function is like @code(shape) except that @i(signal) and @i(table) must be (single-channel) sounds.  Use @code(shape) instead (see Section @ref(shape-sec)).

@codef[snd-up(@pragma(defn)@index(snd-up)@i(srate), @i(sound))] @c{[sal]}@*
@altdef{@code[(snd-up @i(srate) @i(sound))] @c{[lisp]}}@\Increases sample rate by linear
interpolation.  The @i(sound) is the signal to be up-sampled, and @i(srate)
is the output sample rate.  Do not call this function.  Nyquist performs
sample-rate conversion automatically as needed.  If you want to force a
conversion, call @code(force-srate) (see Section @ref(force-srate-sec)).

@label(snd-xform-sec)
@codef[snd-xform(@pragma(defn)@index(snd-xform)@i(sound), @i(sr), @i(time), @i(start),
@i(stop), @i(scale))] @c{[sal]}@*
@altdef{@code[(snd-xform @i(sound) @i(sr) @i(time) @i(start) @i(stop) @i(scale))] @c{[lisp]}}@\Makes a copy of @i(sound) and then alters it in
the following order:  (1) the start time (@code(snd-t0)) of the sound is shifted to 
@i(time), (1) the sound is stretched as a result of setting the sample rate
to @i(sr) (the start time is unchanged by this), (3) the sound is clipped
 from @i(start) to @i(stop), (4) if @i(start) is greater than @i(time), the sound is shifted
shifted by @i(time) - @i(start), so that the start time is @i(time), (5) the
sound is scaled by @i(scale).  An empty (zero) sound at @i(time) will be
returned if all samples are clipped.  Normally, you should accomplish all
this using transformations.  A transformation applied to a sound has no
effect, so use @code(cue) to create a transformable sound (see Section
@ref(use-sounds-sec)).

@label(snd-yin-sec)
@codef{snd-yin(@pragma(defn)@index(snd-yin)@i(sound), @i(minstep), @i(maxstep), @i(rate))} @c{[sal]}@*
@altdef{@code[(snd-yin @i(sound) @i(minstep) @i(maxstep) @i(rate))] @c{[lisp]}}@\Identical to
@code[yin]. See Section @ref(yin-sec).

@end(fndefs)

@subsection(Filters)
These are also ``Signal Operators,'' the subject of the previous section, 
but there are so many filter functions, they are
documented in this special section.

Some filters allow time-varying filter parameters.  In these functions,
filter coefficients are calculated at the sample rate of the filter
parameter, and coefficients are not interpolated.

@begin(fndefs)

@codef[snd-alpass(@pragma(defn)@index(snd-alpass)@i(sound), @i(delay), @i(feedback))] @c{[sal]}@*
@altdef{@code[(snd-alpass @i(sound) @i(delay) @i(feedback))] @c{[lisp]}}@\An all-pass filter.  This produces a repeating echo effect without the resonances of @code(snd-delay).  The @i(feedback) should be less than one to avoid exponential amplitude blowup.  Delay is rounded to the nearest sample.  You should use @code(alpass) instead (see Section @ref(alpass-sec)).

@codef[snd-alpasscv(@pragma(defn)@index(snd-alpasscv)@i(sound), @i(delay),
@i(feedback))] @c{[sal]}@*
@altdef{@code[(snd-alpasscv @i(sound) @i(delay) @i(feedback))] @c{[lisp]}}@\An all-pass filter with variable @i(feedback).  
This is just like @i(snd-alpass) except @i(feedback) is a sound.
You should use @code(alpass) instead (see Section @ref(alpass-sec)).

@codef[snd-alpassvv(@pragma(defn)@index(snd-alpassvv)@i(sound), @i(delay), @i(feedback), @i(maxdelay))] @c{[sal]}@*
@altdef{@code[(snd-alpassvv @i(sound) @i(delay) @i(feedback) @i(maxdelay))] @c{[lisp]}}@\An all-pass filter with variable @i(feedback) and @i(delay). This is just like @i(snd-alpass) except @i(feedback) and @i(delay) are sounds, and there is an additional @code(FLONUM) parameter, @i(maxdelay), that gives an upper bound on the value of @i(delay). @p(Note:) @i(delay) must remain between zero and @i(maxdelay). If not, results are undefined, and Nyquist may crash. You should use @code(alpass) instead (see Section @ref(alpass-sec)).

@codef[snd-areson(@pragma(defn)@index(snd-areson)@i(sound), @i(hz), @i(bw),
@i(normalization))] @c{[sal]}@*
@altdef{@code[(snd-areson @i(sound) @i(hz) @i(bw) @i(normalization))] @c{[lisp]}}@\A notch filter modeled after the @code(areson)
unit generator in Csound.  The @code(snd-areson) filter is an exact
complement of @code(snd-reson) such that if both are applied to the
same signal with the same parameters, the sum of the results yeilds
the original signal.  Note that because of this complementary design,
the power is not normalized as in @code(snd-reson).  See @code(snd-reson)
for details on @i(normalization).  You should use @code(areson) instead (see
Section @ref(areson-sec)).

@codef[snd-aresoncv(@pragma(defn)@index(snd-aresoncv)@i(sound), @i(hz), @i(bw),
@i(normalization))] @c{[sal]}@*
@altdef{@code[(snd-aresoncv @i(sound) @i(hz) @i(bw) @i(normalization))] @c{[lisp]}}@\This function is identical to @code(snd-areson) except
the @i(bw) (bandwidth) parameter is a sound.  Filter coefficients are
updated at the sample rate of @i(bw).  The ``@code(cv)'' suffix stands for Constant,
Variable, indicating that @i(hz) and @i(bw) are constant (a number) and
variable (a sound), respectively.  This naming convention is used throughout.
You should use @code(areson) instead (see
Section @ref(areson-sec)).

@codef[snd-aresonvc(@pragma(defn)@index(snd-aresonvc)@i(sound), @i(hz), @i(bw),
@i(normalization))] @c{[sal]}@*
@altdef{@code[(snd-aresonvc @i(sound) @i(hz) @i(bw) @i(normalization))] @c{[lisp]}}@\This function is identical to @code(snd-areson) except
the @i(hz) (center frequency) parameter is a sound.  Filter coefficients are
updated at the sample rate of @i(hz).  
You should use @code(areson) instead (see
Section @ref(areson-sec)).

@codef[snd-aresonvv(@pragma(defn)@index(snd-aresonvv)@i(sound), @i(hz), @i(bw),
@i(normalization))] @c{[sal]}@*
@altdef{@code[(snd-aresonvv @i(sound) @i(hz) @i(bw) @i(normalization))] @c{[lisp]}}@\This function is identical to @code(snd-areson) except
both @i(hz) (center frequency) and @i(bw) (bandwidth) are sounds.  Filter
coefficients are updated at the next sample of either @i(hz) or @i(bw).
You should use @code(areson) instead (see
Section @ref(areson-sec)).

@codef[snd-atone(@pragma(defn)@index(snd-atone)@i(sound), @i(hz))] @c{[sal]}@*
@altdef{@code[(snd-atone @i(sound) @i(hz))] @c{[lisp]}}@\A high-pass filter 
modeled after the @code(atone) unit generator in Csound.  The @code(snd-atone) filter is an exact
complement of @code(snd-tone) such that if both are applied to the
same signal with the same parameters, the sum of the results yeilds
the original signal.  You should use @code(hp) instead (see
Section @ref(hp-sec)).

@codef[snd-atonev(@pragma(defn)@index(snd-atonev)@i(sound), @i(hz))] @c{[sal]}@*
@altdef{@code[(snd-atonev @i(sound) @i(hz))] @c{[lisp]}}@\This is just like
@code(snd-atone) except that the @i(hz) cutoff frequency is a sound.  Filter
coefficients are updated at the sample rate of @i(hz).  You should use
@code(hp) instead (see Section @ref(hp-sec)).

@codef[snd-biquad(@pragma(defn)@index(snd-biquad)@i(sound), @i(b0), @i(b1), @i(b2), @i(a1), @i(a2), @i(z1init), @i(z2init))] @c{[sal]}@*
@altdef{@code[(snd-biquad @i(sound) @i(b0) @i(b1) @i(b2) @i(a1) @i(a2) @i(z1init) @i(z2init))] @c{[lisp]}}@\A general second order IIR filter, where @i(a0) is assumed to be unity. For @i(a1) and @i(a2), the sign convention is opposite to that of Matlab. All parameters except the input @i(sound) are of type @code(FLONUM). You should probably use one of @code(lowpass2), @code(highpass2), @code(bandpass2), @code(notch2), @code(allpass2), @code(eq-lowshelf), @code(eq-highshelf), @code(eq-band), @code(lowpass4), @code(lowpass6), @code(lowpass8), @code(highpass4), @code(highpass6), or @code(highpass8), which are all based on @code(snd-biquad) and described in Section @ref(lowpass2-sec). For completeness, you will also find @code(biquad) and @code(biquad-m) described in that section.

@label(snd-chase-sec)
@codef[snd-chase(@pragma(defn)@index(snd-chase)@i(sound), @i(risetime), @i(falltime))] @c{[sal]}@*
@altdef{@code[(snd-chase @i(sound) @i(risetime) @i(falltime))] @c{[lisp]}}@\A slew rate limiter. The output ``chases'' the input at rates determined by @i(risetime) and @i(falltime).  If the input changes too fast, the output will lag behind the input. This is a form of lowpass filter, but it was created to turn hard-switching square waves into smoother control signals that could be used for linear crossfades. If the input switches from 0 to 1, the output will linearly rise to 1 in @i(risetime) seconds. If the input switches from 1 to 0, the output will linearly fall to 0 in @i(falltime) seconds.  The generated slope is constant; the transition is linear; this is not an exponential rise or fall.  The @i(risetime) and @i(falltime) must be scalar constants; complain to the author if this is not adequate. The @code(snd-chase) function is safe for ordinary use. See @code(snd-follow) in Section @ref(snd-follow-sec) for a related function. 

@codef[snd-congen(@pragma(defn)@index(snd-congen)@i(gate), @i(risetime), @i(falltime))] @c{[sal]}@*
@altdef{@code[(snd-congen @i(gate) @i(risetime) @i(falltime))] @c{[lisp]}}@\A simple ``contour generator'' based 
on analog synthesizers.  The @i(gate) is a sound that normally steps from 0.0 to 1.0 at the start of an envelop and goes from
1.0 back to 0.0 at the beginning of the release. At each sample, the output converges to the input exponentially.  If @i(gate) is greater than the output, e.g. the attack, then the output converges half-way to the output in @i(risetime).  If the @i(gate) is less than the output, the half-time is @i(falltime).  The sample rate, starting time, logical-stop-time, and terminate time are taken from  @i(gate). You should use @code(congen) instead (see Section @ref(congen-sec).

@codef[snd-convolve(@pragma(defn)@index(snd-convolve)@i(sound), @i(response))] @c{[sal]}@*
@altdef{@code[(snd-convolve @i(sound) @i(response))] @c{[lisp]}}@\Convolves
@i(sound) by @i(response) using a simple O(N x M) algorithm. The @i(sound)
can be any length, but the @i(response) is computed and stored in a table. The required compuation time per sample and total space are proportional to the
length of @i(response). Use @code(convolve) instead (see Section
@ref(convolve-sec)).

@codef[snd-delay(@pragma(defn)@index(snd-delay)@i(sound), @i(delay), @i(feedback))] @c{[sal]}@*
@altdef{@code[(snd-delay @i(sound) @i(delay) @i(feedback))] @c{[lisp]}}@\Feedback
delay.  The output, initially @i(sound), is recursively delayed by @i(delay), scaled by @i(feedback), and added to itself, producing an repeating echo effect.  The @i(feedback) should be less than one to avoid exponential amplitude blowup.  Delay is rounded to the nearest sample.  You should use @code(feedback-delay) instead (see Section @ref(feedback-delay-sec))

@codef[snd-delaycv(@pragma(defn)@index(snd-delaycv)@i(sound), @i(delay),
@i(feedback))] @c{[sal]}@*
@altdef{@code[(snd-delaycv @i(sound) @i(delay) @i(feedback))] @c{[lisp]}}@\Feedback delay with variable @i(feedback).  This is just like
@i(snd-delay) except @i(feedback) is a sound.  You should use
@code(feedback-delay) instead (see Section @ref(feedback-delay-sec)).

@codef[snd-reson(@pragma(defn)@index(snd-reson)@i(sound), @i(hz), @i(bw), @i(normalization))] @c{[sal]}@*
@altdef{@code[(snd-reson @i(sound) @i(hz) @i(bw) @i(normalization))] @c{[lisp]}}@\A
second-order resonating (bandpass) filter with center frequency @i(hz) and
bandwidth @i(bw), modeled after the @code(reson) unit generator in Csound.  
The @i(normalization) parameter must be an integer and (like in Csound)
specifies a scaling factor.  A value of 1 specifies a peak amplitude
response of 1.0; all frequencies other than @i(hz) are attenuated.  A
value of 2 specifies the overall RMS value of the amplitude response
is 1.0; thus filtered white noise would retain the same power.  A value of
zero specifies no scaling.  The result sample rate, start time, etc. are takend from @i(sound).
You should use @code(reson) instead (see Section
@ref(reson-sec)).

@codef[snd-resoncv(@pragma(defn)@index(snd-resoncv)@i(sound), @i(hz), @i(bw),
@i(normalization))] @c{[sal]}@*
@altdef{@code[(snd-resoncv @i(sound) @i(hz) @i(bw) @i(normalization))] @c{[lisp]}}@\This function is identical to @code(snd-reson) except
@i(bw) (bandwidth) is a sound.  Filter coefficients are updated at the
sample rate of @i(bw).  You should use @code(reson) instead (see Section
@ref(reson-sec)).

@codef[snd-resonvc(@pragma(defn)@index(snd-resonvc)@i(sound), @i(hz), @i(bw),
@i(normalization))] @c{[sal]}@*
@altdef{@code[(snd-resonvc @i(sound) @i(hz) @i(bw) @i(normalization))] @c{[lisp]}}@\This function is identical to @code(snd-reson) except
@i(hz) (center frequency) is a sound.  Filter coefficients are updated at the
sample rate of @i(hz).  You should use @code(reson) instead (see Section
@ref(reson-sec)).

@codef[snd-resonvv(@pragma(defn)@index(snd-resonvv)@i(sound), @i(hz), @i(bw),
@i(normalization))] @c{[sal]}@*
@altdef{@code[(snd-resonvv @i(sound) @i(hz) @i(bw) @i(normalization))] @c{[lisp]}}@\This function is identical to @code(snd-reson) except
botth @i(hz) (center frequency) and @i(bw) (bandwidth) are sounds.  Filter
coefficients are updated at the next sample from either @i(hz) or @i(bw).  You should use @code(reson) instead (see Section
@ref(reson-sec)).

@codef[snd-stkchorus(@pragma(defn)@index(snd-stkchorus)@index(chorus)@index(effect, chorus)@index(STK chorus)@i(sound), @i(delay), @i(depth), @i(freq), @i(mix))] @c{[sal]}@*
@altdef{@code[(snd-stkchorus @i(sound) @i(delay) @i(depth) @i(freq) @i(mix))] @c{[lisp]}}@\A chorus implemented in STK. The parameter @i(delay) is a @code(FIXNUM) 
representing the median desired delay length in samples. A typical
value is 6000. The @code(FLONUM) parameters @i(depth) and @i(freq) set the modulation
depth (from 0 to 1) and modulation frequency (in Hz), @i(mix) sets the mixture
of input sound and chorused sound, where a value of 0.0 means input sound 
only (dry) and a value of 1.0 means chorused sound only (wet). 
You should use @code(pitshift) instead
(see Section @ref(stkchorus-sec)).

@codef[snd-stkpitshift(@pragma(defn)@index(snd-stkpitshift)@index(pitch shift)@index(effect, pitch shift)@index(STK pitch shift)@i(sound), @i(shift), @i(mix))] @c{[sal]}@*
@altdef{@code[(snd-stkpitshift @i(sound) @i(shift) @i(mix))] @c{[lisp]}}@\A 
pitch shifter implemented in STK. The @i(sound) is shifted in pitch by
@i(shift), a @code(FLONUM) representing the shift factor. A value of 1.0 means
 no shift.  The parameter @i(mix) sets the mixture of input and shifted sounds.
A value of 0.0 means input only (dry) and a value of 1.0 means shifted 
sound only (wet). You should use @code(pitshift) instead
(see Section @ref(stkpitshift-sec)).

@codef[snd-stkrev(@pragma(defn)@index(snd-stkrev)@index(reverb)@index(effect, reverberation)@index(STK reverb)@i(rev-type), @i(sound), @i(decay), @i(mix))] @c{[sal]}@*
@altdef{@code[(snd-stkrev @i(rev-type) @i(sound) @i(decay) @i(mix))] @c{[lisp]}}@\A reverb implemented in STK. The parameter rev-type is a 
 @code(FIXNUM) ranging from zero to 
two and selects the type of reverb. Zero selects NRev type, one selects JCRev, 
and two selects PRCRev. The input @i(sound) is processed by the reverb with
a @i(decay) time in seconds (a @code(FLONUM)). The @i(mix), a @code(FLONUM), 
sets the 
mixture of dry input and reverb output. A value of 0.0 means input only (dry)
and a value of 1.0 means reverb only (wet). The sample rate 
is that of @i(sound). You
should use @code(nrev), @code(jcrev) or @code(prcrev) instead (see 
Section @ref(stkrev-sec)).

@codef[snd-tone(@pragma(defn)@index(snd-tone)@index(low-pass filter)@i(sound), @i(hz))] @c{[sal]}@*
@altdef{@code[(snd-tone @i(sound) @i(hz))] @c{[lisp]}}@\A
first-order recursive low-pass filter, based on the @i(tone) unit generator
of Csound.  The @i(hz) parameter is the cutoff frequency, the response
curve's half-power point.  The result sample rate, start time, etc. are takend from @i(sound).
You should use @code(lp) instead (see Section
@ref(lp-sec)).

@codef[snd-tonev(@pragma(defn)@index(snd-tonev)@i(sound), @i(hz))] @c{[sal]}@*
@altdef{@code[(snd-tonev @i(sound) @i(hz))] @c{[lisp]}}@\This function is
identical to @code(snd-tone) except @i(hz) (cutoff frequency) is a sound.
The filter coefficients are updated at the sample rate of @i(hz).  You
should use @code(lp) instead (see Section
@ref(lp-sec)).


@end(fndefs)


@subsection(Table-Lookup Oscillator Functions)
These functions all use a sound to describe one period of a periodic
waveform.  In the current implementation, the sound samples are copied to an
array (the waveform table) when the function is called.  To make a
table-lookup oscillator generate a specific pitch, we need to have several
pieces  of information:
@begin(itemize)
A waveform to put into the table.  This comes from the @i(sound) parameter.

The length (in samples) of the waveform.  This is obtained by reading
samples (starting at the sound's start time, not necessarily at time zero)
until the physical stop time of the sound.  (If you read the waveform from a
file or generate it with functions like @code(sim) and @code(sine), then the
physical and logical stop times will be the same and will correspond to the
duration you specified, rounded to the nearest sample.)  

The intrinsic sample rate of the waveform.  This sample rate is simply the
sample rate property of @i(sound).

The pitch of the waveform.  This is supplied by the @i(step) parameter and
indicates the pitch (in steps) of @i(sound).  You might expect that the
pitch would be related to the period (length) of @i(sound), but there is the
interesting case that synthesis based on sampling often loops over multiple
periods.  This means that the fundamental frequency of a generated tone may
be some multiple of the looping rate.  In Nyquist, you always specify the
perceived pitch of the looped @i(sound) if the sound is played at the
@i(sound)'s own sample rate.

The desired pitch.  This is specified by the @i(hz) parameter
in Hertz (cycles per second) in these low-level functions.  Note that this
is not necessarily the ``loop'' rate at which the table is scanned.
Instead, Nyquist figures what sample rate conversion would be necessary to
``transpose'' from the @i(step) which specifies the original pitch of
@i(sound) to @i(hz), which gives the desired pitch.  The mixed use of steps
and Hertz came about because it seemed that sample tables would be tagged
with steps (``I sampled a middle-C''), whereas frequency deviation in the
@code(fmosc) function is linear, thus calling for a specification in Hertz.

The desired sample rate.  This is given by the @i(sr) parameter in Hertz.
@end(itemize)

Other parameters common to all of these oscillator functions are:
@begin(itemize)
@i(t0), the starting time, and

@i(phase), the starting phase in degrees.  Note that if the @i(step)
parameter indicates that the table holds more than one fundamental period, then a starting phase of 360 will be different than a starting phase of 0.
@end(itemize)

@begin(fndefs)
 @codef[snd-amosc(@pragma(defn)@index(snd-amosc)@i(sound), @i(step), @i(sr), @i(hz), @i(t0),
@i(am), @i(phase))] @c{[sal]}@*
@altdef{@code[(snd-amosc @i(sound) @i(step) @i(sr) @i(hz) @i(t0) @i(am) @i(phase))] @c{[lisp]}}@\An oscillator with amplitude modulation.  The sound
@i(am) specifies the amplitude and the logical stop time.  The physical stop
time is also that of @i(am).  You should use @code(amosc) instead (see
Section @ref(amosc-sec)).

@codef[snd-fmosc(@pragma(defn)@index(snd-fmosc)@i(s), @i(step), @i(sr), @i(hz), @i(t0), @i(fm),
@i(phase))] @c{[sal]}@*
@altdef{@code[(snd-fmosc @i(s) @i(step) @i(sr) @i(hz) @i(t0) @i(fm) @i(phase))] @c{[lisp]}}@\A Frequency Modulation oscillator.  The sound @i(fm) specifies
frequency deviation (in Hertz) from @i(hz).  You should use @code(fmosc)
instead (see Section @ref(fmosc-sec)).

@codef[snd-fmfb(@pragma(defn)@index(snd-fmfb)@i(t0), @i(hz), @i(sr), @i(index), @i(dur))] @c{[sal]}@*
@altdef{@code[(snd-fmfb @i(t0) @i(hz) @i(sr) @i(index) @i(dur))] @c{[lisp]}}@\A Feedback FM oscillator. The resulting sound starts
at @i(t0), has a fundamental frequency of @i(hz), a sample rate of @i(sr),
and a duration of @i(dur) seconds. The @i(index) is a @code(FLONUM) that
specifies the amount of feedback. You should use @code(fmfb) instead (see
Section @ref(fmfb-sec)).

@codef[snd-fmfbv(@pragma(defn)@index(snd-fmfbv)@i(t0), @i(hz), @i(sr), @i(index))]@*
@altdef{@code[(snd-fmfv @i(t0) @i(hz) @i(sr) @i(index))] @c{[lisp]}}@\A
 Feedback FM oscillator. The resulting sound starts
at @i(t0), has a fundamental frequency of @i(hz), and
a sample rate of @i(sr). The @i(index) is a @code(SOUND) that
specifies the amount of feedback and determines the duration. 
You should use @code(fmfb) instead (see Section @ref(fmfb-sec)).

@codef[snd-buzz(@pragma(defn)@index(snd-buzz)@i(n), @i(sr), @i(hz), @i(t0), @i(fm))] @c{[sal]}@*
@altdef{@code[(snd-buzz @i(n) @i(sr) @i(hz) @i(t0) @i(fm))] @c{[lisp]}}@\A
buzz oscillator, which generates @i(n) harmonics of equal amplitude.
The @i(fm) specifies
frequency deviation (in Hertz) from @i(hz).  You should use @code(buzz)
instead (see Section @ref(buzz-sec)).

@codef[snd-pluck(@pragma(defn)@index(snd-pluck)@i(sr), @i(hz), @i(t0), @i(d),
 @i(final-amp))] @c{[sal]}@*
@altdef{@code[(snd-pluck @i(sr) @i(hz) @i(t0) @i(d) @i(final-amp))] @c{[lisp]}}@\A Karplus-Strong plucked string oscillator with sample rate
@i(sr), fundamental frequency @i(hz), starting time @i(t0), duration @i(d),
initial amplitude approximately 1.0 (not exact because the string is
initialized with random values) and final amplitude approximately 
@i(final-amp). You should use @code(pluck) instead (see Section
 @ref(pluck-sec)).

@codef[snd-osc(@pragma(defn)@index(snd-osc)@i(s), @i(step), @i(sr), @i(hz), @i(t0), @i(d), @i(phase))] @c{[sal]}@*
@altdef{@code[(snd-osc @i(s) @i(step) @i(sr) @i(hz) @i(t0) @i(d) @i(phase))] @c{[lisp]}}@\A simple table lookup oscillator with fixed frequency.  The duration
is @i(d) seconds.  You should use @code(osc) instead (see Section
@ref(osc-sec)).

@codef[snd-partial(@pragma(defn)@index(snd-partial)@i(sr), @i(hz), @i(t0), @i(env))] @c{[sal]}@*
@altdef{@code[(snd-partial @i(sr) @i(hz) @i(t0) @i(env))] @c{[lisp]}}@\This is a
special case of @code(snd-amosc) that generates a sinusoid starting at phase
0 degrees.  The @i(env) parameter gives the envelope or any other amplitude
modulation.  You should use @code(partial) instead (see Section
@ref(partial-sec)).

@codef[snd-sine(@pragma(defn)@index(snd-sine)@i(t0), @i(hz), @i(sr), @i(d))] @c{[sal]}@*
@altdef{@code[(snd-sine @i(t0) @i(hz) @i(sr) @i(d))] @c{[lisp]}}@\This is a
special case of @code(snd-osc) that always generates a sinusoid with initial
phase of 0 degrees.  You should use @code(sine) instead (see Section
@ref(sine-sec)).

@codef[snd-sampler(@pragma(defn)@index(snd-sampler)@i(s), @i(step),
@i(start), @i(sr), @i(hz), @i(t0), @i(fm), @i(npoints))] @c{[sal]}@*
@altdef{@code[(snd-sampler @i(s) @i(step) @i(start) @i(sr) @i(hz)
@i(t0) @i(fm) @i(npoints))] @c{[lisp]}}@\Returns a sound constructed
by reading a sample from beginning to end and then splicing on copies
of the same sound from a loop point to the end.  
The sound @i(s) is the source sound to be looped, and @i(step) (a
FLONUM) is the nominal fundamental frequency (in steps, not Hz) of
@i(s). The @i(start) (a FLONUM) is the time in seconds at which to
start the loop, @i(sr) (a FLONUM) is the desired sample rate of the
output, @i(hz) is the nominal fundamental frequency of the output,
@i(t0) (a FLONUM) is the starting time of the output, and @i(fm) (a
SOUND) is frequency modulation that is added to @i(hz) to determine
the output fundamental frequency. The parameter @i(npoints) (a FIXNUM)
specifies how many points should be used for sample
interpolation.  Currently this parameter defaults to 2 and only 2-point
(linear) interpolation is implemented.  It is an error to modulate
such that the frequency is negative. Note also that the loop point may
be fractional. This function implements a typical sampling synthesis
algorithm, looping and resampling the input according to the ratio
between the desired fundamental frequency (which is the sum of @i(hz)
and @i(fm)) and the nominal fundamental of the looped sound (which is
assumed to be given by @i(step)). You should use @code(sampler)
instead (see Section @ref(sampler-sec)).


@codef[snd-siosc(@pragma(defn)@index(snd-siosc)@i(tables), @i(sr), @i(hz), @i(t0),
@i(fm))] @c{[sal]}@*
@altdef{@code[(snd-siosc @i(tables) @i(sr) @i(hz) @i(t0) @i(fm))] @c{[lisp]}}@\A Spectral Interpolation Oscillator with frequency modulation. The
@i(tables) is a list of sounds and sample counts as follows: (@i(table0)
@i(count1) @i(table1) ... @i(countN) @i(tableN)). The initial waveform is given by @i(table0), which is interpolated linearly to @i(table1) over the first
@i(count1) samples. From @i(count1) to @i(count2) samples, the waveform is
interpolated from @i(table1) to @i(table2), and so on.  If more than
@i(countN) samples are generated, @i(tableN) is used for the remainder of
the sound. The duration and logical stop time  of the sound is taken from
@i(fm), which specified frequency modulation (deviation) in Hertz. You
should use @code(siosc) instead (see Section @ref(siosc-sec)).

@end(fndefs)

@subsection(Physical Model Functions)
These functions perform some sort of physically-based modeling synthesis.
@begin(fndefs)
@codef[(snd-bandedwg@pragma(defn)@index(snd-bandedwg)@index(STK banded waveguide) @i(freq) @i(bowpress-env) @i(preset) @i(sr))]@*
@altdef{@code[(snd-bandedwg @i(freq) @i(bowpress-env) @i(preset) @i(sr))] @c{[lisp]}}@\A Banded Wave Guide 
Percussion instrument implemented in STK. The parameter @i(freq) is a 
@code(FLONUM) in Hz, @i(bowpress-env) is 
a @code(SOUND) that ranges from zero to one, @i(preset) is a @code(FIXNUM), 
and @i(sr) is the desired sample rate in Hz. Currently, there are four 
presets: uniform-bar (0), tuned-bar (1), glass-harmonica (2), and 
tibetan-bowl (3). You should use @code(wg-uniform-bar), @code(wg-tuned-bar),
 @code(wg-glass-harm), or @code(wg-tibetan-bowl) instead (see Section 
@ref(bandedwg-sec)).

@codef[snd-bowed(@pragma(defn)@index(snd-bowed)@index(stk bowed)@i(freq),
@i(bowpress-env), @i(sr))] @c{[sal]}@*
@altdef{@code[(snd-bowed @i(freq) @i(bowpress-env) @i(sr))] @c{[lisp]}}@\A bowed string instrument implemented in
STK. The freq is a @code(FLONUM) in Hertz, bowpress-env is a
 @code(SOUND) that ranges from z
ero to one, and sr is the desired sample rate (a @code(FLONUM)). 
You should use bowed instead  (see Section @ref(bowed-sec)).

@codef[snd-bowed-freq(@pragma(defn)@index(snd-bowed-freq)@index(stk bowed)@i(freq), @i(bowpress-env), @i(freq-env), @i(sr))] @c{[sal]}@*
@altdef{@code[(snd-bowed-freq @i(freq) @i(bowpress-env) @i(freq-env) @i(sr))] @c{[lisp]}}@\A bowed model just like @code(snd-bowed) but with 
an additional parameter for continuous frequency control. You should use 
@code(bowed-freq) instead (see Section @ref(bowed-sec)).

@codef[snd-clarinet(@pragma(defn)@index(snd-clarinet)@index(stk clarinet)@i(freq), @i(breath-env), @i(sr))] @c{[sal]}@*
@altdef{@code[(snd-clarinet @i(freq) @i(breath-env) @i(sr))] @c{[lisp]}}@\A clarinet
model implemented in STK. The @i(freq) is a @code(FLONUM) in Hertz,
 @i(breath-env) is
a @code(SOUND) that ranges from zero to one, and @i(sr) is the
 desired sample
rate (a @code(FLONUM)). You should use @code(clarinet) instead
 (see Section 
@ref(clarinet-sec)).

@codef[snd-clarinet-freq(@pragma(defn)@index(snd-clarinet-freq)@index(STK clarinet)@i(freq), @i(breath-env), @i(freq-env), @i(sr))] @c{[sal]}@*
@altdef{@code[(snd-clarinet-freq @i(freq) @i(breath-env) @i(freq-env) @i(sr))] @c{[lisp]}}@\A clarinet model just like @code(snd-clarinet) but with 
an additional parameter for continuous frequency control. You should use 
@code(clarinet-freq) instead (see Section @ref(clarinet-sec)).

@codef[snd-clarinet-all(@pragma(defn)@index(snd-clarinet-all)@i(freq), @i(vibrato-freq),
@i(vibrato-gain), @i(freq-env), @i(breath-env),
@i(reed-stiffness), @i(noise), @i(sr))] @c{[sal]}@*
@altdef{@code[(snd-clarinet-all @i(freq) @i(vibrato-freq) @i(vibrato-gain) @i(freq-env) @i(breath-env) @i(reed-stiffness) @i(noise) @i(sr))] @c{[lisp]}}@\A clarinet model just like 
@code(snd-clarinet-freq) but with 
additional parameters for vibrato generation and continuous control of
reed stiffness and breath noise. You should use 
@code(clarinet-all) instead (see Section @ref(clarinet-sec)).

@codef[snd-flute(@pragma(defn)@index(snd-flute)@index(stk flute)@i(freq),
 @i(breath-env), @i(sr))] @c{[sal]}@*
@altdef{@code[(snd-flute @i(freq) @i(breath-env) @i(sr))] @c{[lisp]}}@\A flute implemented in STK. The @i(freq) is a 
@code(FLONUM) in Hertz, @i(breath-env) is a @code(SOUND)
 that ranges from zero to one, and @i(sr) is
 the desired sample rate (a @code(FLONUM)). You should use @code(flute)
 instead (see Section @ref(flute-sec)).

@codef[snd-flute-freq(@pragma(defn)@index(snd-flute-freq)@index(stk flute)@i(freq), @i(breath-env),
@i(freq-env), @i(sr))] @c{[sal]}@*
@altdef{@code[(snd-flute-freq @i(freq) @i(breath-env) @i(freq-env) @i(sr))] @c{[lisp]}}@\A flute model just like @code(snd-flute) but with 
an additional parameter for continuous frequency control. You should use 
@code(flute-freq) instead (see Section @ref(flute-sec)).

@codef[snd-flute-all(@pragma(defn)@index(snd-flute-all)@index(stk flute)@i(freq), @i(vibrato-freq), @i(vibrato-gain), @i(freq-env), @i(breath-env),
@i(jet-delay), @i(noise), @i(sr))] @c{[sal]}@*
@altdef{@code[(snd-flute-all @i(freq) @i(vibrato-freq) @i(vibrato-gain) @i(freq-env) @i(breath-env) @i(jet-delay) @i(noise) @i(sr))] @c{[lisp]}}@\A flute model just like 
@code(snd-flute-freq) but with 
additional parameters for vibrato generation and continuous control of
breath noise. You should use 
@code(flute-all) instead (see Section @ref(flute-sec)).

@codef[snd-mandolin(@pragma(defn)@index(snd-mandolin)@index(STK mandolin)@i(t0), @i(freq), @i(dur), @i(body-size), @i(detune), @i(sr))] @c{[sal]}@*
@altdef{@code[(snd-mandolin @i(t0) @i(freq) @i(dur) @i(body-size) @i(detune) @i(sr))] @c{[lisp]}}@\A plucked
 double-string instrument model implemented in STK. The @i(t0) parameter
 is the starting time (in seconds), @i(freq) is a @code(FLONUM) in
 Hz, @i(body-size) and @i(detune) are @code(FLONUM)s, and @code(sr)
 is the desired sample
 rate. You should use @code(mandolin) instead (see Section @ref(mandolin-sec)).

@codef[snd-modalbar(@pragma(defn)@index(snd-modalbar)@index(STK modal bar)@i(t0), @i(freq), @i(preset), @i(dur), @i(sr))] @c{[sal]}@*
@altdef{@code[(snd-modalbar @i(t0) @i(freq) @i(preset) @i(dur) @i(sr))] @c{[lisp]}}@\Struck bar instrument
 model implemented in STK. The parameter @i(t0) is the starting 
time (in seconds), @i(freq) is a @code(FLONUM) in Hz,
 @code(preset) is a @code(FIXNUM) ranging from 0 to 8, @i(dur) is a 
@code(FLONUM) that
 sets the duration (in seconds) and @i(sr) is the desired sample rate. You 
should use @code(modalbar) instead (see Section @ref(modalbar-sec)).

@codef[snd-sax(@pragma(defn)@index(snd-sax)@index(STK sax)@i(freq), @i(breath-env), @i(sr))] @c{[sal]}@*
@altdef{@code[(snd-sax @i(freq) @i(breath-env) @i(sr))] @c{[lisp]}}@\A sax
model implemented in STK. The @i(freq) is a @code(FLONUM) in Hertz, @i(breath-env) is
a @code(SOUND) that ranges from zero to one, and @i(sr) is the desired sample
rate (a @code(FLONUM)). You should use @code(sax) instead (see Section 
@ref(sax-sec)).

@codef[snd-sax-freq(@pragma(defn)@index(snd-sax-freq)@i(freq), @i(freq-env), @i(breath-env),
 @i(sr))] @c{[sal]}@*
@altdef{@code[(snd-sax-freq @i(freq) @i(freq-env) @i(breath-env) @i(sr))] @c{[lisp]}}@\A sax model just like @code(snd-sax) but with 
an additional parameter for continuous frequency control. You should use 
@code(sax-freq) instead (see Section @ref(sax-sec)).

@codef[snd-sax-all(@pragma(defn)@index(snd-sax-all)@i(freq), @i(vibrato-freq),
@i(vibrato-gain), @i(freq-env), @i(breath-env),
@i(reed-stiffness), @i(noise), @i(blow-pos), @i(reed-table-offset), @i(sr))] @c{[sal]}@*
@altdef{@code[(snd-sax-all @i(freq) @i(vibrato-freq) @i(vibrato-gain) @i(freq-env) @i(breath-env) @i(reed-stiffness) @i(noise) @i(blow-pos) @i(reed-table-offset) @i(sr))] @c{[lisp]}}@\A 
sax model just like 
@code(snd-sax-freq) but with 
additional parameters for vibrato generation and continuous control of
reed stiffness, breath noise, excitation position, and reed table offset.
 You should use 
@code(sax-all) instead (see Section @ref(sax-sec)).

@codef[snd-sitar(@pragma(defn)@index(snd-sitar)@index(STK sitar)@i(t0),
@i(freq), @i(dur), @i(sr))] @c{[sal]}@*
@altdef{@code[(snd-sitar @i(t0) @i(freq) @i(dur) @i(sr))] @c{[lisp]}}@\A sitar model implemented in STK. The parameter 
@i(t0) is the starting time, @i(freq) is a @code(FLONUM) (in Hz), E
@i(dur) sets the duration and @i(sr) is the sample rate (in Hz) 
of the resulting sound. You should use @code(sitar) instead (see Section
@ref(sitar-sec)).


@end(fndefs)


@subsection(Sequence Support Functions)
The next two functions are used to implement Nyquist's @code(seq) construct.

@begin(fndefs)
@codef[snd-seq(@pragma(defn)@index(snd-seq)@i(sound), @i(closure))] @c{[sal]}@*
@altdef{@code[(snd-seq @i(sound) @i(closure))] @c{[lisp]}}@\This function returns
@i(sound) until the logical stop time of @i(sound).  Then, the XLISP 
@i(closure)
is evaluated, passing it the logical stop time of @i(sound) as a
parameter.  The closure must return a sound, which is then added to
@i(sound).  (An add is used so that @i(sound) can continue past its logical
stop if desired.)  Do not call this function.  See @code(seq) in Section
@ref(seq-sec).

@codef[snd-multiseq(@pragma(defn)@index(snd-multiseq)@i(array), @i(closure))] @c{[sal]}@*
@altdef{@code[(snd-multiseq @i(array) @i(closure))] @c{[lisp]}}@\This
function is similar to @code(snd-seq) except the first parameter is a
multichannel sound rather than a single sound.  A multichannel sound is
simply an XLISP array of sounds.  An array of sounds is returned which is
the sum of @i(array) and another array of sounds returned by @i(closure).
The @i(closure) is passed the logical stop time of the multichannel sound,
which is the maximum logical stop time of any element of @i(array).    Do
not call this function.  See @code(seq) in Section @ref(seq-sec).
@end(fndefs)

@begin(fndefs)
@codef[snd-trigger(@pragma(defn)@index(snd-trigger)@i(s), @i(closure))] @c{[sal]}@*
@altdef{@code[(snd-trigger @i(s) @i(closure))] @c{[lisp]}}@\This is one of
the only ways in which a behavior instance can be created by changes in a
signal. When @i(s) (a @code(SOUND)) makes a transition from less than or 
equal to zero to greater than zero, the closure, which takes a starting
time parameter, is evaluated. The closure must return a @code(SOUND). The
sum of all these sounds is returned. If there are no sounds, the result will
be zero. The stop time of the result is the maximum stop time of @i(s) and
all sounds returned by the closure. The sample rate of the return value is
the sample rate of @i(s), and the sounds returned by the closure must all
have that same sample rate. Do not call this function. 
See @code(trigger) in Section @ref(trigger-sec).

An implementation note: There is no way to have @code(snd-trigger) return
a multichannel sound. An alternative implementation would be a built-in
function to scan ahead in a sound to find the time of the next zero crossing.
This could be combined with some LISP code similar to @code(seq) to sum up
instances of the closure. However, this would force arbitrary look-ahead
and therefore would not work with real-time inputs, which was the motivation
for @code(snd-trigger) in the first place.
@end(fndefs)

@chapter(Nyquist Globals)
@index(Global Variables)
There are many global variables in Nyquist. A convention in Lisp is to place asterisks (*) around global variables, e.g. @code(*table*). This is only a convention, and the asterisks are just like any other letter as far as variable names are concerned. Here are some globals users should know about:

@begin(description, leftmargin +2 in, indent -2 in)
@codef(*table*)@index(*table*)@\Default table used by @code(osc) and other oscillators.

@codef(*A4-Hertz*)@pragma(defn)@index(*a4-hertz*)@\Frequency of A4 in Hertz.. Note: you must call @code[(set-pitch-names)] to recompute pitches after changing @code(*A4-Hertz*).

@codef(*autonorm*)@pragma(defn)@index(*autonorm*)@\The normalization factor to be applied to the next sound when @code(*autonorm-type*) is @code('previous). See Sections @ref(peak-ex-sec) and @ref(play-sec).

@codef(*autonormflag*)@pragma(defn)@index(*autonormflag*)@\Enables the automatic normalization feature of the @code(play) command. You should use @code[(autonorm-on)] and @code[(autonorm-off)] rather than setting @code(*autonormflag*) directly. See Sections @ref(peak-ex-sec) and @ref(play-sec).

@codef(*autonorm-max-samples*)@pragma(defn)@index(*autonorm-max-samples*)@\Specifies how many samples will be computed searching for a peak value when @code(*autonorm-type*) is @code('lookahead). See Sections @ref(peak-ex-sec) and @ref(play-sec).

@codef(*autonorm-previous-peak*)@pragma(defn)@index(*autonorm-previous-peak*)@\The peak of the previous sound generated by @code(play). This is used to compute the scale factor for the next sound when @code(*autonorm-type*) is @code('previous). See Sections @ref(peak-ex-sec) and @ref(play-sec).

@codef(*autonorm-target*)@pragma(defn)@index(*autonorm-target*)@\The target peak amplitude for the autonorm feature. The default value is 0.9. See Sections @ref(peak-ex-sec) and @ref(play-sec).

@codef(*autonorm-type*)@pragma(defn)@index(*autonorm-type*)@\Determines how the autonorm feature is implemented. Valid values are @code('lookahead) (the default) and @code('previous). See Sections @ref(peak-ex-sec) and @ref(play-sec).

@codef(*breakenable*)@pragma(defn)@index(*breakenable*)@\Controls whether XLISP enters a break loop when an error is encountered. See Section @ref(symbols-sec).

@codef(*clipping-error*)@pragma(defn)@index(*clipping-error*)@index(clipping)@\If
the peak absolute amplitude value of a sound saved or played exceeds
@code(*clipping-threshold*), an XLISP error is raised. See
@code(*clipping-threshold* for more detail.

@codef(*clipping-threshold*)@pragma(defn)@index(*clipping-threshold*)@\See
@code(*clipping-error*) for a description of this
variable. @code(*clipping-threshold*) is initialized to 127/128. This
number is conservative, 
and it is possible to slightly exceed this value, even with 8-bit
files without actual clipping (consider rounding). Also, floating point
format files will not clip even when the amplitude exceeds 1.0. Note
that a ``clipping'' threshold of 1.0 is optimistic: 1.0 corresponds to
a 16-bit integer value of 32,768 (2^15), but the maximum positive
16-bit integer is 32,767. Thus, a positive sample of 1.0 will clip
when written or played as 16-bit audio.

@codef(*control-srate*)@pragma(defn)@index(*control-srate*)@\Part of the environment, establishes the control sample rate. See Section @ref(environment-sec) for details.

@codef(*default-sf-bits*)@pragma(defn)@index(*default-sf-bits*)@\The default bits-per-sample for sound files. Typically 16.

@codef(*default-sf-dir*)@pragma(defn)@index(*default-sf-dir*)@\The default sound file directory.  Unless you give a full path for a file, audio files are assumed to be in this directory. (Applies to many functions that deal with sound files. Check the function description to see if @code(*default-sf-dir*) applies.)

@codef(*default-sf-format*)@pragma(defn)@index(*default-sf-format*)@\The default sound file format. When you write a file, this will be the default format: AIFF for Mac and most Unix systems, NeXT for NeXT systems, and WAV for Win32.

@codef(*default-sf-srate*)@pragma(defn)@index(*default-sf-srate*)@\The default sample rate for sound files. Typically  44100.0, but often set to 22050.0 for speed in non-critical tasks.

@codef(*default-control-srate*)@pragma(defn)@index(*default-control-srate*)@\Default value for @code(*control-srate*). This value is restored when you execute @code[(top)] to pop out of a debugging session. Change it by calling @code[(set-control-srate @i(value))].

@codef(*default-sound-srate*)@pragma(defn)@index(*default-sound-srate*)@\Default value for @code(*sound-srate*). This value is restored when you execute @code[(top)] to pop out of a debugging session. Change it by calling @code[(set-sound-srate @i(value))].

@codef(*file-separator*)@pragma(defn)@index(*file-separator*)@\The character that separates directories in a path,
e.g. ``@code(/)'' for Unix, ``@code(:)'' for Mac, and ``@code(\)'' for Win32.
This is normally set in @code(system.lsp).

@codef(*rslt*)@pragma(defn)@index(*rslt*)@\When a function returns more than one value, @code(*rslt*) is set to a list of the ``extra'' values. This provides a make-shift version of the @code(multiple-value-return) facility in Common Lisp.

@codef(*sound-srate*)@pragma(defn)@index(*sound-srate*)@\Part of the environment, establishes the audio sample rate. See Section @ref(environment-sec) for details.

@codef(*soundenable*)@pragma(defn)@index(*soundenable*)@\Controls whether writes to a sound file will also be played as audio.  Set this variable by calling @code{(sound-on)} or @code{(sound-off)}.

@codef(*tracenable*)@pragma(defn)@index(*tracenable*)@\Controls whether XLISP prints a backtrace when an error is encountered.

@b(XLISP variables)@\See Section @ref(symbols-sec) for a list of 
global variables defined by XLISP.

@b(Environment variables)@\See Section @ref(environment-sec) for definitions of variables used in the environment for behaviors. In general, you should never set or access these variables directly.

@b(Various constants)@\See Section @ref(constants-sec) for definitions of predefined constants for loudness, duration, and pitch.

@end(description)

@chapter(Time/Frequency Transformation)
Nyquist provides functions for FFT and inverse FFT operations on streams of audio data.
Because sounds can be of any length, but an FFT operates on a fixed amount of data, FFT
processing is typically done in short blocks or windows that move through the audio. Thus,
a stream of samples is converted in to a sequence of FFT frames representing short-term
spectra. 

Nyquist does not have a special data type corresponding to a sequence of FFT frames.
This would be nice, but it would require creating a large set of operations suitable for
processing frame sequences. Another approach, and perhaps the most ``pure'' would
be to convert a single sound into a multichannel sound, with one channel per bin of the
FFT. 

Instead, Nyquist violates its ``pure'' functional model and resorts to objects
for FFT processing. A sequence of frames is represented by an XLISP object. Whenever you
send the selector @code[:next] to the object, you get back either NIL, indicating the
end of the sequence, or you get an array of FFT coefficients. 

The Nyquist function @code[snd-fft] (mnemonic, isn't it?) returns one of the frame sequence
generating objects. You can pass any frame sequence generating object to another function,
@code[snd-ifft], and turn the sequence back into audio. 

With @code[snd-fft] and @code[snd-ifft], you can create all sorts of interesting processes. The main
idea is to create intermediate objects that both accept and generate sequences of frames.
These objects can operate on the frames to implement the desired spectral-domain
processes. Examples of this can be found in the file 
@code[fft_tutorial.htm]@index(fft tutorial)@index(fast fourier transform tutorial)@index(demos, fft),
which is part of the standard Nyquist release. The documentation for @code[snd-fft] and
@code[snd-ifft] follows.

@begin(fndefs)
@codef[snd-fft(@pragma(defn)@index(snd-fft)@index(fft)@i(sound), @i(length), @i(skip), @i(window))] @c{[sal]}@*
@altdef{@code[(snd-fft @i(sound) @i(length) @i(skip) @i(window))] @c{[lisp]}}@\This
function performs an FFT on the first samples in @i(sound) and returns a Lisp array of @code[FLONUM]s. 
The function modifies the @i(sound), violating the normal rule that sounds are immutable in Nyquist, so 
it is advised that you copy the sound using @code[snd-copy] if there are any other references to 
@i(sound). The length of the FFT is specified by @i(length), a @code[FIXNUM] (integer) which must
be a power of 2. After 
each FFT, the sound is advanced by @i(skip) samples, also of type @code[FIXNUM]. Overlapping FFTs, 
where @i(skip) is less than @i(length), are allowed. If @i(window) is not @code[NIL], it must be a sound. 
The first @i(length) samples of @i(window) are multiplied by @i(length) samples of @i(sound) before
performing the FFT. When there are no more samples in @i(sound) to transform,
this function returns @code[NIL]. The coefficients in the returned array, in order, are the DC coefficient,
the first real, the first imaginary, the second real, the second imaginary, etc. 
The last array element corresponds to the real coefficient at the Nyquist frequency.

@codef[snd-ifft(@pragma(defn)@index(snd-ifft)@index(ifft)@index(inverse fft)@i(time), @i(srate), @i(iterator), @i(skip), @i(window))] @c{[sal]}@*
@altdef{@code[(snd-ifft @i(time) @i(srate) @i(iterator) @i(skip) @i(window))] @c{[lisp]}}@\This function performs an IFFT on a sequence of spectral frames obtained from @i(iterator) 
and returns a sound. The start time of the sound is given by @i(time). Typically, this would be computed 
by calling @code[(local-to-global 0)]. The sample rate is given by @i(srate). Typically, this would 
be @code[*sound-srate*], but it might also depend upon the sample rate of the sound from which the 
spectral frames were derived. To obtain each frame, the function sends the message @code[:next] to the 
@i(iterator) object, using XLISP's primitives for objects and message passing. The object should return
an array in the same format as obtained from @code[snd-fft], and the object should return @code[NIL]
when the end of the sound is reached. After each frame is inverse transformed into the time domain, it is 
added to the resulting sound. Each successive frame is added with a sample offset specified by @i(skip) 
relative to the previous frame. This must be an integer greater than zero. If @i(window) is 
not @code[NIL], it must be a sound. This window signal is multiplied by the inverse transformed frame 
before the frame is added to the output sound. The length of each frame should be the same power of 2. 
The length
is implied by the array returned by @i(iterator), so it does not appear as a parameter. This length
is also the number of samples used from @i(window). Extra samples are ignored, and window is padded
with zeros if necessary, so be sure @i(window) is the right length. The resulting sound is computed on
demand as with other Nyquist sounds, so @code[:next] messages are sent to @i(iterator) only when new
frames are needed. One should be careful not to reuse or modify @i(iterator) once it is passed to
@code[snd-ifft].
@end(fndefs)

@chapter(MIDI, Adagio, and Sequences)
@label(adagio-chap)
@index(MIDI)@index(Sequences)
Nyquist includes facilities to read and write MIDI files as well as an ASCII 
text-based score representation language, Adagio. XLISP and Nyquist can be
used to generate MIDI files using compositional algorithms. (See also Section @ref(xmusic-sec).)
A tutorial on using the Adadio representation and MIDI can be found in 
@code(demos/midi_tutorial.htm)@index(demos, midi). The Adagio language is
described below. Adagio was originally developed as part of the CMU MIDI
Toolkit, which included a program to record and play MIDI using the 
Adagio representation. Some of the MIDI features of Adagio may not be
useful within Nyquist.

Nyquist offers a number of different score representations, and you may 
find this confusing. In general, MIDI files are a common way to exchange
music performance data, especially with sequencers and score notation 
systems. The @code(demos/midi_tutorial.htm) examples show how to get the most 
precise control when generating MIDI data. Adagio is most useful as a
text-based score entry language, and it is certainly more compact 
than Lisp expressions for MIDI-like data. The Xmusic library 
(Chapter @ref(xmusic-sec)) is best for algorithmic generation of music
and score manipulation. There are functions to convert between the
Adagio, MIDI sequence data, and Xmusic score representations.

@pragma(doinclude)
@include(adagio-nyquist.mss)

@chapter(Linear Prediction Analysis and Synthesis)
@index(Linear Prediction)@index(LPC)
Nyquist provides functions to perform Linear Prediction Coding (LPC) 
analysis and synthesis. In simple terms, LPC analysis assumes that a
sound is the result of an all-pole filter applied to a source with a 
flat spectrum. LPC is good for characterizing the general spectral 
shape of a signal, which may be time-varying as in speech sounds.
For synthesis, any source can be filtered, allowing the general 
spectral shape of one signal (used in analysis) to be applied to
any source (used in synthesis). A popular effect is to give vowel-like
spectra to musical tones, creating an artificial (or sometimes natural)
singing voice.

Examples of LPC analysis and synthesis can be found in the file 
@code[lpc_tutorial.htm]@index(lpc tutorial)@index(linear prediction tutorial)@index(demos, lpc),
which is part of the standard Nyquist release.

As with FFT processing, LPC analysis takes a sound as input and returns 
a stream of frames. Frames are returned from an object using the @code(:next)
selector just as with FFT frames. An LPC frame is a list consisting of:
@i(RMS1), the energy of the input signal, @i(RMS2), the energy of the
residual signal, @i(ERR), the square root of @i(RMS1)/@i(RMS2), and 
@i(FILTER-COEFS), an array of filter coefficients. To make code more
readable and to avoid code dependence on the exact format of a frame, 
the functions @code(lpc-frame-rms1)@index(lpc-frame-rms1), 
@code(lpc-frame-rms2)@index(lpc-frame-rms2), 
@code(lpc-frame-err)@index(lpc-frame-err), and
@code(lpc-frame-filter-coefs)@index(lpc-frame-filter-coefs) can be
applied to a frame to obtain the respective fields.

The @i(z) transform
of the filter is @i(H)(@i(z)) = 1/@i(A)(@i(z)), where @i(A)(@i(z)) is a
polynomial of the form @i(A)(@i(z)) = 1 + @i(a@-[1])@i(z) + 
@i(a@-[2])@i(z) + ... + @i(a@-[p])@i(z). The @i(FILTER-COEFS) array has
the form @code[#(]@i(a@-[p]) @i(a@-[p-1]) ... @i(a@-[3]) 
@i(a@-[2]) @i(a@-[1])@code[)].

The file @code(lpc.lsp) defines some useful classes and functions. The file
is @i(not) automatically loaded with Nyquist, so you must execute 
@code[(load "lpc")] before using them.

@section(LPC Classes and Functions)
@begin(fndefs)
@codef[make-lpanal-iterator(@pragma(defn)@index(make-lpanal-iterator)@i(sound), @i(framedur), @i(skiptime), @i(npoles))] @c{[sal]}@*
@altdef{@code[(make-lpanal-iterator @i(sound) @i(framedur) @i(skiptime) @i(npoles))] @c{[lisp]}}@\Makes an iterator
object, an instance of @code(lpanal-class), 
that returns LPC frames from successive frames of samples in
@i(sound). The duration (in seconds)
of each frame is given by @i(framedur), a 
@code(FLONUM). The skip size (in seconds) between successive frames
is given by @i(skiptime), a @code(FLONUM). Typical values for
@i(framedur) and @i(skiptime) are 0.08 and 0.04, giving 25 frames
per second and a 50% frame overlap. The number of poles is given
by @i(npoles), a @code(FIXNUM). The result is an object that
responds to the @code(:next) selector by returning a frame as 
described above. @code(NIL) is returned when @i(sound) terminates.
(Note that one or more of the last analysis windows may be 
padded with zeros. @code(NIL) is only returned when the corresponding
window would begin after the termination time of the sound.)

@codef[make-lpc-file-iterator(@pragma(defn)@index(make-lpc-file-iterator)@i(filename))] @c{[sal]}@*
@altdef{@code[(make-lpc-file-iterator @i(filename))] @c{[lisp]}}@\Another way to get LPC frames is to read them from a
 file. This function opens an ASCII file containing LPC frames and
 creates an iterator object, an instance of class @code(lpc-file-class)
 to access them. Create a file using @code(save-lpc-file) (see below).

@codef[save-lpc-file(@pragma(defn)@index(save-lpc-file)@i(lpc-iterator), @i(filename))] @c{[sal]}@*
@altdef{@code[(save-lpc-file @i(lpc-iterator) @i(filename))] @c{[lisp]}}@\Create a file containing LPC frames.
This file can be read by @code[make-lpc-file-iterator] (see above).

@codef{show-lpc-data(@pragma(defn)@index(show-lpc-data)@i(lpc-iterator),
@i(iniframe), @i(endframe) [, @i(poles?)])} @c{[sal]}@*
@altdef{@code{(show-lpc-data @i(lpc-iterator) @i(iniframe) @i(endframe)
 [@i(poles?)])} @c{[lisp]}}@\Print values of LPC
frames from an LPC iterator object. The object is @i(lpc-iterator),
which is typically an instance of @code(lpanal-class) or 
@code(lpc-file-class). Frames are numbered from zero, and only
files starting at @i(iniframe) (a @code[FIXNUM]) and ending before
@i(endframe) (also a @code[FIXNUM]) are printed. By default, only
the values for @i(RMS1), @i(RMS2), and @i(ERR) are printed, but
if optional parameter @i(poles?) is non-@code[NIL], then
the LPC coefficients are also printed.

@codef[allpoles-from-lpc(@pragma(defn)@index(allpoles-from-lpc)@i(snd), @i(lpc-frame))] @c{[sal]}@*
@altdef{@code[(allpoles-from-lpc @i(snd) @i(lpc-frame))] @c{[lisp]}}@\A single LPC frame defines a filter.
Use @code(allpoles-from-lpc) to apply this filter to @i(snd),
a @code(SOUND). To obtain @i(lpc-frame), a @code(LIST)
 containing an LPC frame, either send @code(:next) to an
 LPC iterator, or use @code(nth-frame) (see below). The result
 is a @code(SOUND) whose duration is the same as that of @i(snd).

@codef[lpreson(@pragma(defn)@index(lpreson)@i(snd), @i(lpc-iterator), 
@i(skiptime))] @c{[sal]}@*
@altdef{@code[(lpreson @i(snd) @i(lpc-iterator) @i(skiptime))] @c{[lisp]}}@\Implements a time-varying all-pole filter 
controlled by a sequence of LPC frames from an iterator. The
@code(SOUND) to be filtered is @i(snd), and the source of
LPC frames is @i(lpc-iterator), typically an instance of 
@code(lpanal-class) or @code(lpc-file-class). The frame 
period (in seconds) is given by @i(skiptime) (a @code(FLONUM)).
This number does not have to agree with the @i(skiptime) used
to analyze the frames. (Greater values will cause the filter
evolution slow down, and smaller values will cause it to
speed up.) The result is a @code(SOUND). The duration of the
result is the minimum of the duration of @i(snd) and that of
the sequence of frames. 

@codef[lpc-frame-rms1(@pragma(defn)@index(lpc-frame-rms1)@i(frame))] @c{[sal]}@*
@altdef{@code[(lpc-frame-rms1 @i(frame))] @c{[lisp]}}@\Get the energy of the input signal from a frame.

@codef[lpc-frame-rms2(@pragma(defn)@index(lpc-frame-rms2)@i(frame))] @c{[sal]}@*
@altdef{@code[(lpc-frame-rms2 @i(frame))] @c{[lisp]}}@\Get the energy of the residual from a frame.

@codef[lpc-frame-err(@pragma(defn)@index(lpc-frame-err)@i(frame))] @c{[sal]}@*
@altdef{@code[(lpc-frame-err @i(frame))] @c{[lisp]}}@\Get the square root of @i(RMS1)/@i(RMS2) from a frame.

@codef[lpc-frame-filter-coefs(@pragma(defn)@index(lpc-frame-filter-coefs)@i(frame))] @c{[sal]}@*
@altdef{@code[(lpc-frame-filter-coefs @i(frame))] @c{[lisp]}}@\Get the filter coefficients from a frame.

@end(fndefs)

@section(Low-level LPC Functions)
The lowest-level Nyquist functions for LPC are 
@begin(itemize)
@code(snd-lpanal) for analysis,

@code(snd-allpoles), an all-pole filter with fixed coefficients, and

@code(snd-lpreson), an all-pole filter that takes frames from an LPC iterator.
@end(itemize)

@begin(fndefs)
@codef[snd-lpanal(@pragma(defn)@index(snd-lpanal)@i(samps), @i(npoles))] @c{[sal]}@*
@altdef{@code[(snd-lpanal @i(samps) @i(npoles))] @c{[lisp]}}@\Compute
an LPC frame with @i(npoles) (a @code(FIXNUM)) poles from an 
@code(ARRAY) of samples (@code(FLONUMS)). Note that @code(snd-fetch-array)
can be used to fetch a sequence of frames from a sound. Ordinarily, you
should not use this function. Use @code(make-lpanal-iterator) instead.

@codef[snd-allpoles(@pragma(defn)@index(snd-allpoles)@i(snd), @i(lpc-coefs), @i(gain))] @c{[sal]}@*
@altdef{@code[(snd-allpoles @i(snd) @i(lpc-coefs) @i(gain))] @c{[lisp]}}@\A fixed all-pole filter. The input is
@i(snd), a @code(SOUND). The filter coefficients are given by @i(lpc-coefs)
(an @code(ARRAY)), and the filter gain is given by @i(gain), a @code(FLONUM).
The result is a @code(SOUND) whose duration matches that of @i(snd). 
Ordinarily, you should use @code(allpoles-from-lpc) instead (see above).

@codef[snd-lpreson(@pragma(defn)@index(snd-lpreson)@i(snd), @i(lpc-iterator), 
@i(skiptime))] @c{[sal]}@*
@altdef{@code[(snd-lpreson @i(snd) @i(lpc-iterator) @i(skiptime))] @c{[lisp]}}@\This function is identical to @code(lpreson) (see above).
@end(fndefs)


@chapter(Developing and Debugging in Nyquist)
@index(debugging)@index(developing code)
There are a number of tools, functions, and techniques that can help to debug Nyquist programs. Since these are described in many places 
throughout this manual, this chapter brings together many suggestions and techniques for developing code and debugging. You @i(really) 
should read this chapter before you spend too much time with Nyquist. Many problems that you will certainly run into are addressed here.

@section(Debugging)
Probably the most important debugging tool is the backtrace. There are
two kinds of backtrace: one for SAL, and one for Lisp.

SAL mode is actually just an XLISP function (@code(sal)) that reads
input and evaluates it. When SAL encounters an error, it normally
prints a trace of the SAL stack (all the active functions written in SAL),
exists the current command, and reads the next command.

If you call XLISP functions from SAL, including most Nyquist sound 
processing functions, and an error occurs within these XLISP functions,
you will only see the SAL function that called the XLISP functions
listed in the stack trace. Sometimes you need more details.

When Nyquist encounters an error when it is not running SAL, it 
normally suspends execution and prints an 
error message. To find out where in the program the error occurred 
and how you got there, start by typing @code[(bt)]. This will print 
out the last several function calls and their arguments, which is 
usually sufficient to see what is going on.

In order for @code[(bt)] to work, you must have a couple of global 
variables set: @code(*tracenable*) is ordinarily set to @code(NIL).  
If it is true, then a backtrace is automatically printed when an 
error occurs; @code(*breakenable*) must be set to @code(T), as 
it enables the execution to be suspended when an error is 
encountered. If @code(*breakenable*) is @code(NIL) (false), 
then execution stops when an error occurs but the stack is 
not saved and you cannot get a backtrace. Finally, @code(bt) 
is just a macro to save typing.  The actual backtrace function 
is @code(baktrace), which takes an integer argument telling how 
many levels to print.  All of these things are set up by default 
when you start Nyquist.

To get this XLISP backtrace behavior when SAL encounters an error,
you need to have @code(*breakenable*) set while SAL is running. The
best way to do this is to run within the NyquistIDE program,
open the Preferences dialog, and choose the desired settings, e.g.
``Enable XLISP break on SAL error.''

Since Nyquist sounds are executed with a lazy evaluation scheme, some
errors are encountered when samples are being generated.  In this
case, it may not be clear which expression is in error. Sometimes, it
is best to explore a function or set of functions by examining
intermediate results. Any expression that yields a sound can be
assigned to a variable and examined using one or more of:
@code(s-plot), @code(snd-print-tree), and of course @code(play). The
@code(snd-print-tree) function prints a lot of detail about the inner
representaion of the sound. Keep in mind that if you assign a sound
to a global variable and then look at the samples (e.g. with 
@code(play) or @code(s-plot)), the samples will be retained in
memory. At 4 bytes per sample, a big sound may use all of your 
memory and cause a crash.

Another technique is to use low sample rates so that it is easier to 
plot results or look at samples directly. The calls:
@begin(example)
set-sound-srate(100)
set-control-srate(100)
@end(example)
set the default sample rates to 100, which is too slow for audio, but useful for examining programs and results. The function
@begin(example)
snd-samples(@i(sound), @i(limit))
@end(example)
will convert up to @i(limit) samples from @i(sound) into a Lisp 
array. This is another way to look at results in detail.

The @code(trace) function is sometimes useful.  It prints the name of
a function and its arguments everytimg the function is called, and the
result is printed when the function exits.  To trace the osc function,
type:
@begin(example)
trace(osc)
@end(example)
and to stop tracing, type @code[untrace(osc)].

If a variable needs a value or a function is undefined, and if @code(*breakenable*) was set, you will get a prompt where you can fix
the error (by setting the variable or loading the function definition)
and keep going.  At the debug (or break) prompt, your input must 
be in XLISP, not SAL syntax. Use @code[(co)], short for @code[(continue)] to
reevaluate the variable or function and continue execution.

When you finish debugging a particular call, you can ``pop'' 
up to the top level by typing @code[(top)], a short name for @code[(top-level)].
There is a button named "Top" in the NyquistIDE that takes you back to the
top level (ready to accept XLISP expressions), 
and another button named "SAL" that puts you back in SAL mode.

@section(Useful Functions)
@begin(fndefs)
@codef[grindef(@pragma(defn)@index(grindef)@index(listing of lisp function)@i(name))] @c{[sal]}@*
@altdef{@code[(grindef @i(name))] @c{[lisp]}}@\Prints
a formatted listing of a lisp function. This is often useful to quickly inspect
a function without searching for it in source files. Do not forget to quote the
name, e.g. @code[(grindef 'prod)].

@codef[args(@pragma(defn)@index(args)@index(arguments to a lisp function)@i(name))] @c{[sal]}@*
@altdef{@code[(args @i(name))] @c{[lisp]}}@\Similar
to @code(grindef), this function prints the arguments to a function. This may
be faster than looking up a function in the documentation if you just need a
reminder. For example, @code[(args 'lp)] prints ``(LP S C),'' which may help you
to remember that the arguments are a sound (S) followed by the cutoff (C) 
frequency.
@end(fndefs)

The following functions are useful short-cuts that might have been included in 
XLISP. They are so useful that they are defined as part of Nyquist.

@begin(fndefs)
@codef[incf(@pragma(defn)@index(incf)@index(increment)@i(symbol))] @c{[sal]}@*
@altdef{@code[(incf @i(symbol))] @c{[lisp]}}@\Increment @i(symbol)
by one. This is a macro, and @i(symbol) can be anything that can be set by
@code(setf). Typically, @i(symbol) is a variable: ``@code[(incf i)],'' but
@i(symbol) can also be an array element: ``@code[(incf (aref myarray i))].''

@codef[decf(@pragma(defn)@index(decf)@index(decrement)@i(symbol))] @c{[sal]}@*
@altdef{@code[(decf @i(symbol))] @c{[lisp]}}@\Decrement @i(symbol)
by one. (See @code(incf), above.)

@codef[push(@pragma(defn)@index(push)@i(val), @i(lis))] @c{[sal]}@*
@altdef{@code[(push @i(val) @i(lis))] @c{[lisp]}}@\Push @i(val) onto @i(lis) (a Lisp
list). This is a macro that is equivalent to writing (in Lisp) 
@code[(setf @i(lis) (cons @i(val) @i(lis)))].

@codef[pop(@pragma(defn)@index(pop)@i(lis))] @c{[sal]}@*
@altdef{@code[(pop @i(lis))] @c{[lisp]}}@\Remove (pop) the first item from @i(lis) (a
Lisp list). This is a macro that is equivalent to writing (in Lisp)
@code[(setf @i(lis) (cdr @i(lis)))]. Note that the remaining list is returned,
not the head of the list that has been popped. Retrieve the head of the list
(i.e. the top of the stack) using @code(first) or, equivalently, @code(car).
@end(fndefs)

The following macros are useful control constructs.

@begin(fndefs)
@codef[while(@pragma(defn)@index(while)@i(test), @i(expr1), @i(expr2), @r(...))] @c{[sal]}@*
@altdef{@code[(while @i(test) @i(expr1) @i(expr2) @r(...))] @c{[lisp]}}@\A conventional
``while'' loop. If @i(test) is true, evaluate expressions
 (@i(expr1), @i(expr2), etc.) and repeat. If @i(test) is false, return. This
 expression evaluates to NIL unless the expression @code[(return @i(expr))]
 is evaluated, in which case the value of @i(expr) is returned. In SAL, the 
 loop statement is preferred.

@codef[when(@pragma(defn)@index(when)@i(test), @i(action))] @c{[sal]}@*
@altdef{@code[(when @i(test) @i(action))] @c{[lisp]}}@\A conventional ``if-then'' 
statement. If @i(test) is true, @i(action) is evaluated and returned. Otherwise,
NIL is returned. (Use @code(if) or @code(cond) to implement
 ``if-then-else'' and more complex conditional forms.
@end(fndefs)

It is often necessary to load a file @i(only if) it has not already been
loaded. For example, the @code(pianosyn) library loads very slowly, so if
some other file already loaded it, it would be good to avoid loading it
again. How can you load a file once? Nyquist does not keep track of files
that are loaded, but you must be loading a file to define some function,
so the idea is to tell Nyquist "I require @i(function) from @i(file)"; if
the function does not yet exist, Nyquist satisfies the requirement by loading
the file.
@begin(fndefs)
@codef{require-from(@pragma(defn)@index(require-from)@index(load file conditionally)@i(fnsymbol), 
@i(filename) [, @i(path)])} @c{[sal]}@*
@altdef{@code{(require-from @i(fnsymbol) @i(filename) [@i(path)])} @c{[lisp]}}@\Tests whether @i(fnsymbol), an unquoted 
function name, is defined. If 
not, @i(filename), a @code(STRING),
is loaded. Normally @i(fnsymbol) is a function that will be called from
within the current file, and @i(filename) is the file that defines 
@i(fnsymbol). The @i(path), if a @code(STRING), is prepended to @i(filename).
If @i(path) is @code(t) (true), then the directory of the current file is
used as the path.
@end(fndefs)

Sometimes it is important to load files relative to the current file. For example,
the @code(lib/piano.lsp) library loads data files from the @code(lib/piano) directory,
but how can we find out the full path of @code(lib)? The solution is:
@begin(fndefs)
@codef[current-path(@pragma(defn)@index(current-path)@index(path, 
current)@index(full path name))] @c{[sal]}@*
@altdef{@code[(current-path)] @c{[lisp]}}@\Returns the full path name of the file that is
currently being loaded (see @code(load)). Returns NIL if no file is being loaded.
@end(fndefs) 

Finally, there are some helpful math functions:
@begin(fndefs)
@codef[real-random(@index(random)@index(uniform random)@pragma(defn)@index(real-random)@i(from), @i(to))] @c{[sal]}@*
@altdef{@code[(real-random @i(from) @i(to))] @c{[lisp]}}@\Returns a random @code(FLONUM) between @i(from) and @i(to). (See also @code(rrandom), which is equivalent to @code((real-random 0 1))).

@codef[power(@pragma(defn)@index(power)@index(exponent)@i(x), @i(y))] @c{[sal]}@*
@altdef{@code[(power @i(x) @i(y))] @c{[lisp]}}@\Returns @i(x) raised to
the @i(y) power.
@end(fndefs)

@chapter(Xmusic and Algorithmic Composition)
@label(xmusic-sec)
@index(Xmusic)@index(Algorithmic Composition)
Several Nyquist libraries offer support for algorithmic composition. Xmusic 
is a library for generating sequences and patterns of data. Included in Xmusic 
is the @code(score-gen) macro which helps to generate scores from patterns.
Another important facility is the @code(distributions.lsp) library,
containing many different random number generators.

@section(Xmusic Basics)
Xmusic is inspired by and based on Common Music by Rick Taube. Currently, 
Xmusic only implements patterns and some simple support for scores to be
realized as sound by Nyquist. In contrast, Common Music supports MIDI and
various other synthesis languages and includes a graphical interface, some
visualization tools, and many other features. Common Music runs in Common
Lisp and Scheme, but not XLISP, which is the base language for Nyquist.

Xmusic patterns are objects that generate data streams. For example, 
the @code(cycle-class) of objects generate cyclical patterns such as 
"1 2 3 1 2 3 1 2 3 ...", or "1 2 3 4 3 2 1 2 3 4 ...". Patterns can
be used to  specify pitch sequences, rhythm, loudness, and other parameters.

Xmusic functions are automatically loaded when you start Nyquist.
To use a pattern object, you first create the pattern, e.g.
@begin(example)
set pitch-source = make-cycle(list(c4, d4, e4, f4))
@end(example)
After creating the pattern, you can access it repeatedly 
with @code(next)@index(next in pattern) to generate data, e.g.
@begin(example)
play seqrep(i, 13, pluck(next(pitch-source), 0.2))
@end(example)
This will create a sequence of notes with the following pitches: c, d, 
e, f, c, d, e, f, c, d, e, f, c. If you evaluate this again, the 
pitch sequence will continue, starting on "d".

It is very important not to confuse the creation of a sequence with 
its access. Consider this example:
@begin(example)
play seqrep(i, 13,
         pluck(next(make-cycle(list(c4, d4, e4, f4))), 0.2))
@end(example)
This looks very much like the previous example, but it only repeats notes
on middle-C. The reason is that every time @code(pluck) is evaluated, 
@code(make-cycle) is called and creates a new pattern object. After the 
first item of the pattern is extracted with @code(next), the cycle is not
used again, and no other items are generated.

To summarize this important point, there are two steps to using a pattern.
First, the pattern is created and stored in a
variable using @code(setf). Second, the pattern is accessed (multiple
times) using @code(next).

Patterns can be nested, that is, you can write patterns of patterns. 
In general, the @code(next) function does not return patterns. Instead,
if the next item in a pattern is a (nested) pattern, @code(next) recursively
gets the next item of the nested pattern.

While you might expect that each call to @code(next) would advance the
top-level pattern to the next item, and descend recursively if necessary
to the inner-most nesting level, this is not how @code(next) works. Instead,
@code(next) remembers the last top-level item, and if it was a pattern, 
@code(next) continues to generate items from that same inner pattern
until the end of the inner pattern's @i(period) is reached. The next 
paragraph explains the concept of the @i(period).

The data returned by a pattern object is structured into logical groups
called @i(periods). You can get an entire period (as a list) by calling
@code[next(@i(pattern), t)]@index(next pattern). For example:
@begin(example)
set pitch-source = make-cycle(list(c4, d4, e4, f4))
print next(pitch-source, t)
@end(example)
This prints the list @code[(60 62 64 65)], which is one period 
of the cycle.

You can also get explicit markers that 
delineate periods by calling @code[send(@i(pattern), :next)]. In this 
case, the value returned is either the next item of the pattern, or the
symbol @code(+eop+) if the end of a period has been reached. What 
determines a period? This is up to the specific pattern class, so see the
documentation for specifics. You can override the ``natural'' period
using the keyword @code(for:), e.g.
@begin(example)
set pitch-source = make-cycle(list(c4, d4, e4, f4), for: 3)
print next(pitch-source, t)
print next(pitch-source, t)
@end(example)
This prints the lists @code[(60 62 64) (65 60 62)]. Notice that
these periods just restructure the stream of items 
into groups of 3.

Nested patterns are probably easier to understand by example than by
specification. Here is a simple nested pattern of cycles:
@begin(example)
set cycle-1 = make-cycle({a b c})
set cycle-2 = make-cycle({x y z})
set cycle-3 = make-cycle(list(cycle-1, cycle-2))
exec dotimes(i, 9, format(t, "~A ", next(cycle-3)))
@end(example)
This will print "A B C X Y Z A B C". Notice that the inner-most
cycles @code(cycle-1) and @code(cycle-2) generate a period of items
before the top-level @code(cycle-3) advances to the next pattern.

Before describing specific pattern classes, there are several optional
parameters that apply in the creating of any pattern object. These are:
@begin(description, leftmargin +2 in, indent -2 in)
@code(for:)@\The length of a period. This overrides the default 
by providing a numerical length. The value of this optional 
parameter may be a pattern that generates a sequence of integers
that determine the length of each successive period. A period 
length may not be negative, but it may be zero.

@code(name:)@\A pattern object may be given a name. This is useful 
if the @code(trace:) option is used.

@code(trace:)@\If non-null, this optional parameter causes information
about the pattern to be printed each time an item is generated 
from the pattern.

@end(description)
The built-in pattern classes are described in the following section.

@section(Pattern Classes)
@subsection(cycle)
The @code(cycle-class) iterates repeatedly through a list of items. 
For example, two periods of @code[make-cycle({a b c})] would be
@code[(A B C) (A B C)]. 

@begin(fndefs)
@codef{make-cycle(@pragma(defn)@index(make-cycle)@index(cycle pattern)@index(pattern, cycle)@i(items), for: @i(for), name: @i(name), trace: @i(trace))} @c{[sal]}@*
@altdef{@code{(make-cycle @i(items) :for @i(for) :name @i(name)
 :trace @i(trace))} @c{[lisp]}}@\Make a cycle 
pattern that iterates over @i(items). The default period length is the
length of @i(items). (See above for a description of the 
optional parameters.) If @i(items) is a pattern, a period of the 
pattern becomes the list from which items are generated. The
list is replaced every period of the cycle.
@end(fndefs)

@subsection(line)
The @code(line-class) is similar to the cycle class, but when it reaches the 
end of the list of items, it simply repeats the last item in the list. 
For example, two periods of @code[make-line({a b c})] would be
@code[(A B C) (C C C)]. 

@begin(fndefs)
@codef{make-line(@pragma(defn)@index(make-line)@index(line pattern)@index(pattern, line)@i(items), for: @i(for), name: @i(name), trace: @i(trace))} @c{[sal]}@*
@altdef{@code{(make-line @i(items) :for @i(for) :name @i(name) :trace @i(trace))} @c{[lisp]}}@\Make a line 
pattern that iterates over @i(items). The default period length is the
length of @i(items). As with @code(make-cycle), @i(items) may be a 
pattern. (See above for a description of the optional parameters.)
@end(fndefs)

@subsection(random)
The @code(random-class) generates items at random from a list. The default
selection is uniform random with replacement, but items may be further 
specified with a weight, a minimum repetition count, and a maximum 
repetition count. Weights give the relative probability of the selection
of the item (with a default weight of one). The minimum count specifies how
many times an item, once selected at random, will be repeated. The maximum
count specifies the maximum number of times an item can be selected in a row.
If an item has been generated @i(n) times in succession, and the maximum
is equal to @i(n), then the item is disqualified in the next random selection.
Weights (but not currently minima and maxima) can be patterns. The patterns 
(thus the weights) are recomputed every period.  

@begin(fndefs)
@codef{make-random(@pragma(defn)@index(make-random)@index(random pattern)@index(pattern, random)@i(items), for: @i(for), name: @i(name), trace: @i(trace))} @c{[sal]}@*
@altdef{@code{(make-random @i(items) :for @i(for) :name @i(name) :trace @i(trace))} @c{[lisp]}}@\Make a random 
pattern that selects from @i(items). Any (or all) element(s) of @i(items)
may be lists of the following form: @code{(@i(value) :weight @i(weight)
:min @i(mincount) :max @i(maxcount))}, where @i(value) is the item 
(or pattern) to be generated, @i(weight) is the (optional) 
relative probability of 
selecting this item, @i(mincount) is the (optional) minimum number of repetitions
when this item is selected, and @i(maxcount) is the (optional) maximum number of 
repetitions allowed before selecting some other item. The default period
length is the length of @i(items). If @i(items) is a pattern, a period
from that pattern becomes the list from which random selections are
made, and a new list is generated every period.
@end(fndefs)

@subsection(palindrome)
The @code(palindrome-class) repeatedly traverses a list forwards and then 
backwards. For example, two periods of @code[make-palindrome({a b c})] 
would be @code[(A B C C B A) (A B C C B A)]. The @code(elide:)
keyword parameter controls whether the first and/or last elements are
repeated:
@begin(example)
make-palindrome({a b c}, elide: nil)
     ;; generates A B C C B A A B C C B A ...

make-palindrome({a b c}, elide: t)
     ;; generates A B C B A B C B ...

make-palindrome({a b c}, elide: :first)
     ;; generates A B C C B A B C C B ...

make-palindrome({a b c}, elide: :last)
     ;; generates A B C B A A B C B A ...
@end(example)

@begin(fndefs)
@codef{make-palindrome(@pragma(defn)@index(make-palindrome)@index(palindrome pattern)@index(pattern, palindrome)@i(items), elide: @i(elide), for: @i(for), name: @i(name), trace: @i(trace))} @c{[sal]}@*
@altdef{@code{(make-palindrome @i(items) :elide @i(elide) :for @i(for) :name @i(name) :trace @i(trace))} @c{[lisp]}}@\Generate items
from list alternating in-order and reverse-order sequencing. The keyword 
parameter @i(elide) can have the values @code(:first), @code(:last), 
@code(t), or @code(nil) to control repetition of the first and last elements.
The @i(elide) parameter can also be a pattern, in which case it is evaluated
every period. One period is one complete forward and backward traversal
of the list. If @i(items) is a pattern, a period
from that pattern becomes the list from which random selections are
made, and a new list is generated every period.
@end(fndefs)

@subsection(heap)
The @code(heap-class) selects items in random order from a list
 without replacement, which means that all items are generated once before
any item is repeated. For example, two periods of @code[make-heap({a b c})]
 might be @code[(C A B) (B A C)]. Normally, repetitions can occur 
even if all list elements are distinct. This happens when the last element
of a period is chosen first in the next period. To avoid repetitions, the
@code(max:) keyword argument can be set to 1. The @code(max:) keyword only
controls repetitions from the end of one period to the beginning of the next.
If the list contains more than one copy of the same value, it may be repeated
within a period regardless of the value of @code(max:).

@begin(fndefs)
@codef{make-heap(@pragma(defn)@index(make-heap)@index(heap pattern)@index(pattern, heap)@i(items), for: @i(for), max: @i(max), name: @i(name), trace: @i(trace))} @c{[sal]}@*
@altdef{@code{(make-heap @i(items) :for @i(for) :max @i(max) :name @i(name) :trace @i(trace))} @c{[lisp]}}@\Generate items
randomly from list without replacement. If @i(max) is 1, the first element of 
a new period will not be the same as the last element of the previous period,
avoiding repetition. The default value of @i(max) is 2, meaning repetition is
allowed. The period length is the length
of @i(items). If @i(items) is a pattern, a period
from that pattern becomes the list from which random selections are
made, and a new list is generated every period.
@end(fndefs)

@subsection(accumulation)
The @code(accumulation-class) takes a list of values and returns
the first, followed by the first two, followed by the first three, 
etc. In other words, for each list item, return all items from the
first through the item. For example, if the list is (A B C), each
generated period is (A A B A B C).

@begin(fndefs)
@codef{make-accumulation(@pragma(defn)@index(make-accumulation)@index(accumulation pattern)@index(pattern, accumulation)@i(items), name: @i(name), trace: @i(trace))} @c{[sal]}@*
@altdef{@code{(make-accumulation @i(items) :name @i(name) :trace @i(trace))} @c{[lisp]}}@\For each item, generate items from the first to
the item including the item. The period length is (@i(n)@+(2) + @i(n)) / 2
where @i(n) is the length of @i(items).  If @i(items) is a pattern, a period
from that pattern becomes the list from which items are generated,
and a new list is generated every period. Note that this is similar in 
name but different from @code(make-accumulate).

@subsection(copier)
The @code(copier-class) makes copies of periods from a sub-pattern.
For example, three periods 
of @code[make-copier(make-cycle({a b c}, for: 1), repeat: 2, merge: t)]
would be @code[(A A) (B B) (C C)]. Note that entire periods (not
individual items) are repeated, so in this example the @code(for:) 
keyword was used to force periods to be of length one so that 
each item is repeated by the @code(repeat:) count.

@codef{make-copier(@pragma(defn)@index(make-copier)@index(copier 
pattern)@index(pattern, copier)@i(sub-pattern), repeat: @i(repeat), merge: @i(merge), for: @i(for), name: @i(name), trace: @i(trace))} @c{[sal]}@*
@altdef{@code{(make-copier @i(sub-pattern) :repeat @i(repeat) :merge @i(merge) :for @i(for) :name @i(name) :trace @i(trace))} @c{[lisp]}}@\Generate a period
from @i(sub-pattern) and repeat it @i(repeat) times. If @i(merge) is false
(the default), each repetition of a period from @i(sub-pattern) results
in a period by default. If @i(merge) is true (non-null), then all
 @i(repeat) repetitions of the period are merged into one result
period by default. If the @code(for:) keyword is used, the same items
are generated, but the items are grouped into periods determined by
the @code(for:) parameter. If the @code(for:) parameter is a pattern,
it is evaluated every result period. The @i(repeat) and @i(merge) values 
may be patterns that return a repeat count and a boolean value, respectively. 
If so, these patterns are evaluated initially and after each @i(repeat)
 copies are made (independent of the @code(for:) keyword parameter, if any).
The @i(repeat) value returned by a pattern can also be negative. A negative
number indicates how many periods of @i(sub-pattern) to skip. After skipping
these patterns, new @i(repeat) and @i(merge) values are generated.
@end(fndefs)

@subsection(accumulate)
The @code(accumulate-class) forms the sum of numbers returned by another
pattern.  For example, each period
of @code[make-accumulate(make-cycle({1 2 -3}))] is @code[(1 3 0)].
The default output period length is the length of the input period.

@begin(fndefs)
@codef{make-accumulate(@pragma(defn)@index(make-accumulate)@index(accumulate 
pattern)@index(pattern, accumulate)@i(sub-pattern), for: @i(for), max: @i(maximum), min: @i(minimum), name: @i(name), trace: @i(trace))} @c{[sal]}@*
@altdef{@code{(make-accumulate @i(sub-pattern) :for @i(for) :max @i(maximum) :min @i(minimum) :name @i(name) :trace @i(trace))} @c{[lisp]}}@\Keep
a running sum of numbers generated by @i(sub-pattern). The default
period lengths match the period lengths from @i(sub-pattern). If @i(maximum) (a pattern or a number) is specified, and the running sum exceeds @i(maximum), the running sum is reset to @i(maximum). If @i(minimum) (a pattern or a number) is specified, and the running sum falls below @i(minimum), the running sum is reset to @i(minimum). If @i(minimum) is greater than @i(maximum), the running sum will be set to one of the two values. Note that this is similar in name but not in function to
@code(make-accumulation).
@end(fndefs)

@subsection(sum)
The @code(sum-class) forms the sum of numbers, one from each of two other
patterns.  For example, each period
of @code[make-sum(make-cycle({1 2 3}), make-cycle({4 5 6}))] 
is @code[(5 7 9)].
The default output period length is the length of the input period of the 
first argument. Therefore, the first argument must be a pattern, but the
second argument can be a pattern or a number.

@begin(fndefs)
@codef{make-sum(@pragma(defn)@index(make-sum)@index(sum
pattern)@index(pattern, sum)@i(x), @i(y), for: @i(for), name: @i(name), trace: @i(trace))} @c{[sal]}@*
@altdef{@code{(make-sum @i(x) @i(y) :for @i(for) :name @i(name) :trace @i(trace))} @c{[lisp]}}@\Form
sums of items (which must be numbers) from pattern
 @i(x) and pattern or number @i(y).  The default
period lengths match the period lengths from @i(x).
@end(fndefs)

@subsection(product)
The @code(product-class) forms the product of numbers, one
from each of two other
patterns.  For example, each period
of @code[make-product(make-cycle({1 2 3}), make-cycle({4 5 6}))] 
is @code[(4 10 18)].
The default output period length is the length of the input period of the 
first argument. Therefore, the first argument must be a pattern, but the
second argument can be a pattern or a number.

@begin(fndefs)
@codef{make-product(@pragma(defn)@index(make-product)@index(product pattern)@index(pattern, product)@i(x), @i(y), for: @i(for), name: @i(name), trace: @i(trace))} @c{[sal]}@*
@altdef{@code{(make-product @i(x) @i(y) :for @i(for) :name @i(name) :trace @i(trace))} @c{[lisp]}}@\Form
products of items (which must be numbers) from pattern
 @i(x) and pattern or number @i(y).  The default
period lengths match the period lengths from @i(x).
@end(fndefs)


@subsection(eval)
The @code(eval-class) evaluates an expression to produce each output item.
The default output period length is 1.

@begin(fndefs)
@codef{make-eval(@pragma(defn)@index(make-eval)@index(eval pattern)@index(pattern, eval)@index(expression pattern)@index(pattern, expression)@i(expr), for: @i(for), name: @i(name), trace: @i(trace))} @c{[sal]}@*
@altdef{@code{(make-eval @i(expr) :for @i(for) :name @i(name) :trace @i(trace))} @c{[lisp]}}@\Evaluate
@i(expr) to generate each item. If @i(expr) is a pattern, each item is generated by getting the next item from @i(expr) and evaluating it.
@end(fndefs)


@subsection(length)
The @code(length-class) generates periods of a specified length from 
another pattern. This is similar to using the @code(for:) keyword, but
for many patterns, the @code(for:) parameter alters the points at which
other patterns are generated. For example, if the palindrome pattern
has an @code(elide:) pattern parameter, the value will be computed every
period. If there is also a @code(for:) parameter with a value of 2, then
@code(elide:) will be recomputed every 2 items. In contrast, if the 
palindrome (without a @code(for:) parameter) is embedded in a @i(length)
pattern with a lenght of 2, then the periods will all be of length 2, but
the items will come from default periods of the palindrome, and therefore
the @code(elide:) values will be recomputed at the beginnings of 
default palindrome periods.

@begin(fndefs)
@codef{make-length(@index(length pattern)@index(pattern, 
length)@pragma(defn)@index(make-length)@i(pattern), @i(length-pattern), 
name: @i(name), trace: @i(trace))} @c{[sal]}@*
@altdef{@code{(make-length @i(pattern) @i(length-pattern) :name @i(name) :trace @i(trace))} @c{[lisp]}}@\Make a pattern of class
@code(length-class) that regroups items generated by a
@i(pattern) according to pattern lengths given by @i(length-pattern).
Note that @i(length-pattern) is not optional: There is no default
pattern length and no @code(for:) keyword.
@end(fndefs)

@subsection(window)
The @code(window-class) groups items from another pattern by using a sliding
window. If the @i(skip) value is 1, each output period is formed
by dropping the first item of the previous perioda and appending the next item
from the pattern. The @i(skip) value and the output period length can change
every period. For a simple example, if the period length is 3 and the 
skip value is 1, and the input pattern generates the sequence A, B, C, ...,
then the output periods will be (A B C), (B C D), (C D E), (D E F), ....

@begin(fndefs)
@codef{make-window(@index(window pattern)@index(pattern, 
window)@pragma(defn)@index(make-window)@i(pattern), @i(length-pattern), 
@i(skip-pattern), name: @i(name), trace: @i(trace))} @c{[sal]}@*
@altdef{@code{(make-window @i(pattern) @i(length-pattern) @i(skip-pattern) :name @i(name) :trace @i(trace))} @c{[lisp]}}@\Make
 a pattern of class
@code(window-class) that regroups items generated by a
@i(pattern) according to pattern lengths given by @i(length-pattern) 
and where the period advances by the number of items given by
@i(skip-pattern).
Note that @i(length-pattern) is not optional: There is no default
pattern length and no @code(for:) keyword.
@end(fndefs)


@subsection(markov)
The @code(markov-class) generates items from a Markov model. A Markov model
generates a sequence of @i(states) according to rules which specify possible
future states
given the most recent states in the past. For example, states might be 
pitches, and each pitch might lead to a choice of pitches for the next state.
In the @code(markov-class), states can be either symbols or numbers, but
not arbitrary values or patterns. This makes it easier to specify rules.
However, symbols can be mapped to arbitrary values including pattern 
objects, and these become the actual generated items.
By default, all future states are weighted equally, but weights
may be associated with future states. A Markov model must be
initialized with
a sequence of past states using the @code(past:) keyword. 
The most common form of Markov model is a "first
order Markov model" in which the future item depends only upon one
past item. However, higher order models where the future items depend on
two or more past items are possible. A "zero-order" Markov model, which
depends on no past states, is essentially equivalent to the random pattern.
As an example of a first-order Markov pattern,
two periods of @code[make-markov({{a -> b c} {b -> c} {c -> a}}, past: {a})]
might be @code[(C A C) (A B C)].

@begin(fndefs)
@codef{make-markov(@pragma(defn)@index(make-markov)@index(markov pattern)@index(pattern, markov)@i(rules), past: @i(past), produces: @i(produces), for: @i(for), name: @i(name), trace: @i(trace))} @c{[sal]}@*
@altdef{@code{(make-markov @i(rules) @i(past) :produces @i(produces) :for @i(for) :name @i(name) :trace @i(trace))} @c{[lisp]}}@\Generate a sequence
of items from a Markov process. The @i(rules) parameter has the form: 
@code[(@i(prev1) @i(prev2) ... @i(prevn) -> @i(next1) @i(next2) ... @i(nextn))]
where @i(prev1) through @i(prevn) represent a sequence of most recent
(past) states. The symbol @code(*) is treated specially: it matches any
previous state. If @i(prev1) through @i(prevn) (which may be just one state
as in the example above) match the previously generated states, this
rule applies. Note that every rule must specify the same number of previous
states; this number is known as the order of the Markov model.
The first rule in @i(rules) that applies is used to select the
next state. If no rule applies, the next state is @code(NIL) (which is 
a valid state that can be used in rules).
Assuming a rule applies, the list of possible next
states is specified by @i(next1) 
through @i(nextn). Notice that these are alternative choices for the
next state, not a sequence of future states, and each rule can have
any number of choices. Each choice may be the state
itself (a symbol or a number), or the choice may be a list consisting of
the state and a weight. The weight may be given by a pattern, in which
case the next item of the pattern is obtained every time the rule is 
applied. For example, this rules says that if the previous states were
A and B, the next state can be A with a weight of 0.5 or C with an 
implied weight of 1: @code[(A B -> (A 0.5) C)]. The
default length of the period is the length of @i(rules). The 
@i(past) parameter must be provided. It is a list of states whose length
matches the order of the Markov model. The keyword parameter @i(produces)
may be used to map from state symbols or numbers to other values or 
patterns. The parameter is a list of alternating symbols and values. For
example, to map A to 69 and B to 71, use @code[list(quote(a), 69, quote(b), 71)].
  You can
also map symbols to patterns, for example
@code[list(quote(a), make-cycle({57 69}), quote(b), make-random({59 71}))]. The
next item of the pattern is is generated each time the Markov model generates
the corresponding state.  Finally, the @i(produces) keyword can be 
@code(eval:), which means to evaluate the Markov model state. This could 
be useful if states are Nyquist global variables such as 
@code(C4, CS4, D4, ]..., which evaluate to numerical 
values (60, 61, 62, ...).

@codef{markov-create-rules(@pragma(defn)@index(markov-create-rules)@index(markov analysis)@i(sequence), @i(order) [, @i(generalize)])} @c{[sal]}@*
@altdef{@code{(markov-create-rules @i(sequence) @i(order) [@i(generalize)])} @c{[lisp]}}@\Generate a set of rules suitable for the 
@code(make-markov) function. The @i(sequence) is a ``typical'' sequence
of states, and @i(order) is the order of the Markov model. It is often the
case that a sample sequence will not have a transition from the last state 
to any other state, so the generated Markov model can reach a ``dead end'' 
where no rule applies. This might lead to an infinite stream of NIL's. To
avoid this, the optional parameter @i(generalize) can be set to @code(t) 
(true), indicating that there should be a fallback rule that matches any
previous states and whose future states are weighted according to their
frequency in @i(sequence). For example, if sequence contains 5 A's, 5 B's and
10 G's, the default rule will be @code[(* -> (A 5) (B 5) (G 10))]. This
rule will be appended to the end so it will only apply if no other rule does.
@end(fndefs)

@section(Random Number Generators)
@index(random)@index(probability distributions)@index(distributions, probability)@index(stochastic functions)
The @code(distributions.lsp) library implements random number generators that return random values with various probability distributions. Without this library, you can generate random numbers with @i(uniform) distributions. In a uniform distribution, all values are equally likely. To generate a random integer in some range, use @code(random). To generate a real number (FLONUM) in some range, use @code(real-random) (or @code(rrandom) if the range is 0-1). But there are other interesting distributions. For example, the Gaussian distribution is often used to model 
real-world errors and fluctuations where values are clustered around some central value and large deviations are more unlikely than small ones. See Dennis Lorrain, "A Panoply of Stochastic 'Canons'," @i(Computer Music Journal) vol. 4, no. 1, 1980, pp. 53-81.

In most of the random number generators described below, there are optional parameters to indicate a maximum and/or minimum value. These can be used to truncate the distribution. For example, if you basically want a Gaussian distribution, but you never want a value greater than 5, you can specify 5 as the maximum value. 
The upper and lower bounds are implemented simply by drawing a random number from the full distribution repeatedly until a number falling into the desired range is obtained. Therefore, if you select an acceptable range that is unlikely, it may take Nyquist a long time to find each acceptable random number. The intended use of the upper and lower bounds is to weed out values that are already fairly unlikely.


@begin(fndefs)
@codef[linear-dist(@pragma(defn)@index(linear-dist)@index(linear distribution)@i(g))] @c{[sal]}@*
@altdef{@code[(linear-dist @i(g))] @c{[lisp]}}@\Return a @code(FLONUM) value from a linear distribution, where the probability of a value decreases linearly from zero to @i(g) which must be greater than zero. (See Figure @ref(linear-fig).) The linear distribution is useful for generating for generating time and pitch intervals.
@end(fndefs)

@begin(figure)
@center(@graphic((height = 2.5 in, width = 3.75 in, magnify = 1,
		postscript = "linear-fig.ps"))
@html(<img src="linear-fig.gif"><br><br>)
@fillcaption(The Linear Distribution, @i(g) = 1.)
@tag(linear-fig)
@end(figure)

@begin(fndefs)
@codef{exponential-dist(@pragma(defn)@index(exponential-dist)@index(exponential distribution)@i(delta) [, @i(high)])} @c{[sal]}@*
@altdef{@code{(exponential-dist @i(delta) [@i(high)])} @c{[lisp]}}@\Return a @code(FLONUM) value from an exponential distribution. The initial downward slope is steeper with larger values of @i(delta), which must be greater than zero. (See Figure @ref(exponential-fig). The optional @i(high) parameter puts an artificial upper bound on the return value.
The exponential distribution generates values greater
than 0, and can be used to generate time intervals. Natural random intervals such as the time intervals between the release of atomic particles or the passing of yellow volkswagons in traffic have exponential distributions. The
exponential distribution is memory-less: knowing that a random number from this distribution is greater than some value (e.g. a note duration is at least 1 second) tells you nothing new about how soon the note will end. This
is a continuous distribution, but @code(geometric-dist) (described below) implements the discrete form.
@end(fndefs)

@begin(figure)
@center(@graphic((height = 2.5 in, width = 3.75 in, magnify = 1,
		postscript = "exponential-fig.ps"))
@html(<img src="exponential-fig.gif"><br><br>)
@fillcaption(The Exponential Distribution, @i(delta) = 1.)
@tag(exponential-fig)
@end(figure)

@begin(fndefs)
@codef{gamma-dist(@pragma(defn)@index(gamma-dist)@i(nu) [, @i(high)])} @c{[sal]}@*
@altdef{@code{(gamma-dist @i(nu) [@i(high)])} @c{[lisp]}}@\Return a @code(FLONUM) value from a Gamma distribution. The value is greater than zero, has a mean of @i(nu) (a @code(FIXNUM) greater than zero), and a mode (peak) of around @i(nu) - 1. 
 The optional @i(high) parameter puts an artificial upper bound on the return value.
@end(fndefs)

@begin(figure)
@center(@graphic((height = 2.5 in, width = 3.75 in, magnify = 1,
		postscript = "gamma-fig.ps"))
@html(<img src="gamma-fig.gif"><br><br>)
@fillcaption(The Gamma Distribution, @i(nu) = 4.)
@tag(gamma-fig)
@end(figure)

@begin(fndefs)
@codef{bilateral-exponential-dist(@pragma(defn)@index(bilateral-exponential-dist)@index(bilateral exponential distribution)@i(xmu),
 @i(tau) [, @i(low), @i(high)])} @c{[sal]}@*
@altdef{@code{(bilateral-exponential-dist @i(xmu) @i(tau) [@i(low) @i(high)])} @c{[lisp]}}@\Returns a @code(FLONUM) value from a bilateral exponential distribution, where @i(xmu) is the center of the double exponential and @i(tau) controls the spread of the distribution. A larger @i(tau) gives a wider distribution (greater variance), and @i(tau) must be greater than zero. The @i(low) and @i(high) parameters give optional artificial bounds on the minimum and maximum output values, respectively.
This distribution is similar to the exponential, except
it is centered at 0 and can output negative values as well. Like
the exponential, it can be used to generate time intervals; however, it might be necessary to add a lower bound so as not to compute a negative time interval.
@end(fndefs)

@begin(figure)
@center(@graphic((height = 2.5 in, width = 3.75 in, magnify = 1,
		postscript = "bilateral-fig.ps"))
@html(<img src="bilateral-fig.gif"><br><br>)
@fillcaption(The Bilateral Exponential Distribution.)
@tag(bilateral-fig)
@end(figure)

@begin(fndefs)
@codef{cauchy-dist(@pragma(defn)@index(cauchy-dist)@index(cauchy distribution)@i(tau) [, @i(low), @i(high)])} @c{[sal]}@*
@altdef{@code{(cauchy-dist @i(tau) [@i(low) @i(high)])} @c{[lisp]}}@\Returns a @code(FLONUM) from the Cauchy distribution, a symetric distribution with a high peak at zero and a width (variance) that increases with parameter @i(tau), which must be greater than zero. The @i(low) and @i(high) parameters give optional artificial bounds on the minimum and maximum output values, respectively.
@end(fndefs)

@begin(figure)
@center(@graphic((height = 2.5 in, width = 3.75 in, magnify = 1,
		postscript = "cauchy-fig.ps"))
@html(<img src="cauchy-fig.gif"><br><br>)
@fillcaption(The Cauchy Distribution, @i(tau) = 1.)
@tag(cauchy-fig)
@end(figure)

@begin(fndefs)
@codef{hyperbolic-cosine-dist(@pragma(defn)@index(hyperbolic-cosine-dist)[@i(low), @i(high)])} @c{[sal]}@*
@altdef{@code[(hyperbolic-cosine-dist [@i(low) @i(high)])] @c{[lisp]}}@\Returns a @code(FLONUM) value from the hyperbolic cosine distribution, a symetric distribution with its peak at zero. The @i(low) and @i(high) parameters give optional artificial bounds on the minimum and maximum output values, respectively.
@end(fndefs)

@begin(figure)
@center(@graphic((height = 2.5 in, width = 3.75 in, magnify = 1,
		postscript = "hyperbolic-fig.ps"))
@html(<img src="hyperbolic-fig.gif"><br><br>)
@fillcaption(The Hyperbolic Cosine Distribution.)
@tag(hyperbolic-fig)
@end(figure)

@begin(fndefs)
@codef{logistic-dist(@pragma(defn)@index(logistic-dist)@index(logistic distribution)@i(alpha), @i(beta) [, @i(low), @i(high)])} @c{[sal]}@*
@altdef{@code{(logistic-dist @i(alpha) @i(beta) [@i(low) @i(high)])} @c{[lisp]}}@\Returns a @code(FLONUM) value from the logistic distribution, which is symetric about the mean. The @i(alpha) parameter primarily affects dispersion (variance), with larger values resulting in values closer to the mean (less variance), and the @i(beta) parameter primarily influences the mean. The @i(low) and @i(high) parameters give optional artificial bounds on the minimum and maximum output values, respectively.
@end(fndefs)

@begin(figure)
@center(@graphic((height = 2.5 in, width = 3.75 in, magnify = 1,
		postscript = "logistic-fig.ps"))
@html(<img src="logistic-fig.gif"><br><br>)
@fillcaption(The Logistic Distribution, alpha = 1, beta = 2.)
@tag(logistic-fig)
@end(figure)

@begin(fndefs)
@codef{arc-sine-dist(@pragma(defn)@index(arc-sine-dist)@index(arcsine distribution))} @c{[sal]}@*
@altdef{@code{(arc-sine-dist)} @c{[lisp]}}@\Returns a @code(FLONUM) value from the arc sine distribution, which outputs values between 0 and 1. It is symetric about the mean of 1/2, but is more likely to generate values closer to 0 and 1. 
@end(fndefs)

@begin(figure)
@center(@graphic((height = 2.5 in, width = 3.75 in, magnify = 1,
		postscript = "arcsine-fig.ps"))
@html(<img src="arcsine-fig.gif"><br><br>)
@fillcaption(The Arc Sine Distribution.)
@tag(arcsine-fig)
@end(figure)

@begin(fndefs)
@codef{gaussian-dist(@index(Gaussian distribution)@pragma(defn)@index(gaussian-dist)@i(xmu), @i(sigma) [, @i(low), @i(high)])} @c{[sal]}@*
@altdef{@code{(gaussian-dist @i(xmu) @i(sigma) [@i(low) @i(high)])} @c{[lisp]}}@\Returns a @code(FLONUM) value from the Gaussian or Gauss-Laplace distribution, a linear function of the normal distribution. It is symetric about the mean of @i(xmu), with a standard deviation of @i(sigma), which must be greater than zero. The @i(low) and @i(high) parameters give optional artificial bounds on the minimum and maximum output values, respectively.
@end(fndefs)

@begin(figure)
@center(@graphic((height = 2.5 in, width = 3.75 in, magnify = 1,
		postscript = "gaussian-fig.ps"))
@html(<img src="gaussian-fig.gif"><br><br>)
@fillcaption{The Gauss-Laplace (Gaussian) Distribution, @i(xmu) = 0, @i(sigma) = 1.}
@tag(gaussian-fig)
@end(figure)

@begin(fndefs)
@codef{beta-dist(@index(beta distribution)@pragma(defn)@index(beta-dist)@i(a), @i(b))} @c{[sal]}@*
@altdef{@code[(beta-dist @i(a) @i(b))] @c{[lisp]}}@\Returns a @code(FLONUM) value from the Beta distribution. This distribution outputs values between 0 and 1, with outputs more likely to be close to 0 or 1. The parameter @i(a) controls the height (probability) of the right side of the distribution (at 1) and @i(b) controls the height of the left side (at 0). The distribution is symetric about 1/2 when @i(a) = @i(b).
@end(fndefs)

@begin(figure)
@center(@graphic((height = 2.5 in, width = 3.75 in, magnify = 1,
		postscript = "beta-fig.ps"))
@html(<img src="beta-fig.gif"><br><br>)
@fillcaption(The Beta Distribution, @i(alpha) = .5, @i(beta) = .25.)
@tag(beta-fig)
@end(figure)

@begin(fndefs)
@codef{bernoulli-dist(@index(Bernoulli distribution)@pragma(defn)@index(bernoulli-dist)@i(px1) [, @i(x1), @i(x2)])} @c{[sal]}@*
@altdef{@code{(bernoulli-dist @i(px1) [@i(x1) @i(x2)])} @c{[lisp]}}@\Returns either @i(x1) (default value is 1) with probability @i(px1) or @i(x2) (default value is 0) with probability 1 - @i(px1). The value of @i(px1) should be between 0 and 1. By
convention, a result of @i(x1) is viewed as a success while @i(x2) is viewed as
a failure.
@end(fndefs)

@begin(figure)
@center(@graphic((height = 3.5 in, width = 3.75 in, magnify = 0.75,
		postscript = "bernoulli-fig.ps"))
@html(<img src="bernoulli-fig.gif"><br><br>)
@fillcaption(The Bernoulli Distribution, @i(px1) = .75.)
@tag(bernoulli-fig)
@end(figure)

@begin(fndefs)
@codef{binomial-dist(@index(binomial distribution)@pragma(defn)@index(binomial-dist)@i(n), @i(p))} @c{[sal]}@*
@altdef{@code{(binomial-dist @i(n) @i(p))} @c{[lisp]}}@\Returns a @code(FIXNUM) value from the binomial distribution, where @i(n) is the number of Bernoulli trials run (a @code(FIXNUM)) and @i(p) is the probability of success in the Bernoulli trial (a @code(FLONUM) from 0 to 1). The mean is the product of @i(n) and @i(p).
@end(fndefs)

@begin(figure)
@center(@graphic((height = 3.5 in, width = 3.75 in, magnify = 0.75,
		postscript = "binomial-fig.ps"))
@html(<img src="binomial-fig.gif"><br><br>)
@fillcaption(The Binomial Distribution, @i(n) = 5, @i(p) = .5.)
@tag(binomial-fig)
@end(figure)

@begin(fndefs)
@codef{geometric-dist(@index(geometric distribution)@pragma(defn)@index(geometric-dist)@i(p))} @c{[sal]}@*
@altdef{@code[(geometric-dist @i(p))] @c{[lisp]}}@\Returns a @code(FIXNUM) value from the geometric distribution, which is defined as the number of failures before a success is achieved in a Bernoulli trial with probability of success @i(p) (a @code(FLONUM) from 0 to 1).
@end(fndefs)

@begin(figure)
@center(@graphic((height = 3.5 in, width = 3.75 in, magnify = 0.75,
		postscript = "geometric-fig.ps"))
@html(<img src="geometric-fig.gif"><br><br>)
@fillcaption(The Geometric Distribution, @i(p) = .4.)
@tag(geometric-fig)
@end(figure)

@begin(fndefs)
@codef{poisson-dist(@index(Poisson distribution)@pragma(defn)@index(poisson-dist)@i(delta))} @c{[sal]}@*
@altdef{@code[(poisson-dist @i(delta))] @c{[lisp]}}@\Returns a @code(FIXNUM) value from the Poisson distribution with a mean of @i(delta) (a @code(FIXNUM)). The Poisson distribution is often used to generate a sequence of time intervals, resulting in random but often pleasing rhythms.
@end(fndefs)

@begin(figure)
@center(@graphic((height = 3.5 in, width = 3.75 in, magnify = 0.75,
		postscript = "poisson-fig.ps"))
@html(<img src="poisson-fig.gif"><br><br>)
@fillcaption(The Poisson Distribution, @i(delta) = 3.)
@tag(poisson-fig)
@end(figure)

@begin(comment)
*****************
Note: this should remain out of Nyquist until the granulate code is cleaned up (why are there separate functions for pitch-dist and len-dist instead of a single function where any parameter can be specified by a closure?)
*****************
@begin(fndefs)
@codef{pitch-dist-granulate(@pragma(defn)@index(pitch-dist-granulate)@index(granular synthesis)@i(filename), @i(grain-dur), @i(grain-dev), @i(ioi), @i(ioi-dev), @i(pitch-dist) [, @i(file-start), @i(file-end)])} @c{[sal]}@*
@altdef{@code{(pitch-dist-granulate @i(filename) @i(grain-dur) @i(grain-dev) @i(ioi) @i(ioi-dev) @i(pitch-dist) [@i(file-start) @i(file-end)])} @c{[lisp]}}@\*** need to write this ***

@i(filename) @dash name of the file

@i(dist) @dash the distribution that determines the length of the grains

@i(ioi) @dash the basic inter-onset-interval for grains

@i(ioi-dev) @dash ioi is actually: ioi + random(0, ioi-dev)

@i(pitch-dev) @dash grains are resampled at rate between 1 and pitch-dev

@i(file-start) @dash when to start reading the file (an offset from start). The
default is 0.

@i(file-end) @dash when to stop reading the file (an offset from end) the
default is 0

returns @dash a set of sound grains created from the input file

This is a granular
synthesis function based on the one by Roger B. Dannenberg. The
pitch of the grains will be based on the distribution you give to it. The
distribution must be passed in as a continuation.

(len-dist-granulate @i(filename) @i(dist ioi ioi-dev) @i(pitch-dev) [@i(file-start)@i(file-end)])

@i(filename) @dash name of the file

@i(grain-dur) @dash the duration of a grain

@i(grain-dev) @dash grain dur is actually grain-dur + random(0, grain-dev)

@i(ioi</span>) @dash the basic inter-onset-interval for grains

@i(ioi</span>-dev) @dash ioi is actually: ioi + random(0, ioi-dev)

@i(pitch-dist) @dash the distribution of the alteration in pitch to the grains. The
distribution values should be > 1.

@i(file-start) @dash when to start reading the file (an offset from start). The
default is 0

@i(file-end) @dash when to stop reading the file (an offset from end). The
default is 0

returns @dash a set of sound grains created from the input file

This is a granular
synthesis function based on the one by Roger B. Dannenberg. The
length of the grains will be based on the distribution you give to it. The
distribution must be passed in as a continuation.
@end(fndefs)
@end(comment)

@section(Score Generation and Manipulation)

A common application of pattern generators is to specify parameters 
for notes. (It should be understood that ``notes'' in this context
means any Nyquist behavior, whether it represents a conventional note,
an abstract sound object, or even some micro-sound event that is just
a low-level component of a hierarchical sound organization. Similarly,
``score'' should be taken to mean a specification for a 
sequence of these ``notes.'') 
The @code(score-gen) macro (defined by 
loading @code(xm.lsp)) establishes a convention for representing 
scores and for generating them using patterns.

The @code(timed-seq) macro, described in Section @ref(timed-seq-sec), 
already provides a way to represent a ``score'' as a list of expressions.
The Xmusic representation goes a bit further by specifying that
@i(all notes are specified by an alternation of keywords and values, where
some keywords have specific meanings and interpretations.)

The basic idea of @code(score-gen)@index(score-gen) is you provide a template for notes in 
a score as a set of keywords and values. For example,
@begin(example)
set pitch-pattern = make-cycle(list(c4, d4, e4, f4))
score-gen(dur: 0.4, name: quote(my-sound),
          pitch: next(pitch-pattern), score-len: 9)
@end(example)
generates a score of 9 notes as follows:
@begin(example)
((0 0 (SCORE-BEGIN-END 0 3.6))
 (0 0.4 (MY-SOUND :PITCH 60))
 (0.4 0.4 (MY-SOUND :PITCH 62))
 (0.8 0.4 (MY-SOUND :PITCH 64))
 (1.2 0.4 (MY-SOUND :PITCH 65))
 (1.6 0.4 (MY-SOUND :PITCH 60))
 (2 0.4 (MY-SOUND :PITCH 62))
 (2.4 0.4 (MY-SOUND :PITCH 64))
 (2.8 0.4 (MY-SOUND :PITCH 65))
 (3.2 0.4 (MY-SOUND :PITCH 60)))
@end(example)
The use of keywords like @code(:PITCH) helps to make scores
readable and easy to process without specific knowledge of 
about the functions called in the score. For example, one 
could write a transpose operation to transform all the 
@code(:pitch) parameters in a score without having to know 
that pitch is the first parameter of @code(pluck) and the
second parameter of @code(piano-note). Keyword parameters are
also used to give flexibility to note specification with
@code(score-gen). Since this approach requires the use of
keywords, the next section 
is a brief explanation of how to define functions that use
keyword parameters.

@subsection(Keyword Parameters)
@index(keyword parameters)
@index(parameters, keyword)
Keyword parameters are parameters whose presence is
indicated by a special symbol, called a keyword, followed
by the actual parameter. Keyword parameters in SAL have
default values that are used if no actual parameter is
provided by the caller of the function. (See Appendix
 @ref(xlisp-app) to learn about keywords in XLISP.)

To specify that a parameter is a keyword parameter, 
use a keyword symbol (one that ends in a colon) followed
by a default value.
  For example, here is a function that
accepts keyword parameters and invokes the @code(pluck) 
function:
@begin(example)
define function k-pluck(pitch: 60, dur: 1)
  return pluck(pitch, dur)
@end(example)
Now, we can call k-pluck with keyword parameters. The 
keywords are simply the formal parameter names with
a prepended colon character (@code(:pitch) and @code(:dur)
in this example), so a function call would look like:
@begin(example)
pluck(pitch: c3, dur: 3)
@end(example)
Usually, it is best to give keyword parameters useful
default values. That way, if a parameter such as @code(dur:)
is missing, a reasonable default value (1) can be used
automatically. 
It is never an error to omit a keyword parameter, but the
called function can check to see if a keyword parameter 
was supplied or not.
Because of default values, we can call 
@code[k-pluck(pitch: c3)] with no duration, 
@code[k-pluck(dur: 3)] with only a duration, 
or even @code[k-pluck()] with no parameters.

In XLISP, there is additional syntax to specify an alternate symbol
to be used as the keyword and to allow the called function
to determine whether or not a keyword parameter was 
supplied, but these features are little-used. See the XLISP
manual for details.

@subsection(Using score-gen)@pragma(defn)@index(score-gen)
The @code(score-gen) macro computes a score based on keyword parameters.
Some keywords have a special meaning, while others are not interpreted
but merely placed in the score. The resulting score can be synthesized
using @code(timed-seq) (see Section @ref(timed-seq-sec)).

The form of a call to @code(score-gen) is simply:
@begin(fndefs)
@codef[score-gen(@pragma(defn)@index(score-gen)@i(k1:) @i(e1), @i(k2:) @i(e2), @r(...))] @c{[sal]}@*
@altdef{@code[(score-gen @i(:k1) @i(e1) @i(:k2) @i(e2) @r(...))] @c{[lisp]}}@\where the @i(k)'s 
are keywords and the @i(e)'s are 
expressions. A score is generated by evaluating the expressions once for 
each note and constructing a list of keyword-value pairs. A number
of keywords have special interpretations. The rules for interpreting
these parameters will be explained through a set of "How do I ..." 
questions below.
@end(fndefs)

@i(How many notes will be generated?) The keyword 
parameter @code(score-len:) specifies an upper bound on the number
of notes. (Note: in LISP syntax, keywords
are always @i(preceded) by colons, so you would write
@code(:score-len) instead.) The keyword @code(score-dur:) specifies an upper bound
on the starting time of the last note in the score. (To be more
precise, the @code(score-dur:) bound is reached when the 
default starting time of the next note is greater than or equal
to the @code(score-dur:) value. This definition is necessary because
note times are not strictly increasing.) When either bound
is reached, score generation ends. At least one of these two
parameters must be specified or an error is raised. These keyword
parameters are evaluated just once and are not copied into the
parameter lists of generated notes.

@i(What is the duration of generated notes?) The 
keyword @code(dur:) defaults to 1 and specifies the nominal duration
in seconds. Since the generated note list is compatible with 
@code(timed-seq), the starting time and duration (to be precise, the
@i(stretch factor)) are not passed as parameters to the notes. Instead,
they control the Nyquist environment in which the note will be evaluated.

@i(What is the start time of a note?) The default start time of the
first note is zero. Given a note, the default start time of the next note is 
the start time plus the inter-onset time, which is given by the @code(ioi:)
parameter. If no @code(ioi:) parameter is specified, the inter-onset time
defaults to the duration, given by @code(dur:). In all cases, the default
start time of a note can be overridden by the keyword parameter @code(time:).

@i(When does the score begin and end?) The behavior @code[SCORE-BEGIN-END] 
contains the beginning and ending of the
score (these are used for score manipulations, e.g. when scores are merged,
their begin times can be aligned.) When @code(timed-seq) is used to 
synthesize a score, the @code(SCORE-BEGIN-END) marker is
not evaluated. The @code(score-gen) macro inserts a ``note'' of the form
@code[(0 0 (SCORE-BEGIN-END @i(begin-time) @i(end-time)))] 
at the time given by the @code(begin:) keyword, with @i(begin-time) and 
@i(end-time) determined by the @code(begin:) and @code(end:) 
keyword parameters, respectively. If the @i(begin:) keyword is not 
provided, the score begins at zero. If the @code(end:) keyword
is not provided, the score ends at the default start time
of what would be the next note after the last note in the score 
(as described in the previous paragraph). Note: if @code(time:) is used to 
compute note starting times, and these times are not increasing, it is
strongly advised to use @code(end:) to specify an end time for the score,
because the default end time may be anywhere in the middle of the 
generated sequence.

@i(What function is called to synthesize the note?) The @code(name:) 
parameter names the function. Like other parameters, the value can be any
expression, including something like @code[next(fn-name-pattern)],
allowing function names to be recomputed for each note. The default value 
is @code(note).

@i(Can I make parameters depend upon the starting time or the duration
of the note?) Parameter expressions can use the variable @code(sg:start) 
to access the start time of the note, @code(sg:ioi) to access the 
inter-onset time, and @code(sg:dur) to access the 
duration (stretch factor) of the note. Also, @code(sg:count) counts how 
many notes have been computed so far, starting at 0. The order of 
computation is: @code(sg:start) first, then @code(sg:ioi) and @code(sg:dur),
so for example, an expression to compute @code(sg:dur) can 
depend on @code(sg:ioi).

@i(Can parameters depend on each other?) The keyword @code(pre:) 
introduces an expression that is evaluated before each note, and 
@code(post:) provides an expression to be evaluated after each note. 
The @code(pre:) expression can assign one or more global variables 
which are then used in one or more expressions for parameters.

@i(How do I debug @code(score-gen) expressions?) You can set the 
@code(trace:) parameter to true (@code(t)) to enable a print statement
for each generated note.

@i(How can I save scores generated by @code(score-gen) that I like?) If the
keyword parameter @code(save:) is set to a symbol, the global variable
named by the symbol is set to the value of the generated sequence. Of 
course, the value returned by @code(score-gen) is just an ordinary list that
can be saved like any other value.

In summary, the following keywords have special interpretations 
in @code(score-gen): 
@code(begin:), @code(end:), @code(time:), @code(dur:), @code(name:), 
@code(ioi:), @code(trace:),
@code(save:), @code(score-len:), @code(score-dur:), @code(pre:), @code(post:).
 All other keyword
parameters are expressions that are evaluated once for each note
and become the parameters of the notes.

@subsection(Score Manipulation)
@index(score manipulation)@index(manipulation of scores)
Nyquist encourages the representation of music as 
executable programs, or @i(behaviors), and there are various
ways to modify behaviors, including time stretching, 
transposition, etc. An alternative to composing executable
programs is to manipulate scores as editable data. Each 
approach has its strengths and weaknesses. This section 
describes functions intended to manipulate Xmusic scores
as generated by, or at least in the form generated by, 
@code(score-gen). Recall that this means scores are lists
of events (e.g. notes), where events are three-element lists of the form
(@i(time) @i(duration) @i(expression), and where @i(expression)
is a standard lisp function call where all parameters are
keyword parameters. In addition, the first ``note'' may be
the special @code(SCORE-BEGIN-END) expression. If this is 
missing, the score begins at zero and ends at the end of the
last note.

For convenience, a set of functions is offered to access properties
of events (or notes) in scores. Although lisp functions such as 
@code(car), @code(cadr), and @code(caddr) can be used, code is more
readable when more mnemonic functions are used to access events.

@begin(fndefs)
@codef[event-time(@pragma(defn)@index(event-time)@i(event))] @c{[sal]}@*
@altdef{@code[(event-time @i(event))] @c{[lisp]}}@\Retrieve the time field from 
an event.

@codef[event-set-time(@pragma(defn)@index(event-set-time)@i(event), @i(time))] @c{[sal]}@*
@altdef{@code[(event-set-time @i(event) @i(time))] @c{[lisp]}}@\Construct
a new event where the time of @i(event) is replaced by @i(time).

@codef[event-dur(@pragma(defn)@index(event-dur)@i(event))] @c{[sal]}@*
@altdef{@code[(event-dur @i(event))] @c{[lisp]}}@\Retrieve the duration 
(i.e. the stretch factor) field from an event.

@codef[event-set-dur(@pragma(defn)@index(event-set-dur)@i(event), @i(dur))] @c{[sal]}@*
@altdef{@code[(event-set-dur @i(event) @i(dur))] @c{[lisp]}}@\Construct
a new event where the duration (or stretch factor) of @i(event) is
replaced by @i(dur).

@codef[event-expression(@pragma(defn)@index(event-expression)@i(event))] @c{[sal]}@*
@altdef{@code[(event-expression @i(event))] @c{[lisp]}}@\Retrieve the expression 
field from an event.

@codef[event-set-expression(@pragma(defn)@index(event-set-expression)@i(event), 
@i(dur))] @c{[sal]}@*
@altdef{@code[(event-set-expression @i(event) @i(dur))] @c{[lisp]}}@\Construct
a new event where the expression of @i(event) is replaced by @i(expression).

@codef[event-end(@pragma(defn)@index(event-end)@i(event))] @c{[sal]}@*
@altdef{@code[(event-end @i(event))] @c{[lisp]}}@\Retrieve the end time 
of @i(event), its time plus its duration.

@codef[expr-has-attr(@pragma(defn)@index(expr-has-attr)@i(expression), @i(attribute))] @c{[sal]}@*
@altdef{@code[(expr-has-attr @i(expression) @i(attribute))] @c{[lisp]}}@\Test 
whether a score event @i(expression) has the given @i(attribute).

@codef{expr-get-attr(@pragma(defn)@index(expr-get-attr)@i(expression), @i(attribute) [, @i(default)])} @c{[sal]}@*
@altdef{@code{(expr-get-attr @i(expression) @i(attribute) [@i(default)])} @c{[lisp]}}@\Get the value of the given @i(attribute) from a score event
@i(expression). If @i(attribute) is not present, return @i(default) if
specified, and otherwise @code(nil).

@codef{expr-set-attr(@pragma(defn)@index(expr-set-attr)@i(expr), @i(attribute), @i(value))} @c{[sal]}@*
@altdef{@code[(expr-set-attr @i(expr) @i(attribute) @i(value))] @c{[lisp]}}@\Construct a new expression identical to @i(expr) except that the @i(attribute) has @i(value).

@codef[event-has-attr(@pragma(defn)@index(event-has-attr)@i(event), @i(attribute))] @c{[sal]}@*
@altdef{@code[(event-has-attr @i(event) @i(attribute))] @c{[lisp]}}@\Test 
whether a given score @i(event)'s expression has the given @i(attribute).

@codef{event-get-attr(@pragma(defn)@index(event-get-attr)@i(event), @i(attribute), 
[@i(default)])} @c{[sal]}@*
@altdef{@code{(event-get-attr @i(event) @i(attribute) [@i(default)])} @c{[lisp]}}@\Get the value of the given @i(attribute) from a score 
@i(event)'s expression. If @i(attribute) is not present, return @i(default) if
specified, and otherwise @code(nil).

@codef{event-set-attr(@pragma(defn)@index(event-set-attr)@i(event), @i(attribute), @i(value))} @c{[sal]}@*
@altdef{@code[(event-set-attr @i(event) @i(attribute) @i(value))] @c{[lisp]}}@\Construct a new event identical to @i(event) except that the @i(attribute) has @i(value).

@end(fndefs)

Functions are provided to shift the starting times of notes,
stretch times and durations, stretch only durations,
add an offset to a keyword parameter, scale a keyword parameter, and
other manipulations. Functions are also provided to extract 
ranges of notes, notes that match criteria, and to combine scores.
Most of these functions (listed below in detail)
share a set of keyword parameters that optionally limit the range over which 
the transformation operates. The @code(from-index:) and @code(to-index:) 
parameters specify the index of the first note and the index of the
last note to be changed. If these numbers are negative, they are offsets 
from the end of the score, e.g. -1 denotes the last note of the score. The
@code(from-time:) and @code(to-time:) indicate a range of starting times
of notes that will be affected by the manipulation. Only notes whose time
is greater than or equal to the @i(from-time) and @i(strictly less than)
 the @i(to-time) are modified. If both index and time ranges are specified,
only notes that satisfy @i(both) constraints are selected. (Note: in
LISP syntax, colons @i(precede) the keyword, so use
@code(:from-index), @code(:to-index), @code(:from-time), and @code(:to-time).)

@begin(fndefs)
@codef[score-sorted(@pragma(defn)@index(score-sorted)@i(score))] @c{[sal]}@*
@altdef{@code[(score-sorted @i(score))] @c{[lisp]}}@\Test if @i(score) is sorted.

@codef{score-sort(@pragma(defn)@index(score-sort)@i(score) [, @i(copy-flag)])} @c{[sal]}@*
@altdef{@code{(score-sort @i(score) [@i(copy-flag)])} @c{[lisp]}}@\Sort
 the notes in a 
score into start-time order. If copy-flag is nil, this is a destructive
operation which should only be performed if the top-level score list
is a fresh copy that is not shared by any other variables. (The 
@i(copy-flag) is intended for internal system use only.)
 For the following operations, it is assumed
that scores are sorted, and all operations return a sorted score.

@codef{score-shift(@pragma(defn)@index(score-shift)@i(score), @i(offset), from-index: @i(i), to-index: @i(j), from-time: @i(x),
 to-time: @i(y))} @c{[sal]}@*
@altdef{@code{(score-shift @i(score) @i(offset)
 :from-index @i(i) :to-index @i(j) :from-time @i(x)
 :to-time @i(y))} @c{[lisp]}}@\Add a constant 
@i(offset) to the starting time of a set of notes in @i(score). By default,
all notes are modified, but the range of notes can be limited with the
keyword parameters. The begin time of the score is not changed, but the 
end time is increased by @i(offset).
The original score is not modified, and a new score is returned.

@codef{score-stretch(@pragma(defn)@index(score-stretch)@i(score), @i(factor), dur: @i(dur-flag), time: @i(time-flag), from-index: @i(i),
 to-index: @i(j), from-time: @i(x), to-time: @i(y))} @c{[sal]}@*
@altdef{@code{(score-stretch @i(score) @i(factor)
 :dur @i(dur-flag) :time @i(time-flag) :from-index @i(i)
 :to-index @i(j) :from-time @i(x) :to-time @i(y))} @c{[lisp]}}@\Stretch  
note times and durations by @i(factor). The default @i(dur-flag) is 
non-null, but if @i(dur-flag) is null, the original durations are retained
and only times are stretched. Similarly, the default @i(time-flag) is 
non-null, but if @i(time-flag) is null, the original times are retained
and only durations are stretched. If both @i(dur-flag) and @i(time-flag)
are null, the score is not changed. If a range
of notes is specified, times are scaled within that range, and 
notes after the range are shifted so that the stretched region does not
create a "hole" or overlap with notes that follow. If the range begins
or ends with a time (via @code(from-time:) and @code(to-time:)), time
stretching
takes place over the indicated time interval independent of whether 
any notes are present or where they start. In other words, the 
``rests'' are stretched along with the notes.
The original score is not modified, and a new score is returned.

@codef{score-transpose(@pragma(defn)@index(score-transpose)@i(score),
 @i(keyword),  @i(amount), from-index: @i(i), to-index: @i(j),
 from-time: @i(x), to-time: @i(y))} @c{[sal]}@*
@altdef{@code{(score-transpose @i(score)
 @i(keyword) @i(amount) :from-index @i(i) :to-index @i(j)
 :from-time @i(x) :to-time @i(y))} @c{[lisp]}}@\For 
 each note in the score and in any indicated range, if there is a keyword
 parameter matching @i(keyword) and the
parameter value is a number, increment
the parameter value by @i(amount). For example, to tranpose up by a whole
step, write @code[(score-transpose 2 :pitch @i(score))]. The 
original score is not modified, and a new score 
is returned.

@codef{score-scale(@pragma(defn)@index(score-scale)@i(score), @i(keyword), @i(amount), from-index: @i(i), to-index: @i(j), from-time: @i(x),
 to-time: @i(y))} @c{[sal]}@*
@altdef{@code{(score-scale @i(score) @i(keyword) @i(amount)
 :from-index @i(i) :to-index @i(j) :from-time @i(x)
 :to-time @i(y))} @c{[lisp]}}@\For each note
in the score and in any indicated range, if there is a keyword 
parameter matching @i(keyword) and the
parameter value is a number, multiply
the parameter value by @i(amount). The original score is not modified, 
and a new score is returned.

@codef{score-sustain(@pragma(defn)@index(score-sustain)@i(score), @i(factor), from-index: @i(i), to-index: @i(j), from-time: @i(x),
 to-time: @i(y))} @c{[sal]}@*
@altdef{@code{(score-sustain @i(score) @i(factor)
 :from-index @i(i) :to-index @i(j) :from-time @i(x)
 :to-time @i(y))} @c{[lisp]}}@\For each note
in the score and in any indicated range, multiply
the duration (stretch factor) by @i(amount). This can be used to
make notes sound more legato or staccato, and does not change their 
starting times. The original score is not modified, and 
a new score is returned.

@codef{score-voice(@pragma(defn)@index(score-voice)@i(score),
 @i(replacement-list), from-index: @i(i), to-index: @i(j),
 from-time: @i(x), to-time: @i(y))} @c{[sal]}@*
@altdef{@code{(score-voice @i(score)
 @i(replacement-list) :from-index @i(i) :to-index @i(j)
 :from-time @i(x) :to-time @i(y))} @c{[lisp]}}@\For each note
in the score and in any indicated range, replace the behavior (function)
name using @i(replacement-list), which has the format: 
@code[((@i(old1 new1)) (@i(old2 new2)) @r(...))], where @i(oldi) indicates
a current behavior name and @i(newi) is the replacement. If @i(oldi) 
is @code(*), it matches anything.  For example, to 
replace @code(my-note-1) by @code(trombone) and @code(my-note-2) by
@code(horn), use @code[score-voice(@i(score), {{my-note-1 trombone}
{my-note-2 horn}})]. To replace all instruments with 
@code(piano), use @code[score-voice(@i(score), {{* piano}})]. 
The original score is not modified, and a
 new score is returned.

@codef{score-merge(@pragma(defn)@index(score-merge)@i(score1), @i(score2), @r(...))} @c{[sal]}@*
@altdef{@code[(score-merge @i(score1) @i(score2) @r(...))] @c{[lisp]}}@\Create
a new score containing all the notes of the parameters, which are all
scores. The resulting notes retain their original times and durations. The
merged score begin time is the minimum of the begin times of the parameters
and the merged score end time is the maximum of the end times of 
the parameters. The original scores are not modified, and a new 
score is returned.

@codef{score-append(@pragma(defn)@index(score-append)@i(score1), @i(score2), @r(...))} @c{[sal]}@*
@altdef{@code[(score-append @i(score1) @i(score2) @r(...))] @c{[lisp]}}@\Create
a new score containing all the notes of the parameters, which are all
scores. The begin time of the first score is unaltered. The begin time of
 each other score is aligned to the end time of the
previous score; thus, scores are ``spliced'' in sequence. The original 
scores are not modified, and a new score is returned.

@codef{score-select(@pragma(defn)@index(score-select)@index(score-filter)@i(score),
 @i(predicate), from-index: @i(i), to-index: @i(j), from-time: @i(x),
 to-time: @i(y), reject: @i(flag))} @c{[sal]}@*
@altdef{@code{(score-select @i(score)
 @i(predicate) :from-index @i(i) :to-index @i(j) :from-time @i(x)
 :to-time @i(y) :reject @i(flag))} @c{[lisp]}}@\Select (or reject)
notes to form a new score. Notes are selected if they fall into the
given ranges of index and time @i(and) they satisfy @i(predicate), a function
of three parameters that is applied to the start time, duration, and the 
expression of the note. Alternatively, @i(predicate) may be @code(t), 
indicating that all notes in range are to be selected. 
The selected notes along with the existing score begin and end markers, are combined to form a new score. Alternatively, if
the @code(reject:) parameter is non-null, the notes @i(not) selected form
 the new score (in other words the selected notes are rejected or removed to
 form the new score). The original score is not modified, and a
 new score is returned.

@codef{score-set-begin(@pragma(defn)@index(score-set-begin)@i(score), @i(time))} @c{[sal]}@*
@altdef{@code[(score-set-begin @i(score) @i(time))] @c{[lisp]}}@\The begin 
time
 from the @i(score)'s @code(SCORE-BEGIN-END) marker is set to @i(time). The 
original score is not modified, and a new score is returned.

@codef{score-get-begin(@pragma(defn)@index(score-get-begin)@i(score))} @c{[sal]}@*
@altdef{@code[(score-get-begin @i(score))] @c{[lisp]}}@\Return the begin
time of the @i(score).

@codef{score-set-end(@pragma(defn)@index(score-set-end)@i(score), @i(time))} @c{[sal]}@*
@altdef{@code[(score-set-end @i(score) @i(time))] @c{[lisp]}}@\The end time
 from the @i(score)'s @code(SCORE-BEGIN-END) marker is set to @i(time). The 
original score is not modified, and a new score is returned.

@codef{score-get-end(@pragma(defn)@index(score-get-end)@i(score))} @c{[sal]}@*
@altdef{@code[(score-get-end @i(score))] @c{[lisp]}}@\Return the end
time of the @i(score).

@codef{score-must-have-begin-end(@pragma(defn)@index(score-must-have-begin-end)@i(score))} @c{[sal]}@*
@altdef{@code[(score-must-have-begin-end @i(score))] @c{[lisp]}}@\If 
 @i(score) does not have a begin and end time, construct a score with a
 @code(SCORE-BEGIN-END) expression and return it. If score already has a begin
and end time, just return the score. The orignal score is not modified.

@codef{score-filter-length(@pragma(defn)@index(score-filter-length)@i(score), 
@i(cutoff))} @c{[sal]}@*
@altdef{@code[(score-filter-length @i(score) @i(cutoff))] @c{[lisp]}}@\Remove notes that extend beyond the @i(cutoff) time. This
is similar to @code(score-select), but the here, events are removed when
their nominal ending time (start time plus duration) exceeds the @i(cutoff),
whereas the @code(to-time:) parameter is compared to the note's start time.
The original score is not modified, and a new score is returned.

@codef{score-repeat(@pragma(defn)@index(score-repeat)@i(score), @i(n))} @c{[sal]}@*
@altdef{@code[(score-repeat @i(score) @i(n))] @c{[lisp]}}@\Make a sequence
of @i(n) copies of @i(score). Each copy is shifted to that it's begin
time aligns with the end time of the previous copy, as in @code(score-append).
The original score is not modified, and a new score is returned.

@codef{score-stretch-to-length(@pragma(defn)@index(score-stretch-to-length)@i(score), 
@i(length))} @c{[sal]}@*
@altdef{@code[(score-stretch-to-length @i(score) @i(length))] @c{[lisp]}}@\Stretch the score so that the end time of the score is
the score's begin time plus @i(length). 
The original score is not modified, and a new score is returned.

@codef{score-filter-overlap(@pragma(defn)@index(score-filter-overlap)@i(score))} @c{[sal]}@*
@altdef{@code[(score-filter-overlap @i(score))] @c{[lisp]}}@\Remove
overlapping notes (based on the note start time and duration), giving
priority to the
positional order within the note list (which is also time order). 
The original score is not modified, 
and a new score is returned.

@codef{score-print(@pragma(defn)@index(score-print)@i(score))} @c{[sal]}@*
@altdef{@code[(score-print @i(score))] @c{[lisp]}}@\Print a score with
one note per line. Returns @code(nil).

@codef{score-play(@pragma(defn)@index(score-play)@i(score))} @c{[sal]}@*
@altdef{@code[(score-play @i(score))] @c{[lisp]}}@\Play @i(score)
using @code(timed-seq) to convert the score to a sound, and 
 @code(play) to play the sound.

@codef{score-adjacent-events(@pragma(defn)@index(score-adjacent-events)@i(score),
 @i(function),
 from-index: @i(i), to-index: @i(j), 
 from-time: @i(x), to-time: @i(y))} @c{[sal]}@*
@altdef{@code{(score-adjacent-events @i(score) @i(function) :from-index @i(i) :to-index @i(j) :from-time @i(x) :to-time @i(y))} @c{[lisp]}}@\Call
 @code[(@i(function) @i(A) @i(B) @i(C))], where
@i(A), @i(B), and @i(C) are consecutive notes in the score. The result
replaces @i(B). If the result is @code(nil), @i(B) is deleted, and the
next call will be @code[(@i(function A C D))], etc. The first call is
to @code[(@i(function) nil @i(A B))] and the last is to 
@code[(@i(function) @i(Y Z) nil)]. If there is just one note in the
score, @code[(@i(function) nil @i(A) nil)] is called. Function calls 
are not made if the note is outside of the indicated range.
This function 
allows notes and their parameters to be adjusted according to their 
immediate context. The original score is not modified, 
and a new score is returned.

@codef{score-apply(@pragma(defn)@index(score-apply)@i(score), @i(function),
 from-index: @i(i), to-index: @i(j), from-time: @i(x), to-time: @i(y))} 
@c{[sal]}@*
@altdef{@code{(score-apply @i(score) @i(function) :from-index @i(i) :to-index @i(j) :from-time @i(x) :to-time @i(y))} @c{[lisp]}}@\Replace
each note in the score with the result of
 @code[(@i(function time dur expression))] (in Lisp) or
 @code[@i(function)(@i(time), @i(dur), @i(expression))] (in SAL), 
where @i(time), @i(dur), 
and @i(expression) are the time, duration, and expression of the note.
If a range is indicated, only notes in the range are replaced.
The original score is not modified, and a new score is returned.

@codef{score-indexof(@pragma(defn)@index(score-indexof)@i(score), @i(function),
 from-index: @i(i), to-index: @i(j), from-time: @i(x), to-time: @i(y))} @c{[sal]}@*
@altdef{@code{(score-indexof @i(score) @i(function) :from-index @i(i) :to-index @i(j) :from-time @i(x) :to-time @i(y))} @c{[lisp]}}@\Return the index (position)
of the first score event (in range) for which applying @i(function) 
using @code[(@i(function time dur expression))] returns true.


@codef{score-last-indexof(@pragma(defn)@index(score-last-indexof)@i(score),
 @i(function), from-index: @i(i), to-index: @i(j), from-time: @i(x), 
to-time: @i(y))} @c{[sal]}@*
@altdef{@code{(score-last-indexof @i(score) @i(function)
 :from-index @i(i) :to-index @i(j) :from-time @i(x) :to-time @i(y))} @c{[lisp]}}@\Return the index (position)
of the last score event (in range) for which applying @i(function) 
using @code[(@i(function time dur expression))] returns true.

@codef{score-randomize-start(@pragma(defn)@index(score-randomize-start)@index(jitter)@index(feel factor)@index(offset)@i(score), @i(amt), from-index: @i(i), to-index: @i(j), from-time: @i(x),
 to-time: @i(y))} @c{[sal]}@*
@altdef{@code{(score-randomize-start @i(score) @i(amt)
 :from-index @i(i) :to-index @i(j) :from-time @i(x)
 :to-time @i(y))} @c{[lisp]}}@\Alter the start times of notes by a 
random amount up to plus or minus @i(amt).
The original score is not modified, and a new score is returned.
@end(fndefs)

@subsection(Xmusic and Standard MIDI Files)
@index(MIDI file)@index(Standard MIDI File)
Nyquist has a general facility to read and write MIDI files. 
You can even translate to and from a text representation, as described
in Chapter @ref(adagio-chap). It is also useful sometimes to read notes
from Standard MIDI Files into Xmusic scores and vice versa. At present,
Xmusic only translates notes, ignoring the various controls, program
changes, pitch bends, and other messages.

MIDI notes are translated to Xmusic score events as follows:
@begin(display)
@code[(@i(time) @i(dur) (NOTE :chan @i(channel) :pitch @i(keynum) :vel @i(velocity)))],
@end(display)
where @i(channel), @i(keynum), and @i(velocity) come directly 
from the MIDI message (channels are numbered starting from zero).
Note also that note-off messages are implied by the stretch factor 
@i(dur) which is duration in seconds.

@begin(fndefs)
@codef{score-read-smf(@pragma(defn)@index(score-read-smf)@index(midi file)@i(filename))} @c{[sal]}@*
@altdef{@code[(score-read-smf @i(filename))] @c{[lisp]}}@\Read a 
standard MIDI file from @i(filename). Return an Xmusic score, or @code(nil)
if the file could not be opened. The
start time is zero, and the end time is the maximum end time of all
notes. A very limited interface is offered to extract MIDI program numbers
from the file: The global variable @code(*rslt*) is set to a list of MIDI
program numbers for each channel. E.g. if @code(*rslt*) is @code[(0 20 77)], 
then program for channel 0 is 0, for channel 1 is 20, and for channel 2 is 77. 
Program changes were not found on other channels. The default program number is
0, so in this example, it is not known whether the program 0 on channel 0 
is the result of a real MIDI program change command or just a default value. 
If more than one program change exists on a channel, the @i[last] program 
number is recorded and returned, so this information will only be completely
correct when the MIDI file sends single program change per channel before
any notes are played. This, however, is a fairly common practice. Note that 
the list returned as @code(*rslt*) can be passed 
to @code(score-write-smf), described below.

@codef{score-write-smf(@pragma(defn)@index(score-write-smf)@index(midi file)@i(score), @i(filename),
[@i(programs)])} @c{[sal]}@*
@altdef{@code[(score-write-smf @i(score) @i(filename) @i(programs))] @c{[lisp]}}@\Write a standard MIDI file to @i(filename) 
with notes in @i(score). In this function,
@i(every) event in the score with a @code(pitch:) attribute, regardless of the
``instrument'' (or function name), generates a
MIDI note, using the @code(chan:) attribute for the channel (default 0) and
the @code(vel:) attribute for velocity (default 100). There is no facility
(in the current implementation) to issue control changes, but to allow
different instruments, MIDI programs may be set in two ways. The simplest is
to associate programs with channels using
the optional @i[programs] parameter, which is simply a list of up to 16 MIDI
program numbers. Corresponding program change commands are added to the 
beginning of the MIDI file. If @i[programs] has less than 16 elements, program
change commands are only sent on the first @i[n] channels. The second way to 
issue MIDI program changes is to add a @code(program:) keyword parameter to 
a note in the score. Typically, the note will have a @code(pitch:) of 
@code(nil) so that no actual MIDI note-on message is generated. If program 
changes and notes have the same starting times, their relative playback
order is undefined, and the note may be cut off by an immediately 
following program change. Therefore, program changes should occur slightly,
e.g. 1 ms, before any notes. @i(Program numbers and channels are numbered
starting at zero, matching the internal MIDI representation. This may be
one less than displayed on MIDI hardware, sequencers, etc.)
@end(fndefs)

@subsection(Workspaces)
@label(workspaces-sec)
@index(workspace)
When working with scores, you may find it necessary to save 
them in files between work sessions. This is not an issue
with functions because they are 
normally edited in files and loaded from them. In contrast,
scores are created as Lisp data, and unless you take care to
save them, they will be destroyed when you exit the Nyquist 
program.

A simple mechanism called a workspace has been created
to manage scores (and any other Lisp data, for that matter).
A workspace is just a set of lisp global variables. These
variables are stored in the file @code(workspace.lsp). 
For simplicity, there is only one workspace, and no backups
or versions are maintained, but the user is free to make
backups and copies of @code(workspace.lsp).
To help remember what each variable is for, you can also
associate and retrieve a text string with each variable.
The following functions manage workspaces.

In addition, when a workspace is loaded, you can request that
functions be called. For example, the workspace might store
descriptions of a graphical interface. When the workspace is
loaded, a function might run to convert saved data into a 
graphical interface. (This is how sliders are saved by the IDE.)

@begin(fndefs)
@codef[add-to-workspace(@pragma(defn)@index(add-to-workspace)@i(symbol))] @c{[sal]}@*
@altdef{@code[(add-to-workspace @i(symbol))] @c{[lisp]}}@\Adds 
a global variable to the workspace. The @i(symbol) should be a (quoted)
symbol.

@codef[save-workspace(@pragma(defn)@index(save-workspace))] @c{[sal]}@*
@altdef{@code{(save-workspace)} @c{[lisp]}}@\All global variables
in the workspace are saved to @code(workspace.lsp) (in the current
directory), overwriting the previous file.

@codef{describe(@pragma(defn)@index(describe)@i(symbol) [, @i(description)])}
 @c{[sal]}@*
@altdef{@code[(describe @i(symbol) [@i(description)])] @c{[lisp]}}@\If @i(description), a text string, is present, 
associate @i(description) with the variable named by the
@i(symbol). If @i(symbol) is not already in the workspace, 
it is added. If @i(description) is omitted, the function returns
the current description (from a previous call) for @i(symbol).

@codef{add-action-to-workspace(@pragma(defn)@index(add-action-to-workspace)@i(symbol))} @c{[sal]}@*
@altdef{@code[(add-action-to-workspace @i(symbol))] @c{[lisp]}}@\Requests that the function named by @i(symbol) be
called when the workspace is loaded (if the function is defined).
@end(fndefs)

To restore a workspace, use the command @code[load "workspace"]. This restores
the values of the workspace variables to the values they had when
@code(save-workspace) was last called. It also restores the documentation
strings, if set, by @code(describe). If you load two or more
@code(workspace.lsp) files, the variables will be merged into a
single workspace. The current set of workspace variables are saved in
the list @code(*workspace*). To clear the workspace, set @code(*workspace*) 
to @code(nil). This does not delete any variables, but means that
no variables will be saved by @code(save-workspace) until variables are
added again.

Functions to be called are saved in the list @code(*workspace-actions*).
to clear the functions, set @code(*workspace-actions*) to @code(nil).
Restore functions to the list with @code(add-action-to-workspace).

@subsection(Utility Functions)
This chapter concludes with details of various utility functions for score 
manipulation.

@begin(fndefs)
@codef[patternp(@pragma(defn)@index(patternp)@i(expression))] @c{[sal]}@*
@altdef{@code[(patternp @i(expression))] @c{[lisp]}}@\Test if @i(expression) is 
an Xmusic pattern.

@codef[params-transpose(@pragma(defn)@index(params-transpose)@i(params), @i(keyword), 
 @i(amount))] @c{[sal]}@*
@altdef{@code[(params-transpose @i(params) @i(keyword) @i(amount))] @c{[lisp]}}@\Add a transposition amount to a score event parameter. The 
@i(params)
parameter is a list of keyword/value pairs (not preceded by a function name). 
The @i(keyword) is the keyword of the value to be altered, and @i(amount)
is a number to be added to the value. If no matching keyword is present 
in @i(params), then @i(params) is returned. Otherwise, a new parameter 
list is constructed and returned. The original @i(params) is not changed.

@codef[params-scale(@pragma(defn)@index(params-scale)@i(params), @i(keyword),
 @i(amount))] @c{[sal]}@*
@altdef{@code[(params-scale @i(params) @i(keyword) @i(amount))] @c{[lisp]}}@\Scale a score event parameter by some factor. This is like 
 @code(params-transpose), only using multiplication. The @i(params) 
list is a list of 
keyword/value pairs, @i(keyword) is the parameter keyword, 
and @i(amount) is the scale factor. 

@codef[interpolate(@pragma(defn)@index(interpolate)@index(linear interpolation)@i(x), @i(x1), @i(y1), @i(x2), @i(y2))] @c{[sal]}@*
@altdef{@code[(interpolate @i(x) @i(x1) @i(y1) @i(x2) @i(y2))] @c{[lisp]}}@\Linearly interpolate (or extrapolate)
 between points
(@i(x1), @i(y1)) and (@i(x2), @i(y2)) to compute the y value
 corresponding to @i(x).

@codef[intersection(@pragma(defn)@index(intersection)@index(set intersection)@i(a),
 @i(b))] @c{[sal]}@*
@altdef{@code[(intersection @i(a) @i(b))] @c{[lisp]}}@\Compute the set intersection of lists @i(a) and @i(b).

@codef[union(@pragma(defn)@index(union)@index(set union)@i(a), @i(b))] @c{[sal]}@*
@altdef{@code[(union @i(a) @i(b))] @c{[lisp]}}@\Compute 
the set union of lists @i(a) and @i(b).

@codef[set-difference(@index(difference)@pragma(defn)@index(set-difference)@i(a),
 @i(b))] @c{[sal]}@*
@altdef{@code[(set-difference @i(a) @i(b))] @c{[lisp]}}@\Compute the set of all elements that are in @i(a) but not in @i(b).

@codef[subsetp(@pragma(defn)@index(subsetp)@index(subset)@i(a), @i(b))] @c{[sal]}@*
@altdef{@code[(subsetp @i(a) @i(b))] @c{[lisp]}}@\Returns true iff
@i(a) is a subset of @i(b), that is, each element of @i(a) is a member 
of @i(b).
@end(fndefs)

@chapter(Nyquist Libraries)
@index(libraries)
Nyquist is always growing with new functions. Functions that are most fundamental
are added to the core language. These functions are automatically loaded when you 
start Nyquist, and they are documented in the preceding chapters. Other functions seem
less central and are implemented as lisp files that you can load. These are called
library functions, and they are described here. 

To use a library function, you
must first load the library, e.g. @code[(load "pianosyn")] loads the piano synthesis
library. The libraries are all located in the @code(lib) directory, and you 
should therefore include this directory on your @code(XLISPPATH) variable. (See 
Section @ref(install-sec).) Each library is documented in one of the following
sections. When you load the library described by the section, all functions
documented in that section become available.

@section(Piano Synthesizer)
The piano synthesizer (library name is @code(pianosyn.lsp)) generates
realistic piano tones using a multiple wavetable implementation by Zheng (Geoffrey)
Hua and Jim Beauchamp, University of Illinois. Please see the notice about
acknowledgements that prints when you load the file. Further informations and
example code can be found in 
@code(demos/piano.htm)@index(demos, piano)@index(piano synthesizer tutorial). 
There are several useful functions in this library:
@begin(fndefs)
@codef[piano-note(@pragma(defn)@index(piano-note)@index(piano synthesizer)@i(duration), @i(step), 
 @i(dynamic))] @c{[sal]}@*
@altdef{@code[(piano-note @i(duration) @i(step) @i(dynamic))] @c{[lisp]}}@\Synthesizes a piano tone. @i(Duration) is the duration to the point of
key release, after which there is a rapid decay. @i(Step) is the pitch in half
steps, and @i(dynamic) is approximately equivalent to a MIDI key velocity
parameter. Use a value near 100 for a loud sound and near 10 for a soft sound.

@codef[piano-note-2(@pragma(defn)@index(piano-note-2)@i(step), @i(dynamic))] @c{[sal]}@*
@altdef{@code[(piano-note-2 @i(step) @i(dynamic))] @c{[lisp]}}@\Similar to @code(piano-note) except the duration is nominally 1.0.

@codef[piano-midi(@pragma(defn)@index(piano-midi)@i(midi-file-name))] @c{[sal]}@*
@altdef{@code[(piano-midi @i(midi-file-name))] @c{[lisp]}}@\Use the piano synthesizer 
to play a MIDI file. The file name (a string) is given by @i(midi-file-name).

@codef[piano-midi2file(@pragma(defn)@index(piano-midi2file)@i(midi-file-name), 
@i(sound-file-name))] @c{[sal]}@*
@altdef{@code[(piano-midi2file @i(midi-file-name) @i(sound-file-name))] @c{[lisp]}}@\Use the piano synthesizer to play a MIDI file. The MIDI file
is given by @i(midi-file-name) and the (monophonic) result is written to the file
named @i(sound-file-name).
@end(fndefs)

@section(Dymanics Compression)
To use these functions, load the file @code(compress.lsp). This library 
implements a compressor originally intended for noisy speech audio, but
usable in a variety of situations.
There are actually two compressors that can be used in 
series. The first, @code(compress), is
a fairly standard one: it detects signal level with an RMS
detector and uses table-lookup to determine how much gain
to place on the original signal at that point. One bit of
cleverness here is that the RMS envelope is ``followed'' or
enveloped using @code(snd-follow), which does look-ahead to anticipate
peaks before they happen.

The other interesting feature is @code(compress-map), which builds 
a map in terms of compression and expansion. For speech, the recommended
procedure is to figure out the noise floor on the signal you are compressing
(for example, look at the signal where the speaker is not talking).
Use a compression map that leaves the noise alone and boosts
signals that are well above the noise floor. Alas, the @code(compress-map)
function is not written in these terms, so some head-scratching is
involved, but the results are quite good.

The second compressor is called @code(agc), and it implements automatic gain
control that keeps peaks at or below 1.0. By combining @code(compress) and 
@code(agc), you can process poorly recorded speech for playback on low-quality
speakers in noisy environments. The @code(compress) function modulates the 
short-term gain to to minimize the total dynamic range, keeping the speech at
a generally loud level, and the @code(agc) function rides the long-term gain
to set the overall level without clipping.

@begin(fndefs)
@codef{compress-map(@pragma(defn)@index(compress-map)@i(compress-ratio), 
@i(compress-threshold), 
@i(expand-ratio), @i(expand-ratio), limit: @i(limit), transition: 
@i(transition))} @c{[sal]}@*
@altdef{@code{(compress-map @i(compress-ratio) @i(compress-threshold)
 @i(expand-ratio) @i(expand-ratio) :limit @i(limit) :transition
 @i(transition)])} @c{[lisp]}}@\Construct
a map for the compress function. The map consists of two parts: a compression
part and an expansion part.
The intended use is to compress everything above compress-threshold by
compress-ratio, and to downward expand everything below expand-ratio
by expand-ratio.  Thresholds are in dB and ratios are dB-per-dB.
0dB corresponds to a peak amplitude of 1.0 or rms amplitude of 0.7
If the input goes above 0dB, the output can optionally be limited
by setting @code(limit:) (a keyword parameter) to @code(T). 
This effectively changes 
the compression ratio to infinity at 0dB.  If @code(limit:) is @code(nil)
(the default), then the compression-ratio continues to apply above 0dB.

Another keyword parameter, @code(transition:), sets the amount below the
thresholds (in dB) that a smooth transition starts. The default is 0,
meaning that there is no smooth transition. The smooth transition is a
2nd-order polynomial that matches the slopes of the straight-line compression
curve and interpolates between them.

It is assumed that expand-threshold <= compress-threshold <= 0
The gain is unity at 0dB so if compression-ratio > 1, then gain
will be greater than unity below 0dB.

The result returned by this function is a sound for use in the @code(shape)
function. The sound maps input
dB to gain. Time 1.0 corresponds to 0dB, time 0.0 corresponds to
-100 dB, and time 2.0 corresponds to +100dB, so this is a 
100hz ``sample rate'' sound. The sound gives gain in dB.

@codef[db-average(@pragma(defn)@index(db-average)@i(input))] @c{[sal]}@*
@altdef{@code[(db-average @i(input))] @c{[lisp]}}@\Compute the average amplitude 
of @i(input) in dB.

@codef{compress(@pragma(defn)@index(compress)@i(input), @i(map), @i(rise-time), @i(fall-time) [, @i(lookahead)])} @c{[sal]}@*
@altdef{@code[(compress @i(input) @i(map) @i(rise-time) @i(fall-time)
 [@i(lookahead)])] @c{[lisp]}}@\Compress 
@i(input) using @i(map), a compression curve
probably generated by @code(compress-map) (see above). Adjustments in gain have
the given @i(rise-time) and @i(fall-time). Lookahead tells how far ahead to look
at the signal, and is @i(rise-time) by default.

@codef{agc(@index(automatic gain control)@pragma(defn)@index(agc)@index(gain)@i(input),
@i(range), @i(rise-time), @i(fall-time) [, @i(lookahead)])} @c{[sal]}@*
@altdef{@code{(agc @i(input) @i(range) @i(rise-time) @i(fall-time)
 [@i(lookahead)])} @c{[lisp]}}@\An automatic
gain control applied to @i(input). The maximum gain in dB is @i(range). Peaks
are attenuated to 1.0, and gain is controlled with the given @i(rise-time) and
@i(fall-time). The look-ahead time default is @i(rise-time).
@end(fndefs)

@section(Clipping Softener)
This library, in @code(soften.lsp), was written to improve the quality of
poorly recorded speech. In recordings of speech, extreme clipping generates
harsh high frequency noise. This can sound particulary bad on small speakers
that will emphasize high frequencies. This problem can be ameliorated by 
low-pass filtering regions where clipping occurs. The effect is to dull the
harsh clipping. Intelligibility is not affected by much, and the result can
be much more pleasant on the ears. Clipping is detected simply by looking for
large signal values. Assuming 8-bit recording, this level is set to 126/127.

The function works by cross-fading between the normal signal and a filtered 
signal as opposed to changing filter coefficients.

@begin(fndefs)
@codef[soften-clipping(@pragma(defn)@index(soften-clipping)@index(clipping repair)@i(snd), 
@i(cutoff))] @c{[sal]}@*
@altdef{@code[(soften-clipping @i(snd) @i(cutoff))] @c{[lisp]}}@\Filter the loud regions of a signal where clipping is likely
to have generated additional high frequencies. The input signal is @i(snd)
and @i(cutoff) is the filter cutoff frequency
(4 kHz is recommended for speech).
@end(fndefs)

@section(Graphical Equalizer)
There's nothing really ``graphical'' about this library (@code(grapheq.lsp)), but 
this is a common term for multi-band equalizers. This implementation uses
Nyquist's @code(eq-band) function to split the incoming signal into different
frequency bands. Bands are spaced geometrically, e.g. each band could be one
octave, meaning that each successive band has twice the bandwidth. An interesting
possibility is using computed control functions to make the equalization change
over time.

@begin(fndefs)
@codef[nband-range(@pragma(defn)@index(nband-range)@index(graphical equalizer)@index(equalization)@i(input), @i(gains), @i(lowf), @i(highf))] @c{[sal]}@*
@altdef{@code[(nband-range @i(input) @i(gains) @i(lowf) @i(highf))] @c{[lisp]}}@\A graphical equalizer applied to 
@i(input) (a @code(SOUND)). The gain controls and number of bands is given by @i(gains), an
ARRAY of @code(SOUND)s (in other words, a Nyquist multichannel @code(SOUND)). Any sound in the
array may be replaced by a @code(FLONUM). The bands are
geometrically equally spaced from the lowest frequency @i(lowf) to the
highest frequency @i(highf) (both are @code(FLONUM)s).

@codef[nband(@pragma(defn)@index(nband)@i(input), @i(gains))] @c{[sal]}@*
@altdef{@code[(nband @i(input) @i(gains))] @c{[lisp]}}@\A graphical equalizer, identical 
to @code(nband-range) with a range of 20 to 20,000 Hz.
@end(fndefs)

@section(Sound Reversal)
The @code(reverse.lsp) library implements functions to play sounds in reverse.

@begin(fndefs)
@codef[s-reverse(@index(reverse, sound)@pragma(defn)@index(s-reverse)@index(backward)@index(play in reverse)@i(snd))] @c{[sal]}@*
@altdef{@code[(s-reverse @i(snd))] @c{[lisp]}}@\Reverses @i(snd) (a @code(SOUND)). Sound must be shorter
than @code(*max-reverse-samples*), which is currently initialized to 
25 million samples. Reversal allocates about 4 bytes per sample. This function
uses XLISP in the inner sample loop, so do not be surprised if it calls the 
garbage collector a lot and runs slowly. The result starts at the starting
time given by the current environment (not necessarily the starting time 
of @i(snd)). If @i(snd) has multiple channels, a multiple channel, 
reversed sound is returned.

@codef{s-read-reverse(@index(read samples in reverse)@pragma(defn)@index(s-read-reverse)@i(filename), time-offset: @i(offset), srate: @i(sr), dur: @i(dur), nchans: @i(chans), format: @i(format), mode: @i(mode), bits: @i(n), swap: @i(flag))} @c{[sal]}@*
@altdef{@code{(s-read-reverse @i(filename) :time-offset @i(offset)
 :srate @i(sr) :dur @i(dur) :nchans @i(chans) :format @i(format) :mode @i(mode) :bits @i(n) :swap @i(flag))} @c{[lisp]}}@\This function is identical to @code(s-read) (see @ref(s-read-sec)), except it reads the indicated samples in reverse. Like
@code(s-reverse) (see above), it uses XLISP in the inner loop, so it is slow.
Unlike @code(s-reverse), @code(s-read-reverse) uses a fixed amount of 
memory that is independent of how many samples are computed. Multiple channels
are handled. 
@end(fndefs)

@section(Time Delay Functions)
The @code(time-delay-fns.lsp) library implements chorus, phaser, and flange effects.

@begin(fndefs)
@codef[phaser(@pragma(defn)@index(phaser)@index(effects, phaser)@i(snd))] @c{[sal]}@*
@altdef{@code[(phaser @i(snd))] @c{[lisp]}}@\A phaser effect
applied to @i(snd) (a @code(SOUND)). There are no parameters, 
but feel free to modify the source code of this one-liner.

@codef[flange(@pragma(defn)@index(flange)@index(flange effect)@index(effect, flange)@i(snd))] @c{[sal]}@*
@altdef{@code[(flange @i(snd))] @c{[lisp]}}@\A flange effect
applied to @i(snd). To vary the rate and other parameters, see the source code.

@codef[stereo-chorus(@index(chorus)@pragma(defn)@index(stereo-chorus)@i(snd))] @c{[sal]}@*
@altdef{@code[(stereo-chorus @i(snd))] @c{[lisp]}}@\A chorus effect applied to @i(snd),
a @code(SOUND) (monophonic). The output is a stereo sound. All parameters are built-in,
but see the simple source code to make modifications.

@codef[chorus(@pragma(defn)@index(chorus)@index(effect, chorus)@i(snd), @i(maxdepth), @i(depth), @i(rate), 
@i(saturation))] @c{[sal]}@*
@altdef{@code[(chorus @i(snd) @i(maxdepth) @i(depth) @i(rate) @i(saturation))] @c{[lisp]}}@\A chorus effect applied to @i(snd). All parameters may be arrays
as usual. The @i(maxdepth) is a @code(FLONUM) giving twice the maximum value of @i(depth), 
which may be a @code(FLONUM) or a @code(SOUND). The chorus is implemented as a variable delay 
modulated by a sinusoid running at @i(rate) Hz (a @code(FLONUM)). The sinusoid is 
scaled by @i(depth) and offset by @i(maxdepth)/2. The delayed signal is mixed
with the original, and @i(saturation) gives the fraction of the delayed signal
(from 0 to 1) in the mix. A reasonable choice of parameter values is 
@i(maxdepth) = 0.05, @i(depth) = 0.025, @i(rate) = 0.5, and @i(saturation) = 0.5.
@end(fndefs)

@section(Multiple Band Effects)
@index(multiple band effects)@index(bandfx.lsp)
The @code(bandfx.lsp) library implements several effects based on multiple
frequency bands. The idea is to separate a signal into different frequency
bands, apply a slightly different effect to each band, and sum the effected
bands back together to form the result. This file includes its own set of
examples. After loading the file, try @code[f2()], @code[f3()], @code[f4()],
and @code[f5()] to hear them.

There is much room for expansion and experimentation with this library. Other
effects might include distortion in certain bands (for example, there are
commercial effects that add distortion to low frequencies to enhance the sound
of the bass), separating bands into different channels for stereo or multi-channel
effects, adding frequency-dependent reverb, and performing dynamic compression, 
limiting, or noise gate functions on each band. There are also opportunities for
cross-synthesis: using the content of bands extracted from one signal to modify
the bands of another. The simplest of these would be to apply amplitude envelopes
of one sound to another. Please contact us (dannenberg@@cs.cmu.edu) if you 
are interested in working on this library.

@begin(fndefs)
@codef[apply-banded-delay(@index(banded delay)@pragma(defn)@index(apply-banded-delay)@i(s), @i(lowp), @i(highp), @i(num-bands), @i(lowd), @i(highd), @i(fb), @i(wet))] @c{[sal]}@*
@altdef{@code[(apply-banded-delay @i(s) @i(lowp) @i(highp) @i(num-bands) @i(lowd) @i(highd) @i(fb) @i(wet))] @c{[lisp]}}@\Separates
input @code(SOUND) @i(s) into @code(FIXNUM) @i(num-bands) bands from a low frequency
of @i(lowp) to a high frequency of @i(highp) (these are @code(FLONUMS) that specify
steps, not Hz), and applies a delay to each band. The delay for the lowest band is
given by the @code(FLONUM) @i(lowd) (in seconds) and the delay for the highest band
is given by the @code(FLONUM) @i(highd). The delays for other bands are linearly
interpolated between these values. Each delay has feedback gain controlled by
@code(FLONUM) @i(fb). The delayed bands are scaled by @code(FLONUM) @i(wet), and 
the original sound is scaled by 1 - @i(wet). All are summed to form the result, 
a @code(SOUND).

@codef[apply-banded-bass-boost(@index(banded bass boost)@pragma(defn)@index(apply-banded-bass-boost)@i(s), @i(lowp), @i(highp), @i(num-bands), @i(num-boost), @i(gain))] @c{[sal]}@*
@altdef{@code[(apply-banded-bass-boost @i(s) @i(lowp) @i(highp) @i(num-bands) @i(num-boost) @i(gain))] @c{[lisp]}}@\Applies a boost to 
low frequencies. Separates
input @code(SOUND) @i(s) into @code(FIXNUM) @i(num-bands) bands from a low frequency
of @i(lowp) to a high frequency of @i(highp) (these are @code(FLONUMS) that specify
steps, not Hz), and scales the lowest @i(num-boost) (a @code(FIXNUM)) bands by @i(gain),
a @code(FLONUM). The bands are summed to form the result, a @code(SOUND).

@codef[apply-banded-treble-boost(@index(banded treble boost)@pragma(defn)@index(apply-banded-treble-boost)@i(s), @i(lowp), @i(highp), @i(num-bands), @i(num-boost), @i(gain))] @c{[sal]}@*
@altdef{@code[(apply-banded-treble-boost @i(s) @i(lowp) @i(highp) @i(num-bands) @i(num-boost) @i(gain))] @c{[lisp]}}@\Applies a boost to 
high frequencies. Separates
input @code(SOUND) @i(s) into @code(FIXNUM) @i(num-bands) bands from a low frequency
of @i(lowp) to a high frequency of @i(highp) (these are @code(FLONUMS) that specify
steps, not Hz), and scales the highest @i(num-boost) (a @code(FIXNUM)) bands by @i(gain),
a @code(FLONUM). The bands are summed to form the result, a @code(SOUND).
@end(fndefs)

@section(Granular Synthesis)
Some granular synthesis functions are implemented in the @code(gran.lsp) library
file. There are many variations and control schemes one could adopt for granular
synthesis, so it is impossible to create a single universal granular synthesis
function. One of the advantages of Nyquist is the integration of control and
synthesis functions, and users are encouraged to build their own granular synthesis
functions incorporating their own control schemes. The @code(gran.lsp) file
includes many comments and is intended to be a useful starting point. Another 
possibility is to construct a score with an event for each grain. Estimate a
few hundred bytes per score event (obviously, size depends on the number of
parameters) and avoid using all of your computer's memory.

@begin(fndefs)
@codef{sf-granulate(@index(granular synthesis)@pragma(defn)@index(sf-granulate)@i(filename), @i(grain-dur), @i(grain-dev), @i(ioi), @i(ioi-dev), @i(pitch-dev),
[@i(file-start), @i(file-end)])} @c{[sal]}@*
@altdef{@code{(sf-granulate @i(filename) @i(grain-dur) @i(grain-dev) @i(ioi) @i(ioi-dev) @i(pitch-dev) [@i(file-start) @i(file-end)])} @c{[lisp]}}@\Granular synthesis using a sound file
named @i(filename) as the source for grains. Grains are extracted from
a sound file named by @i(filename) by stepping through the file in equal
increments. Each grain duration is the 
sum of @i(grain-dur) and a random number from 0 to @i(grain-dev). Grains are 
then multiplied by a raised cosine smoothing window and resampled at a ratio
between 1.0 and @i(pitch-dev). If @i(pitch-dev) is greater than one, grains are
stretched and the pitch (if any) goes down. If @i(pitch-dev) is less than one,
grains are shortened and the pitch goes up. Grains are then output
with an 
inter-onset interval between successive grains (which may overlap) 
determined by the sum of
@i(ioi) and a random number from 0 to @i(ioi-dev). 
The duration of the resulting sound is determined by
the stretch factor (not by the sound file). The number of grains is
the total sound duration (determined by the stretch factor)
divided by the mean inter-onset interval, 
which is @i(ioi) + @i(ioi-dev) * 0.5. 
The grains are taken from equally-spaced starting points in @i(filename),
and depending on grain size and number, the grains may or may not overlap.
The output duration will simply be the sum of the inter-onset intervals
and the duration of the last grain. If @i(ioi-dev) is non-zero, the
output duration will vary, but the expected value of the duration is
the stretch factor.
To achieve a rich granular synthesis effect, it is often a good idea to
sum four or more copies of @code(sf-granulate) together. (See the @code(gran-test)
function in @code(gran.lsp).)
@end(fndefs)

@section(MIDI Utilities)
The @code(midishow.lsp) library has functions that can print the contents fo MIDI
files. This intended as a debugging aid.

@begin(fndefs)
@codef[midi-show-file(@pragma(defn)@index(midi-show-file)@index(print midi file)@index(show midi file)@i(file-name))] @c{[sal]}@*
@altdef{@code[(midi-show-file @i(file-name))] @c{[lisp]}}@\Print the contents of a MIDI file to the console.

@codef{midi-show(@pragma(defn)@index(midi-show)@i(the-seq) [, @i(out-file)])} @c{[sal]}@*
@altdef{@code{(midi-show @i(the-seq) [@i(out-file)])} @c{[lisp]}}@\Print the
contents of the sequence @i(the-seq) to the file @i(out-file) (whose default value
is the console.)
@end(fndefs)

@section(Reverberation)
The @code(reverb.lsp) library implements artificial reverberation.

@begin(fndefs)
@codef[reverb(@pragma(defn)@index(reverb)@index(effect, reverberation)@i(snd), 
@i(time))] @c{[sal]}@*
@altdef{@code[(reverb @i(snd) @i(time))] @c{[lisp]}}@\Artificial reverberation applied to @i(snd) with a decay time of
@i(time).
@end(fndefs)

@section(DTMF Encoding)
@index(dtmf)@index(touch tone)
The @code(dtmf.lsp) library implements DTMF encoding. DTMF is the 
``touch tone'' code used by telephones.

@begin(fndefs)
@codef[dtmf-tone(@pragma(defn)@index(dtmf-tone)@i(key), @i(len), @i(space))] @c{[sal]}@*
@altdef{@code[(dtmf-tone @i(key) @i(len) @i(space))] @c{[lisp]}}@\Generate a
single DTMF tone. The @i(key) parameter is either a digit (a @code(FIXNUM)
from 0 through 9) or the atom @code(STAR) or @code(POUND). The duration of
the done is given by @i(len) (a @code(FLONUM)) and the tone is followed by
silence of duration @i(space) (a @code(FLONUM)).

@codef[speed-dial(@pragma(defn)@index(speed-dial)@i(thelist))] @c{[sal]}@*
@altdef{@code[(speed-dial @i(thelist))] @c{[lisp]}}@\Generates a sequence
of DTMF tones using the keys in @i(thelist) (a @code(LIST) of keys as
described above under @code(dtmf-tone)). The duration of each tone is 0.2
seconds, and the space between tones is 0.1 second. Use @code(stretch) to
change the ``dialing'' speed.
@end(fndefs)

@section[Dolby Surround(R), Stereo and Spatialization Effects]
@index(spatialization)@index(stereo)@index(pan)@index(Dolby Surround)

The @code(spatial.lsp) library implements various functions for stereo
manipulation and spatialization. It also includes some functions for
Dolby Pro-Logic panning, which encodes left, right, center, and surround 
channels into stereo. The stereo signal can then be played through
a Dolby decoder to drive a surround speaker array. This library has
a somewhat simplified encoder, so you should certainly test the
output. Consider using a high-end encoder for critical work. There
are a number of functions in @code(spatial.lsp) for testing. See the
source code for comments about these.

@begin(fndefs)
@codef[stereoize(@pragma(defn)@index(stereoize)@index(mono to stereo)@index(effect, stereo)@i(snd))] @c{[sal]}@*
@altdef{@code[(stereoize @i(snd))] @c{[lisp]}}@\Convert a mono sound, @i(snd), to stereo. Four bands of 
equalization and some delay are used to create a stereo effect.

@codef[widen(@pragma(defn)@index(widen)@index(effect, widen)@i(snd), @i(amt))] @c{[sal]}@*
@altdef{@code[(widen @i(snd) @i(amt))] @c{[lisp]}}@\Artificially
widen the stereo field in @i(snd), a two-channel sound. The amount of widening
is @i(amt), which varies from 0 (@i(snd) is unchanged) to 1 (maximum widening).
The @i(amt) can be a @code(SOUND) or a number.

@codef[span(@index(stereo pan)@index(pan, stereo)@index(effect, stereo pan)@pragma(defn)@index(span)@i(snd), @i(amt))] @c{[sal]}@*
@altdef{@code[(span @i(snd) @i(amt))] @c{[lisp]}}@\Pan the virtual center channel of a stereo sound, @i(snd),
by @i(amt), where 0 pans all the way to the left, while 1 pans all the way 
to the right. The @i(amt) can be a @code(SOUND) or a number.

@codef[swapchannels(@pragma(defn)@index(swapchannels)@index(swap channels)@index(effect, swap channels)@i(snd))] @c{[sal]}@*
@altdef{@code[(swapchannels @i(snd))] @c{[lisp]}}@\Swap left and right channels in @i(snd), a stereo sound.

@codef[prologic(@pragma(defn)@index(prologic)@index(Dolby Pro-Logic)@index(Surround Sound)@i(l), @i(c), 
@i(r), @i(s))] @c{[sal]}@*
@altdef{@code[(prologic @i(l) @i(c) @i(r) @i(s))] @c{[lisp]}}@\Encode four monaural @code(SOUND)s representing the front-left,
front-center, front-right, and rear channels, respectively. 
The return value is a stereo sound, which is a Dolby-encoded mix of the
four input sounds. 

@codef[pl-left(@pragma(defn)@index(pl-left)@i(snd))] @c{[sal]}@*
@altdef{@code[(pl-left @i(snd))] @c{[lisp]}}@\Produce a Dolby-encoded (stereo)
signal with @i(snd), a @code(SOUND), encoded as the front left channel.

@codef[pl-center(@pragma(defn)@index(pl-center)@i(snd))] @c{[sal]}@*
@altdef{@code[(pl-center @i(snd))] @c{[lisp]}}@\Produce a Dolby-encoded (stereo)
signal with @i(snd), a @code(SOUND), encoded as the front center channel.

@codef[pl-right(@pragma(defn)@index(pl-right)@i(snd))] @c{[sal]}@*
@altdef{@code[(pl-right @i(snd))] @c{[lisp]}}@\Produce a Dolby-encoded (stereo)
signal with @i(snd), a @code(SOUND), encoded as the front right channel.

@codef[pl-rear(@pragma(defn)@index(pl-rear)@i(snd))] @c{[sal]}@*
@altdef{@code[(pl-rear @i(snd))] @c{[lisp]}}@\Produce a Dolby-encoded (stereo)
signal with @i(snd), a @code(SOUND), encoded as the rear, or surround, channel.

@codef[pl-pan2d(@pragma(defn)@index(pl-pan2d)@i(snd), @i(x), @i(y))] @c{[sal]}@*
@altdef{@code[(pl-pan2d @i(snd) @i(x) @i(y))] @c{[lisp]}}@\Comparable to Nyquist's
existing pan function, @code(pl-pan2d) provides not only left-to-right
panning, but front-to-back panning as well. The function
accepts three parameters: @i(snd) is the (monophonic) input @code(SOUND), 
@i(x) is a left-to-right position, and @i(y) is a front-to-back position.
Both position parameters may be numbers or @code(SOUND)s. An @i(x) value 
of 0 means left, and 1 means right. Intermediate values map linearly
between these extremes. Similarly, a @i(y) value of 0 causes the sound
to play entirely through the front speakers(s), while 1 causes it to play
entirely through the rear. Intermediate values map linearly.
Note that, although there are usually two rear speakers in Pro-Logic systems,
they are both driven by the same signal. Therefore any sound that is
panned totally to the rear will be played over both rear speakers. For
example, it is not possible to play a sound exclusively through the 
rear left speaker.

@codef[pl-position(@pragma(defn)@index(pl-position)@i(snd), @i(x), @i(y), @i(config))] @c{[sal]}@*
@altdef{@code[(pl-position @i(snd) @i(x) @i(y) @i(config))] @c{[lisp]}}@\The
position function builds upon speaker panning to allow more abstract
placement of sounds. Like @code(pl-pan2d), it accepts a (monaural) input
sound as well as left-to-right (@i(x)) and front-to-back (@i(y)) coordinates,
which may be @code(FLONUM)s or @code(SOUND)s. A fourth parameter @i(config) 
specifies the distance from listeners to the speakers (in meters). Current
settings assume this to be constant for all speakers, but this assumption 
can be changed easily (see comments in the code for more detail).
There are several important differences between @code(pl-position) and 
@code(pl-pan2d). First, @code(pl-position) uses a Cartesian coordinate 
system that allows x and y coordinates outside of the
range (0, 1). This model assumes a listener position of (0,0). Each speaker
has a predefined position as well. The input sound's position,
relative to the listener, is given by the vector (@i(x),@i(y)).

@codef[pl-doppler(@pragma(defn)@index(pl-doppler)@index(Doppler effect)@i(snd), 
@i(r))] @c{[sal]}@*
@altdef{@code[(pl-doppler @i(snd) @i(r))] @c{[lisp]}}@\Pitch-shift moving sounds according to the equation: @i(fr) =
@i(f0)((@i(c)+@i(vr))/@i(c)), where @i(fr) is the output frequency, 
@i(f0) is the emitted (source) 
frequency, @i(c) is the speed of sound (assumed to be 344.31 m/s), and 
@i(vr) is the speed at which the emitter approaches the receiver. (@i(vr)
is the first derivative of parameter @i(r), the distance from the listener
in meters.

@end(fndefs)

@section(Drum Machine)
@label(drum-machine-sec)@index(drum machine)

The drum machine software in @code(demos/plight) deserves further explanation. 
to use the software, load the code by evaluating:
@begin(example)
load "../demos/plight/drum.lsp"
exec load-props-file(strcat(*plight-drum-path*, 
                            "beats.props"))
exec create-drum-patches()
exec create-patterns()
@end(example)

Drum sounds and patterns are specified in the @code(beats.props) file (or
whatever name you give to @code(load-props-file)). This file
contains two types of specifications. First, there are sound file specifications.
Sound files are located by a line of the form:
@begin(example)
set sound-directory = "kit/"
@end(example)
This gives the name of the sound file directory, relative to the 
 @code(beats.props) file. Then, for each sound file, there should be a line of
the form:
@begin(example)
track.2.5 = big-tom-5.wav
@end(example)
This says that on track 2, a velocity value of 5 means to play the sound file
 @code(big-tom-5.wav). (Tracks and velocity values are described below.) 
The @code(beats.props) file contains specifications for all the sound files
in @code(demos/plight/kit) using 8 tracks. If you make your own specifications
file, tracks should be numbered consecutively from 1, and velocities should be
in the range of 1 to 9.

The second set of specifications is of beat patterns. A beat pattern is given
by a line in the following form:
@begin(example)
beats.5 = 2--32--43-4-5---
@end(example)
The number after @code(beats) is just a pattern number. Each pattern
is given a unique number. After the equal sign, the digits and dashes are
velocity values where a dash means ``no sound.'' Beat patterns should be
numbered consecutively from 1.

Once data is loaded, there are several functions to access drum patterns and 
create drum sounds (described below). The @code(demos/plight/drums.lsp) file
contains an example function @code(plight-drum-example) to play some drums.
There is also the file @code(demos/plight/beats.props) to serve as an
example of how to specify sound files and beat patterns.

@begin(fndefs)
@codef{drum(@pragma(defn)@index(drum)@i(tracknum), @i(patternnum), @i(bpm))} @c{[sal]}@*
@altdef{@code[(drum @i(tracknum) @i(patternnum) @i(bpm))] @c{[lisp]}}@\Create
a sound by playing drums sounds associated with track @i(tracknum) (a 
FIXNUM) using pattern @i(patternnum). The tempo is given by @i(bpm) in
beats per minute. Normally patterns are a sequence of sixteenth notes, so
the tempo is in @i(sixteenth notes per minute). For example,
if @i(patternnum) is 10,
then use the pattern specified for @code(beats.10). If the third character
of this pattern is 3 and @i(tracknum) is 5, then on the third beat, play
the soundfile assigned to @code(track.5.3). This function returns a @code(SOUND).

@codef{drum-loop(@pragma(defn)@index(drum-loop)@i(snd), @i(duration), @i(numtimes))} @c{[sal]}@*
@altdef{@code[(drum-loop @i(snd) @i(duration) @i(numtimes))] @c{[lisp]}}@\Repeat the sound given by @i(snd) @i(numtimes) times. The repetitions occur at a time offset of @i(duration), regardless of the actual duration of @i(snd). A @code(SOUND) is returned.

@codef{length-of-beat(@pragma(defn)@index(length-of-beat)@i(bpm))} @c{[sal]}@*
@altdef{@code[(length-of-beat @i(bpm))] @c{[lisp]}}@\Given a tempo of 
@i(bpm), return the duration of the beat in seconds. Note that this software
has no real notion of beat. A ``beat'' is just the duration of each character
in the beat pattern strings. This function returns a @code(FLONUM).
@end(fndefs)



@section(Minimoog-inspired Synthesis)
@index(Moog)@index(Minimoog)@index(analog synthesizer)

The @code(moog.lsp) library gives the Nyquist user easy access to ``classic''
synthesizer sounds through an emulation of the Minimoog Synthesizer.
Unlike modular Moogs that were very large, the Minimoog was the first
successful and commonly used portable synthesizer. The trademark filter attack
was unique and easily recognizable. The goal of this Nyquist instrument is not
only to provide the user with default sounds, but also to give control over
many of the ``knobs'' found on the Minimoog. In this implementation, these
parameters are controlled using keywords. The input to the @code(moog) 
instrument is a user-defined sequence of notes, durations, and articulations
that simulate notes played on a keyboard. These are translated into 
control voltages that drive multiple oscillators, similar to the Voltage
Controlled Oscillator or VCO found in the original analog Moog.

The basic functionality of the Minimoog has been implemented, including the
often-used "glide". The glide feature essentially low-pass filters the control
voltage sequence in order to create sweeps between notes. 
Figure @ref(moog-fig) is a simplified schematic of the data flow in the Moog. 
The control lines have been omitted.

@begin(figure)
@center(@graphic((height = 2.514 in, width = 4.65 in, magnify = 0.3,
                postscript = "moog-fig.ps"))
@html(<img src="moog-fig.gif" width=558><br><br>)
@fillcaption(System diagram for Minimoog emulator.)
@tag(moog-fig)
@end(figure)

The most recognizable feature of the Minimoog is its resonant filter, a 
Four-Pole Ladder Filter invented by Robert Moog. It is simply implemented 
in a circuit with four transistors and provides an outstanding 24 dB/octave 
rolloff. It is modeled here using the built-in Nyquist resonant filter. 
One of the Moog filter features is a constant Q, or center frequency to 
bandwidth ratio. This is implemented and the user can control the Q. 

The user can control many parameters using keywords. Their default values,
acceptable ranges, and descriptions are shown below. The defaults were
obtained by experimenting with the official Minimoog software synthesizer
by Arturia.

@subsection(Oscillator Parameters)
@code(range-osc1) (2)@*
@code(range-osc2) (1)@*
@code(range-osc3) (3)@*
These parameters control the octave of each oscillator. A value of 1
corresponds to the octave indicated by the input note. A value of 3
is two octaves above the fundamental. The allowable range is 1 to 7.

@code(detun2) (-.035861)@*
@code(detun3) (.0768)@*
Detuning of two oscillators adds depth to the sound. A value of 1 corresponds
to an increase of a single semitone and a -1 corresponds to a decrease
in a semitone. The range is -1 to 1.

@code(shape-osc1) (@code(*saw-table*))@*
@code(shape-osc2) (@code(*saw-table*))@*
@code(shape-osc3) (@code(*saw-table*))@*
Oscilators can use any wave shape. The default sawtooth waveform is
a built-in Nyquist variable. Other waveforms can be defined by the user.

@code(volume-osc1) (1)@*
@code(volume-osc2) (1)@*
@code(volume-osc3) (1)@*
These parameters control the relative volume of each oscillator. The range
is any @code(FLONUM) greater than or equal to zero.

@subsection(Noise Parameters)
@code(noiselevel) (.05)@*
This parameter controls the relative volume of the noise source. The range
is any @code(FLONUM) greater than or equal to zero.
	
@subsection(Filter Parameters)
@code(filter-cutoff) (768)@*
The cutoff frequency of the filter in given in Hz. The range is zero
to 20,000 Hz.


@code(Q) (2)@*
Q is the ratio of center frequency to bandwidth. It is held constant by
making the bandwidth a function of frequency. The range is any
@code(FLONUM) greater than zero.

@code(contour) (.65)@*
Contour controls the range of the transient frequency sweep from a high
to low cutoff frequency when a 	note is played. The high frequency is
proportional to contour. A contour of 0 removes this sweep. The range
is 0 to 1.

@code(filter-attack) (.0001)@*
Filter attack controls the attack time of the filter, i.e. the time to
reach the high cutoff frequency. The range is any @code(FLONUM) greater
than zero (seconds).

@code(filter-decay) (.5)@*
Filter decay controls the decay time of the filter, i.e. the time of the
sweep from the high to low cutoff frequency. The range is 
any @code(FLONUM) greater than zero (seconds).

@code(filter-sustain) (.8)@*
Filter sustain controls the percentage of the filter cutoff frequency that
the filter settles on following the sweep. The range is 0 to 1.
	
@subsection(Amplitude Parameters)
@code(amp-attack) (.01)@*
This parameter controls the amplitude envelope attack time, i.e. the time to
reach maximum amplitude. The range is 
any @code(FLONUM) greater than zero (seconds).

@code(amp-decay) (1)@*
This parameter controls the amplitude envelope decay time, i.e. the time
between the maximum and sustain volumes. The range is
any @code(FLONUM) greater than zero (seconds).

@code(amp-sustain) (1)@*
This parameter controls the amplitude envelope sustain volume, a fraction
of the maximum. The range is 0 to 1.

@code(amp-release) (0)@*
This parameter controls the amplitude envelope release time, i.e. the time
it takes between the sustain volume and 0 once the note ends. 
The duration controls the overall length of the sound. The range of @code(amp-release) is any @code(FLONUM) greater than zero (seconds).

@subsection(Other Parameters)
@code(glide) (0)@*
Glide controls the low-pass filter on the control voltages. This models the
glide knob on a Minimoog. A higher value corresponds to a lower cutoff
frequency and hence a longer "glide" between notes. A value of 0
corresponds to no glide. The range is zero to 10.

@subsection(Input Format)
A single note or a series of notes can be input to the Moog instrument
by defining a list with the following format:
@begin(example)
list(list(@i(frequency), @i(duration), @i(articulation)), @r(...) )
@end(example)
where @i(frequency) is a @code(FLONUM) in steps, @i(duration) is the duration
of each note in seconds (regardless of the release time of the amplifier), 
and @i(articulation) is a percentage of the duration that a sound will be
played, representing the amount of time that a key is pressed. The filter 
and amplitude envelopes are only triggered if a note is played when
the articulation of the previous note is less than 1, or a key is not down at
the same time. This Moog instrument is a monophonic instrument, so only
one note can sound at a time. The release section of the amplifier is
triggered when the articulation is less than 1 at the time 
(@i(duration) * @i(articulation)).

@subsection(Sample Code/Sounds)

@b[Sound 1 (default parameters):]
@begin(display)
@begin(code)
set s = {{24 .5 .99} {26 .5 .99} {28 .5 .99} 
         {29 .5 .99} {31 2 1}}
play moog(s)
@end(code)
@end(display)

@b[Sound 2 (articulation, with amplitude release):]
@begin(display)
@begin(code)
set s = {{24 .5 .5} {26 .5 1} {28 .5 .25} {29 .5 1} {31 1 .8}}
play moog(s, amp-release: .2)
@end(code)
@end(display)

@b[Sound 3 (glide):]
@begin(display)
@begin(code)
set s = {{24 .5 .5} {38 .5 1} {40 .5 .25}
         {53 .5 1} {55 2 1} {31 2 .8} {36 2 .8}}
play moog(s, amp-release: .2, glide: .5)
@end(code)
@end(display)

@b[Sound 4 (keyword parameters):] Filter attack and decay are purposely
longer than notes being played with articulation equal to 1.
@begin(display)
@begin(code)
set s = {{20 .5 1} {27 .5 1} {26 .5 1} {21 .5 1}
         {20 .5 1} {27 .5 1} {26 .5 1} {21 .5 1}}
play moog(s, shape-osc1: *tri-table*, shape-osc2: *tri-table*,
             filter-attack: 2, filter-decay: 2,
             filter-cutoff: 300, contour: .8, glide: .2, Q: 8)
@end(code)
@end(display)

@b[Sound 5:] This example illustrates the ability to completely define a new 
synthesizer with different parameters creating a drastically different
sound. Sine waves are used for wavetables. There is a high value for glide.
@begin(display)
@begin(code)
define function my-moog(freq) 
  return moog(freq,
    range-osc1: 3, range-osc2: 2, range-osc3: 4,
    detun2: -.043155, detun3: .015016,
    noiselevel: 0,
    filter-cutoff: 400, Q: .1, contour: .0000001,
    filter-attack: 0, filter-decay: .01, filter-sustain: 1,
    shape-osc1: *sine-table*, shape-osc2: *sine-table*,
    shape-osc3: *sine-table*, volume-osc1: 1, volume-osc2: 1,
    volume-osc3: .1, amp-attack: .1, amp-decay: 0,
    amp-sustain: 1, amp-release: .3, glide: 2)

set s = {{80 .4 .75} {28 .2 1} {70 .5 1} {38 1 .5}}
play my-moog(s)
@end(code)
@end(display)

@b[Sound 6:] This example has another variation on the default
 parameters.
@begin(display)
@begin(code)
set s = {{24 .5 .99} {26 .5 .99} {28 .5 .99} 
         {29 .5 .99} {31 2 1}}
play moog(s, shape-osc1: *tri-table*, shape-osc2: *tri-table*,
             filter-attack: .5, contour: .5)
@end(code)
@end(display)

@pragma(doinclude)
@include(nymanimpl.mss)

@appendix(Open Sound Control and Nyquist)@index(Open Sound Control)
@label(osc-app)
Open Sound Control (OSC) is a simple protocol for communicating music
control parameters between software applications and across
networks. For more information, see @html[<a
href="http://wwww.cnmat.berkeley.edu/OpenSoundControl">]@code(http://www.cnmat.berkeley.edu/OpenSoundControl/)@html[</a>]. The
Nyquist implementation of Open Sound Control is simple: an array of
floats can be set by OSC messages and read by Nyquist functions. That
is about all there is to it. 

Note: Open Sound Control must be enabled by calling
@code[osc-enable(t)]. If this fails under Windows, see the
installation instructions in @code(sys/win/README.txt) regarding
@code(SystemRoot).

To control something in (near) real-time, you need to access a slider value as if it a signal, or more properly, a Nyquist @code(SOUND) type. The function @code(snd-slider), described in Section @ref(snd-slider-sec), takes a slider number and returns a @code(SOUND) type representing the current value of the slider. To fully understand this function, you need to know something about how Nyquist is actually computing sounds.

Sounds are normally computed on demand. So the result returned by @code(snd-slider) does not immediately compute any samples. Samples are only computed when something tries to use this signal. At that time, the slider value is read. Normally, if the slider is used to control a sound, you will hear changes in the sound pretty soon after the slider value changes. However, one thing that can interfere with this is that @code(SOUND) samples are computed in blocks of about 1000 samples. When the slider value is read, the same value is used to fill a block of 1000 samples, so even if the sample rate is 44,100 Hz, the effective slider sample rate is 44,100/1000, or 44.1 Hz. If you give the slider a very low sample rate, say 1000, then slider value changes will only be noticed by Nyquist approximately once per second. For this reason, you should normally use the audio sample rate (typically 44,100 Hz) for the rate of the @code(snd-slider) output @code(SOUND). (Yes, this is terribly wasteful to represent each slider value with 1000 samples, but Nyquist was not designed for low-latency computation, and this is an expedient work-around.)

In addition to reading sliders as continually changing @code(SOUND)s, you can get the slider value as a Lisp @code(FLONUM) (a floating point number) using @code(get-slider-value), described in Section @ref(get-slider-value-sec). This might be useful if you are computing a sequence of many notes (or other sound events) and want to apply the current slider value to the whole note or sound event.

Note that if you store the value returned by @code(snd-slider) in a variable, you will capture the history of the slider changes. This will take a lot of memory, so be careful.

Suppose you write a simple expression such as @code[(hzosc (mult 1000 (snd-slider 0 @r(...))))] (or in SAL, @code[hzosc(1000 * snd-slider(0 @r(...)))]) to control an oscillator frequency with a slider. How long does this sound last? The duration of @code[hzosc] is the duration of the frequency control, so what is the duration of a slider? To avoid infinitely long signals, you must specify a duration as one of the parameters of @code[snd-slider].

You might be thinking, what if I just want to tell the slider when to stop? At present, you cannot do that, but in the future there should be a function that stops when its input goes to zero. Then, moving a slider to zero could end the signal (and if you multiplied a complex sound by one of these ending functions, everything in the sound would end and be garbage collected).

Another thing you might want to do with interactive control is start some sound. The @code(trigger) function computes an instance of a behavior each time an input @code(SOUND) goes from zero to greater-than-zero. This could be used, for example, to create a sequence of notes.

The @code(snd-slider) function has some parameters that may be unfamiliar. The second parameter, @i(t0), is the starting time of the sound. This should normally be @code[local-to-global(0)], an expression that computes the instantiation time of the current expression. This will often be zero, but if you call @code[snd-slider] from inside a @code(seq) or @code(seq-rep), the starting time may not be zero. 

The @i(srate) parameter is the sample rate to return. This should normally be the audio sample rate you are working with, which is typically @code[*default-sound-srate*].

@section(Sending Open Sound Control Messages)
A variety of programs support OSC. The only OSC message interpreted by Nyquist has an address of @code[/slider], and two parameters: an integer slider number and a float value, nominally from 0.0 to 1.0. 

Two small programs are included in the Nyquist distribution for sending OSC messages. (Both can be found in the same directory as the nyquist executable.) The first one, @code[osc-test-client] sends a sequence of messages that just cause slider 0 to ramp slowly up and down. If you run this on a command line, you can use "?" or "h" to get help information. There is an interactive mode that lets you send each OSC message by typing RETURN.

@section(The ser-to-osc Program)
The second program is @code[ser-to-osc], a program that reads serial input (for example from a PIC-based microcontroller) and sends OSC messages. Run this command-line program from a shell (a terminal window under OS X or Linux; use the CMD program under Windows). You must name the serial input device on the command line, e.g. under OS X, you might run:
@begin(display)
@code(./ser-to-osc /dev/tty.usbserial-0000103D)
@end(display)
(Note that the program name is preceded by ``@code(./)". This tells the shell exactly where to find the executable program in case the current directory is not on the search path for executable programs.)
Under Windows, you might run:
@begin(display)
@code(ser-to-osc com4)
@end(display)
(Note that you do not type ``@code(./)'' in front of a windows program.)

To use @code(ser-to-osc), you will have to find the serial device. On the Macintosh and Linux, try the following:
@begin(display)
@code(ls /dev/*usb*)
@end(display)
This will list all serial devices with ``usb'' in their names. Probably, one will be a name similar to @code(/dev/tty.usbserial-0000103D). The @code(ser-to-osc) program will echo data that it receives, so you should know if things are working correctly.

Under Windows, open Control Panel from the Start menu, and open the System control panel. Select the Hardware tab and click the Device Manager button. Look in the device list under Ports (COM & LPT). When you plug in your serial or USB device, you should see a new entry appear, e.g. @code(COM4). This is the device name you need.

The format for the serial input is: any non-whitespace character(s), a slider number, a slider value, and a newline (control-j or ASCII 0x0A). These fields need to be separated by tabs or spaces. An optional carriage return (control-m or ASCII 0x0D) preceding the ASCII 0x0A is ignored. The slider number should be in decimal, and theh slider value is a decimal number from 0 to 255. This is scaled to the range 0.0 to 1.0 (so an input of 255 translates to 1.0).

There is a simple test program in @code[demos/osc-test.lsp] you can run to try out control with Open Sound Control. There are two examples in that file. One uses @code(snd-slider) to control the frequency of an oscillator. The other uses @code(get-slider-value) to control the pitch of grains in a granular synthesis process.


@appendix(Intgen)@index(Intgen)
@label(intgen-app)
@pragma(doinclude)
@include(../xlisp/intgen.mss)

@appendix(XLISP: An Object-oriented Lisp)
@label(xlisp-app)
@begin(center)

@b(Version 2.0)

February 6, 1988

by
@b(David Michael Betz)
127 Taylor Road
Peterborough, NH 03458

Copyright (c) 1988, by David Michael Betz
All Rights Reserved
Permission is granted for unrestricted non-commercial use
@end(center)
@newpage
@pragma(doinclude)
@include(../xlisp/xlisp.mss)
