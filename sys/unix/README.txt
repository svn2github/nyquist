README.txt -- Nyquist information for Unix systems

Unix Installation
------------------
For Unix systems, Nyquist is distributed as a compressed file of
sources named nyqsrc3<nn>.zip, where <nn> is the version number
(e.g. v3.01 was in nyqsrc301.zip).  To install Nyquist, copy
nyqsrc3<nn>.zip) to the directory on your machine where you would
like to install Nyquist.

Note 1: you will need the "normal tool chain" consisting of the Gnu
C/C++ compiler, linker, C/C++ runtime libraries, autoconf, libtool,
automake, etc. Most linux installations already have this, but some 
more recent trimmed-down installations for netbooks and
consumer-oriented computers do not have compilers installed by
default.

Note 2: Nyquist also assumes you have ALSA, the Linux audio system.
This has also become standard, but your machine might not have the
ALSA development package (probably named libasound2-dev), so you 
might have to install it. If you find you are missing "asound", you
are missing and need to install the ALSA developmnent package.

After unzipping sources, type:

    gunzip nyqsrc3<nn>.zip
    cd nyquist
    ln -s sys/unix/linux/Makefile Makefile
    setenv XLISPPATH `pwd`/runtime:`pwd`/lib
    make

(For bash shell users, instead of the setenv command, use this:

    export XLISPPATH=`pwd`/runtime:`pwd`/lib
)

The first line creates a nyquist directory and some
subdirectories. The second line (cd) changes directories to the new
nyquist directory. The third line (ln) makes a link from the top-level
directory to the Makefile for your system. In place of linux in
sys/unix/linux/Makefile, you should substitute your system
type. Current systems are next, pmax, rs6k, sgi, linux, and sparc, but
since only the linux version has been tested in recent years, do not
expect anything else to work.  The setenv (or export) command tells
Nyquist where to search for lisp files to be loaded when a file is not
found in the current directory. The (runtime directory should always
be on your XLISPPATH when you run Nyquist, so you may want to set
XLISPPATH in your shell startup file, e.g. .cshrc.

Assuming the make completes successfully, you can run Nyquist as follows:
    ./ny
When you get the prompt, you may begin typing expressions such as
the ones in the following "Examples" section in the Nyquist
manual. (See doc/nyquistman.pdf or doc/home.html).

One you establish that Nyquist (ny) is working from the command line,
you should try using jNyqIDE, the Java-based Nyquist development
environment. First, make jny executable (do this only once when you
install Nyquist):
    chmod +x jny
Then try running jNyqIDE by typing:
    ./jny

If the jNyqIDE window does not appear, make sure you have Java
installed (if not, you probably already encountered errors when you
ran the make command.) You can also try recompiling the Java
files. Note that jnyqide/SpecialMacHandler.java will NOT compile
under non-OS X systems. The Makefile renames this file to "hide" it
from the Java compiler, compiles all the remaining java files, and
then restores jnyqide/SpecialMacHandler.java:
    make jnyqide/jNyqIDE.jar


Note: With Linux and Mac OS X, jNyqIDE defines the environment passed
to Nyquist. If you set XLISPPATH as shown above, it will be passed
along to Nyquist under jNyqIDE. If not, a default XLISPPATH will have
the lib and runtime directories only. This does not apply to Windows
because even though the environment is there, the Windows version of
Nyquist reads the XLISPPATH from the Registry. 

You can also specify the search path by creating the file
nyquist/xlisppath, which should have colon-separated paths on a single
(long) line of text. This file will override the environment variable
XLISPPATH.

It is good to have USER in the environment with your user ID. This
string is used to construct some file names. jNyqIDE will look for it
in the environment. You can also specify your user ID using the file
nyquist/user, but if you have a shared installation of Nyquist,
this will not be very useful.

Note: Nyquist looks for the file init.lsp in the current directory.
If you look in the init.lsp in runtime, you will notice two things.
First, init.lsp loads nyquist.lsp from the Nyquist directory, and
second, init.lsp loads system.lsp which in turn defines the macro
play.  Normally, Nyquist plays audio through the PortAudio library,
which should work on any system. An alternative is to save audio to a
file and invoke a local non-Nyquist program to play the sound file.
You can modify system.lsp to accomplish this.
