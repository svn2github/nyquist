README.txt -- information on Nyquist for Mac OS X 

IMPORTANT
---------
You may get the message 
    "NyquistIDE is damaged and can't be opened. 
    You should move it to the Trash from the Finder."
This appears to be a deliberately incorrect statement from Apple.
Here is what to do:

1) Go to Preferences, Security & Privacy
2) Unlock (requires password)
3) Allow apps downloaded from: Anywhere
4) Control-click on the NyquistIDE icon and select open
5) Tell the Finder yes, you really want to open this application
6) Nyquist should run
7) Close Nyquist (maybe not necessary)
8) Now, you can go back to Security & Privacy and revert back 
    to the original policy, e.g. "Mac App Store and identified
    developers" if you wish (a reasonable precaution)
9) Lock the settings in Security & Privacy (click the lock icon)
10) Test: You should now be able to click to start Nyquist; it 
    should start without any false claims about file damage or 
    even warning messages.

Installation
------------
The simplest way to install and run Nyquist is to get the pre-compiled
NyquistIDE application, which includes executables, documentation, and
libraries all in one package and one directory:
     NyquistIDE.app, and
     nyquist
You should keep these together.

NyquistIDE.app may ask you to find the nyquist directory because it
contains documentation.

NyquistIDE.app will also put symbolic links in nyquist to the lib and
demos folders (hidden inside NyquistIDE.app) so that you can easily
look in demos for descriptions and code (see nyquist/demos/index.html
after the link is in place).

You can move the NyquistIDE.app and nyquist directories to the
/Applications folder if you wish, and you can add NyquistIDE.app to
the Dock. If you move NyquistIDE.app, you should move the nyquist
directory too, but it's not required.

You will probably run Nyquist using the NyquistIDE application, but
you can also run nyquist from the command line. The executable is
located in

    NyquistIDE.app/Contents/Resources/Java/ny

To run from the command line, you will need to set the XLISPPATH
environment variable using this command line (if you use the C shell,
e.g. csh):

    setenv XLISPPATH `pwd`/runtime:`pwd`/lib

If you use the bash shell, use:

    export XLISPPATH=`pwd`/runtime:`pwd`/lib

Note that this sets XLISPPATH in the environment of the current
command line shell. If you exit the shell or switch to another shell,
the XLISPPATH variable will not be set. Your shell reads an
initialization file when it starts. You can add the XLISPPATH
initialization command to this file if you want the variable to be set
automatically in every instance of your command line shell.

On the topic of the XLISPPATH, note that this variable is set by
NyquistIDE when running with that application, overriding any other
value. You can extend the search path by creating the file xlisppath
in the same directory as the nyquist executable ny. The xlisppath file
should have colon-separated paths on a single line of text.

You can also build Nyquist from sources, as described below.


How To Build Nyquist on Mac OS X
--------------------------------
You need to install Xcode, Apple's free software development system
for OS X, and CMake for OS X. Use CMake to build the xcode project.
Get the sources from SourceForge, and run CMake on
nyquist/CMakeLists.txt.
