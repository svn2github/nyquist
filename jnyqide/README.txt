Notes on building a Java app for OS X:

---BEGIN OBSOLETE COMMENTS ON APPBUNDLER---
In this directory, 
   ant jar
builds the Java jar file containing the NyquistIDE

Oracle's appbundler is used. It is in lib/appbundler-1.0.jar

Run the appbundler with:
   ant bundle-nyquist

Here's how to run jnyqide.jar from command line:
    java -Dapple.laf.useScreenMenuBar=true -cp jnyqide.jar jnyqide.Main
(This does everything right except the displayed name on menu bar will
    be Main instead of NyquistIDE, which is what you see when you open
    the .app bundle.)
---END OBSOLETE COMMENTS ON APPBUNDLER---

----------------------------
some comments on building a Java-based App:

Thanks for all the info. It seems like Apple and Oracle make incompatible changes very frequently. Why go to all the trouble to hide application and version details in JavaAppLauncher and then make it all version-dependent? I went through the ant and app bundler hell less than a year ago, and today I found myself starting all over. I had even less luck than you with JavaAppLauncher (in retrospect, maybe I accidentally left a Java 6 spec in my CMakeFile that build the jar, but that's just one more indication of how brain-damaged all these tools have become).

Here's an alternative approach:

Get your Java app to run from the command line with something like:

java -jar myapp.jar

add some switches to get your app name in the menu bar and your icon to display. Try -Xdock:name and -Xdock:icon. I ended up with more switches, but I'm nor really sure what's necessary:

java -Xdock:name="NyquistIDE" -Xdock:icon=`dirname $0`/../Resources/Nyquist.icns -Dcom.apple.mrj.application.apple.menu.about.name=NyquistIDE -Dapple.laf.useScreenMenuBar=true -DisOSXbundle=true -jar jnyqide.jar 

When that's working, write your own launcher as a shell script. Mine is basically a cd and the previous java command:

#!/bin/sh
cd `dirname $0`/../Java
java -Xdock:name="NyquistIDE" -Xdock:icon=`dirname $0`/../Resources/Nyquist.icns -Dcom.apple.mrj.application.apple.menu.about.name=NyquistIDE -Dapple.laf.useScreenMenuBar=true -DisOSXbundle=true -jar jnyqide.jar 

Modify Info.plist so that the "Bundle Executable" is the shell script (make sure the shell script has the execute flag set).

I took out all references to java and jvm in the Info.plist file. Effectively, I believe this means everything relies on the "java" command running a suitable version of Java.

My whole script to compile Java, build the jar, create an Application Bundle, and populate it with lots of auxiliary files is 55 lines. My CMakeLists.txt now has about 6 lines to delete any existing Application Bundle and run the script -- it was much more complicated before.

I make no promises this will work out in practice: If there are Java version problems, I don't know what sorts of errors users will see, but at least you can run the "launcher" or the java command from the command line and see what's going on, as opposed to pop-up mystery messages like "I think you need to install legacy Java 6 but I'm not going to tell you why." 
 
-------------------------------------------------------------------

Documentation from original implementation by
  Dave Mowatt, dmowatt@andrew.cmu.edu

Source Code:

	Main.java - main executable
		Creates a MainFrame swing window

	*.java - All the ide files are in this one directory
	MainFrame.java - root frame for the IDE
		This uses a basic swing interface, with a menu bar,
		quick-action buttons, and the basic components of the
		nyquist IDE: the input window (upper left), the output
		window (left side), and a desktop for all the opened files.

	NyquistThread.java - i/o to nyquist executable
		Executes nyquist and maintains the input/output streams
		for communication to and from the input and output windows
		inside of MainFrame.  The executable path (currently
		"nyquist.exe" or "ny" should be set to reflect whatever
		location the nyquist executable is on that particular machine.

	NyquistFile.java - data for one open file in nyquist
		On a new instance, creates an empty file.  Save will write
		to the current file unless it is a new file - where SaveAs
		will automatically be called.  (If the file is new then the
		java File file variable will be null) Whenever a file is
		modified, a * will be placed next to the file's name in the
		title bar.

	LispFileFilter.java* - filters out .lsp files.

	SyntaxThread.java - maintains syntax highlighting for
				      one document
		This is also used on the input window, where the paren
		balencing is used to prevent invalid input to nyquist.

	ColorText.java - text highlighting functions
		formatAll will go through the entire file to find the balanced
		parens, strings, comments, and the keywords.  The keywords are
		currently in a list at the bottom, just append any missing ones
		or read them in from a file.  The format() functions take
		in the syntax thread that is calling them.  This is so they
		will automatically exit as soon as the thread's document is
		changed.  That way if the user types another letter while the
		current check is running, it will stop the current one and
		start a new check from the beginning.


	Pair.java - Stores the x, y coordinates of one data point
		time is along the x axis, v is along the y axis

	NyqPlot.java - Handles reading in the data, finding 
				   mins/maxes
		Pretty straightforward, reads in the data storing the sonud
		data, then creates a graph based on that.

	PlotFrame.java - Plots a graph of the file
		Upon creation, draws the full graph to an image.  From then on,
		parts  of that image are drawn to the double buffered window.
		This is so scrolling can be done easily without re-drawing
		the image again.  The full graph image should be re-drawn
		on scaling.

	PlotMouseAdapter.java* - Handles data for mouse 
						scrolling/zooming
		Counts how far the mouse has been dragged to just store the
		change on how far to scroll.

* not currently used, but the framework is there

----- design notes for SAL -----

to move to SAL, we must support two languages: XLISP and SAL

when you open an edit window, the type, XLISP or SAL, should become
a property of the window. I think we can just assume XLISP unless
the file extension is SAL. Another option would be to try to detect
the file type if the extension is not .lsp, .sal, or .ny. Maybe we could
compute the ratio of parens to non-parens, but this might fail for
data files (lists of numbers, in XLISP syntax), so we could add other
features like counts of SAL keywords, etc. Let's try the simple
file extension test first.

When you open a new file, the editor should know whether it's a
new lisp window or a new sal window. We won't support a new
text window, but users can edit text and ignore syntax coloring
if they want to.

Syntax editor is needed for SAL. Coloring should match SAL examples
on the CLM/SAL web pages, which look very good.

Indentation should be suggested by the editor when you type newline.
It would be great if TAB would indent the line instead of inserting a TAB.

The IDE needs know when the user is in SAL mode or LISP mode. SAL
prompts with "SAL>", so the sequence "\nSAL> " should put the IDE
into SAL mode. In SAL mode, the IDE should expect SAL to be 
entered into the input window. Input should only be parsed when
the user types two newlines in a row -- that allows multiple line
input.

Also, in SAL mode, errors need to be detected, parsed, and used
to position the cursor at the error location in an edit window.

----- notes on Java -----

DETECTING A MAC

    if (System.getProperty("mrj.version") == null) {   
       mb.add(createJUnitMenu());
    } else {                                               
          // the Mac specific code will go here           
    }                                                             

from http://java.sun.com/developer/technicalArticles/JavaLP/JavaToMac2/

	public boolean isMac() {
		return System.getProperty("mrj.version") != null;
	}

from http://www.kfu.com/~nsayer/Java/reflection.html

LOADING CLASSES ONLY ON MAC

That is, we really want to do this:

	if (isMac()) {
		new SpecialMacHandler(this);
	}
but we will land in hot water because the class loader doesn't know what isMac() will return. It will only see that SpecialMacHandler wants com.apple.mrj.MRJApplicationUtils and fail to start the program on non-Macs. We must achieve the same end without referencing the class directly in the code. Reflection offers us the way. The reflected version of the same code looks like this:

	if (isMac()) {
		try {
			Object[] args = { this };
			Class[] arglist = { Program.class };
			Class mac_class = class.forName("SpecialMacHandler");
			Constructor new_one = mac_class.getConstructor(arglist);
			new_one.newInstance(args);
		}
		catch(Exception e) {
			System.out.println(e);
		}
	}

from http://www.kfu.com/~nsayer/Java/reflection.html


FILE CONFIGURATIONS AFTER INSTALLATION

On OS X, we should have (in the same directory):
   nyquist/demos -- link to NyquistIDE/Contents/Java/demos
   nyquist/lib -- link to NyquistIDE/Contents/Java/lib
   nyquist/doc -- documentation files
   NyquistIDE/Contents/Java/NyquistWords.txt -- command completion
       text
   NyquistIDE/Contents/Java/mac-os-x-link-script.sh -- script to make 
       links to demos & lib
   NyquistIDE/Contents/Java/ny -- executable 
   NyquistIDE/Contents/Java/runtime -- the Nyquist lisp sources

