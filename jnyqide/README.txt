15-392 Final Project Description

Dave Mowatt	dmowatt@andrew.cmu.edu

Source Code:

	Main.java - main executable
		Creates a MainFrame swing window

	nyquist/ - All the ide files for text editing
	nyquist/MainFrame.java - root frame for the IDE
		This uses a basic swing interface, with a menu bar,
		quick-action buttons, and the basic components of the
		nyquist IDE: the input window (upper left), the output
		window (left side), and a desktop for all the opened files.

	nyquist/NyquistThread.java - i/o to nyquist executable
		Executes nyquist and maintains the input/output streams
		for communication to and from the input and output windows
		inside of MainFrame.  The executable path (currently
		"nyquist.exe" or "ny" should be set to reflect whatever
		location the nyquist executable is on that particular machine.

	nyquist/NyquistFile.java - data for one open file in nyquist
		On a new instance, creates an empty file.  Save will write
		to the current file unless it is a new file - where SaveAs
		will automatically be called.  (If the file is new then the
		java File file variable will be null) Whenever a file is
		modified, a * will be placed next to the file's name in the
		title bar.

	nyquist/LispFileFilter.java* - filters out .lsp files.

	colortext/ - All the files relating to text coloring and
			paren checking
	colortext/SyntaxThread.java - maintains syntax highlighting for
				      one document
		This is also used on the input window, where the paren
		balencing is used to prevent invalid input to nyquist.

	colortext/ColorText.java - text highlighting functions
		formatAll will go through the entire file to find the balanced
		parens, strings, comments, and the keywords.  The keywords are
		currently in a list at the bottom, just append any missing ones
		or read them in from a file.  The format() functions take
		in the syntax thread that is calling them.  This is so they
		will automatically exit as soon as the thread's document is
		changed.  That way if the user types another letter while the
		current check is running, it will stop the current one and
		start a new check from the beginning.


	nyquistplot/  - All the files for plotting
	nyquistplot/Pair.java - Stores the x, y coordinates of one data point
		time is along the x axis, v is along the y axis

	nyquistplot/NyqPlot.java - Handles reading in the data, finding 
				   mins/maxes
		Pretty straightforward, reads in the data storing the sonud
		data, then creates a graph based on that.

	nyquistplot/PlotFrame.java - Plots a graph of the file
		Upon creation, draws the full graph to an image.  From then on,
		parts  of that image are drawn to the double buffered window.
		This is so scrolling can be done easily without re-drawing
		the image again.  The full graph image should be re-drawn
		on scaling.

	nyquistplot/PlotMouseAdapter.java* - Handles data for mouse 
						scrolling/zooming
		Counts how far the mouse has been dragged to just store the
		change on how far to scroll.

* not currently used, but the framework is there
