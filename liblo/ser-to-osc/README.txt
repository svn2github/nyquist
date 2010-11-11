ser-to-osc reads a serial port and sends OSC messages to Nyquist.

usage: ser-to-osc [-q] [input-device]

The default input is stdin, but another device can be specified
on the command line. The optional -q switch means "quiet": do not echo input.


The input format is 
Channel <slider-num> <value> <newline>
where <slider-num> is an ascii decimal integer, and
<value> is an ascii decimal integer between 0 and 255.

The input values are translated to the range 0-1.

On the Mac, you build this with the ser-to-osc target in Xcode.
You can find the executable in:
     nyquist/macosxproject/build/Development/ser-to-osc
and you can run this from a terminal window (or with Xcode)


