README.txt -- notes on installing liblo into Nyquist
Roger B. Dannenberg
Feb, 2009

To help manage version skew problems, the Nyquist sources include
a copy of Liblo sources. This additional file contains notes on 
how to integrate Liblo into Nyquist.

Note that Liblo has an LGPL license. To distribute Nyquist 
binaries without sources, you may have to deal with Liblo
licensing restrictions.

How to install and build Liblo.

On OS X (all of these steps should be unnecessary if you simply
         obtain Nyquist sources from CVS and build with Xcode)

 1. Download liblo source tree to a scratch directory.
 2. Copy the entire source tree from scratch to nyquist/liblo
 3. Don't forget to keep the following files and directories
    in nyquist/liblo:
        README.txt (this file)
        ser-to-osc (a directory)
        test-client (a directory)
 4. In the scratch directory, run "sh autogen.sh"
 5. Make nyquist/liblo/osx
 6. Copy scratch/config.h to nyquist/liblo/osx/config.h
 7. Make sure the following are on search paths in Xcode:
    liblo, liblo/src, liblo/osx
 8. NOTE: Xcode will include any .h file that is in the project
    sources, so remove all config.h files from the source tree,
    including those in nylsf, if any.
 9. The liblo sources include <lo/lo_osc_types.h>, but unless
    you have installed sources to a system include path, this
    will not be found. Solution: check the 
    Always Search User Path option in the target preferences
    for all configuration.
10. Add HAVE_CONFIG_H to Preprocessor Macros in 
    All Configurations
11. Some files in liblo that do NOT belong in the liblo library:
    subtest.c
12. Note that Serial.cpp is windows only. Don't put it in the
    ser-to-osc target.
13. It should now build under Xcode.

On Linux: (all of these steps should be unnecessary if you simply
           obtan Nyquist sources from CVS and build with Xcode)

 1. Download liblo source tree to a scratch directory
 2. Copy the entire source tree from scratch to nyquist/liblo
 3. Don't forget to keep the following files and directories
    in nyquist/liblo:
        README.txt (this file)
        ser-to-osc (a directory)
        test-client (a directory)
 4. In nyquist/liblo, run "sh autogen.sh --enable-static --disable-shared"
 5. Run make
