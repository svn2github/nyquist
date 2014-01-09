This is libsndfile-1.0.17.
I could not find an easy way to build a universal binary for libsndfile, so
I performed the following steps:

1) configure and build libsndfile-1.0.17 for the Mac on an intel (i386) machine
   (I don't know if this modifies the src directory at all)
2) cd to this directory
2) cp ~/libsndfile-1.0.17/src/*.[ch] .
3) mkdir nylsf/G72x/
4) cp ~/libsndfile-1.0.17/src/G72x/*.[ch] G72x
5) cp ~/libsndfile-1.0.17/src/G72x/README.original G72x
6) mkdir nylsf/GSM610/
7) cp ~/libsndfile-1.0.17/src/GSM610/*.[ch] GSM610
8) cp ~/libsndfile-1.0.17/src/GSM610/README GSM610
9) add libsndfile target to xcode project and add files to project
   except G72x/g72x_test.c, interleave.c, macbinary3.c, macos.c, 
          test_endswap.c, test_file_io.c, test_log_printf.c

10) modify config.h, replacing CPU_IS_LITTLE_ENDIAN and
    CPU_IS_BIG_ENDIAN code to use defines

The file export-nylsf.bat makes a standalone library appart from Nyquist.
I tried to get everything, but flac will not build on x64 architecture.
To work around that, I took out the FLAC library code that's contained
in Nyquist (at present, Jan 2014) and downloaded FLAC 1.3.0. That also
has problems: It complained about ogg/ogg.h. Rather than provide it,
I took out FLAC_HAS_OGG and I added some #if FLAC_HAS_OGG around ENTIRE
ogg*.c files (equivalent to removing source from project).

After building nylsf, run copylibs.bat to put libraries in findable
places expected by Dannenberg's applications.
