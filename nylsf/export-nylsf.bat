rem This batch file, run in BASE/nyquist/nylsf, produces BASE/nylsf,
rem which can be compiled as an independent library.

mkdir ..\..\nylsf

copy aiff.c ..\..\nylsf
copy alaw.c ..\..\nylsf
copy au.c ..\..\nylsf
copy audio_detect.c ..\..\nylsf
copy avr.c ..\..\nylsf
copy broadcast.c ..\..\nylsf
copy caf.c ..\..\nylsf
copy chanmap.c ..\..\nylsf
copy chunk.c ..\..\nylsf
copy command.c ..\..\nylsf
copy common.c ..\..\nylsf
copy dither.c ..\..\nylsf
copy double64.c ..\..\nylsf
copy dwd.c ..\..\nylsf
copy dwvw.c ..\..\nylsf
copy file_io.c ..\..\nylsf
copy flac.c ..\..\nylsf
copy float32.c  ..\..\nylsf
copy gsm610.c ..\..\nylsf
copy htk.c ..\..\nylsf
copy id3.c ..\..\nylsf
copy ima_adpcm.c ..\..\nylsf
copy ima_oki_adpcm.c ..\..\nylsf
copy interleave.c ..\..\nylsf
copy ircam.c ..\..\nylsf
copy macbinary3.c ..\..\nylsf
copy macos.c ..\..\nylsf
copy mat4.c ..\..\nylsf
copy mat5.c ..\..\nylsf
copy mpc2k.c ..\..\nylsf
copy ms_adpcm.c ..\..\nylsf
copy nist.c ..\..\nylsf
copy ogg.c ..\..\nylsf
copy ogg_pcm.c ..\..\nylsf
copy ogg_speex.c ..\..\nylsf
copy ogg_vorbis.c ..\..\nylsf
copy paf.c ..\..\nylsf
copy pcm.c ..\..\nylsf
copy pvf.c ..\..\nylsf
copy raw.c ..\..\nylsf
copy rf64.c ..\..\nylsf
copy rx2.c ..\..\nylsf
copy sd2.c ..\..\nylsf
copy sds.c ..\..\nylsf
copy sf_g72x.c ..\..\nylsf
copy sndfile.c ..\..\nylsf
copy strings.c ..\..\nylsf
copy svx.c ..\..\nylsf
copy txw.c ..\..\nylsf
copy ulaw.c ..\..\nylsf
copy voc.c ..\..\nylsf
copy vox_adpcm.c ..\..\nylsf
copy w64.c ..\..\nylsf
copy wav.c ..\..\nylsf
copy wav_w64.c ..\..\nylsf
copy windows.c ..\..\nylsf
copy wve.c  ..\..\nylsf
copy xi.c  ..\..\nylsf

mkdir ..\..\nylsf\GSM610
copy G72x\g721.c ..\..\nylsf\G72x
copy G72x\g723_16.c ..\..\nylsf\G72x
copy G72x\g723_24.c ..\..\nylsf\G72x
copy G72x\g723_40.c  ..\..\nylsf\G72x
copy G72x\g72x.c ..\..\nylsf\G72x
copy GSM610\add.c ..\..\nylsf\GSM610
copy GSM610\code.c ..\..\nylsf\GSM610
copy GSM610\decode.c ..\..\nylsf\GSM610
copy GSM610\gsm.h ..\..\nylsf\GSM610
copy GSM610\gsm610_priv.h ..\..\nylsf\GSM610
copy GSM610\gsm_create.c ..\..\nylsf\GSM610
copy GSM610\gsm_decode.c ..\..\nylsf\GSM610
copy GSM610\gsm_destroy.c ..\..\nylsf\GSM610
copy GSM610\gsm_encode.c ..\..\nylsf\GSM610
copy GSM610\gsm_option.c ..\..\nylsf\GSM610
copy GSM610\long_term.c ..\..\nylsf\GSM610
copy GSM610\lpc.c ..\..\nylsf\GSM610
copy GSM610\preprocess.c ..\..\nylsf\GSM610
copy GSM610\rpe.c ..\..\nylsf\GSM610
copy GSM610\short_term.c ..\..\nylsf\GSM610
copy GSM610\table.c ..\..\nylsf\GSM610
copy chanmap.h ..\..\nylsf
copy common.h ..\..\nylsf
copy ima_oki_adpcm.h ..\..\nylsf
copy ogg.h ..\..\nylsf
copy sfconfig.h ..\..\nylsf
copy sfendian.h ..\..\nylsf
copy sf_unistd.h ..\..\nylsf
copy sndfile.h ..\..\nylsf
copy wav_w64.h ..\..\nylsf
copy G72x\g72x.h ..\..\nylsf\G72x
copy G72x\g72x_priv.h ..\..\nylsf\G72x
copy config.h ..\..\nylsf
copy ..\sys\win\msvc\switches.h ..\..\nylsf
copy ..\cmt\swlogic.h ..\..\nylsf

copy CMakeLists.txt ..\..\nylsf
copy static.cmake ..\..\nylsf
copy README.txt ..\..\nylsf

mkdir ..\..\nylsf\libogg
mkdir ..\..\nylsf\libogg\include
mkdir ..\..\nylsf\libogg\include\ogg
mkdir ..\..\nylsf\libogg\src
copy ..\libogg\include\ogg\*.h ..\..\nylsf\libogg\include\ogg
copy ..\libogg\src\*.c ..\..\nylsf\libogg\src

mkdir ..\..\nylsf\libvorbis
mkdir ..\..\nylsf\libvorbis\include
mkdir ..\..\nylsf\libvorbis\include\vorbis
mkdir ..\..\nylsf\libvorbis\lib
mkdir ..\..\nylsf\libvorbis\lib\modes
mkdir ..\..\nylsf\libvorbis\lib\books
mkdir ..\..\nylsf\libvorbis\lib\books\coupled
mkdir ..\..\nylsf\libvorbis\lib\books\floor
mkdir ..\..\nylsf\libvorbis\lib\books\uncoupled
copy ..\libvorbis\include\vorbis\*.h ..\..\nylsf\libvorbis\include\vorbis
copy ..\libvorbis\lib\*.c ..\..\nylsf\libvorbis\lib
copy ..\libvorbis\lib\modes\*.h ..\..\nylsf\libvorbis\lib\modes
copy ..\libvorbis\lib\books\coupled\*.h ..\..\nylsf\libvorbis\lib\books\coupled
copy ..\libvorbis\lib\books\floor\*.h ..\..\nylsf\libvorbis\lib\books\floor
copy ..\libvorbis\lib\books\uncoupled\*.h ..\..\nylsf\libvorbis\lib\books\uncoupled
del ..\..\nylsf\libvorbis\lib\barkmel.c
del ..\..\nylsf\libvorbis\lib\psytune.c
del ..\..\nylsf\libvorbis\lib\tone.c
copy ..\libvorbis\lib\*.h ..\..\nylsf\libvorbis\lib

rem FLAC IS TO BE DOWNLOADED AND INSTALLED BESIDE nylsf
rem mkdir ..\..\nylsf\flac
rem mkdir ..\..\nylsf\flac\include
rem mkdir ..\..\nylsf\flac\include\flac
rem mkdir ..\..\nylsf\flac\include\share
rem mkdir ..\..\nylsf\flac\src
rem mkdir ..\..\nylsf\flac\src\libflac
rem mkdir ..\..\nylsf\flac\src\libflac\include
rem mkdir ..\..\nylsf\flac\src\libflac\include\private
rem mkdir ..\..\nylsf\flac\src\libflac\include\protected
rem copy ..\FLAC\include\flac\*.h ..\..\nylsf\flac\include\flac
rem copy ..\FLAC\include\share\*.h ..\..\nylsf\flac\include\share
rem copy ..\FLAC\src\libFLAC\*.c ..\..\nylsf\flac\src\libflac
rem copy ..\FLAC\src\libFLAC\include\private\*.h ..\..\nylsf\flac\src\libflac\include\private
rem copy ..\FLAC\src\libFLAC\include\protected\*.h ..\..\nylsf\flac\src\libflac\include\protected
