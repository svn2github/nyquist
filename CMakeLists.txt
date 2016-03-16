cmake_minimum_required(VERSION 2.8.8)
project(nyquist)
find_package(Java COMPONENTS Development)
include(UseJava)

###########
# OPTIONS #
###########

option(USE_SOURCE_LIBS "Enabling this will disable searching for system installed libraries and instead use source-compiled versions" ON)

if(WIN32)
  add_definitions("-D_CRT_SECURE_NO_WARNINGS -D_WINSOCK_DEPRECATED_NO_WARNINGS")
endif(WIN32)

################
# DEPENDENCIES #
################

if(USE_SOURCE_LIBS)

# sndfile
set(ogg_SRCS
   libogg/src/bitwise.c
   libogg/src/framing.c)

add_library(ogg_static STATIC ${ogg_SRCS})
if(WIN32)
  set_target_properties(ogg_static PROPERTIES COMPILE_FLAGS
                        "-DWIN32 -D_WINDOWS")
endif(WIN32)

set_property(TARGET ogg_static 
PROPERTY INCLUDE_DIRECTORIES
${CMAKE_CURRENT_SOURCE_DIR}/libogg/include)

set(vorbis_SRCS
libvorbis/lib/mdct.c
libvorbis/lib/smallft.c
libvorbis/lib/block.c
libvorbis/lib/envelope.c
libvorbis/lib/window.c
libvorbis/lib/lsp.c
libvorbis/lib/lpc.c
libvorbis/lib/analysis.c
libvorbis/lib/synthesis.c
libvorbis/lib/psy.c
libvorbis/lib/info.c
libvorbis/lib/floor1.c
libvorbis/lib/floor0.c
libvorbis/lib/res0.c
libvorbis/lib/mapping0.c
libvorbis/lib/registry.c
libvorbis/lib/codebook.c
libvorbis/lib/sharedbook.c
libvorbis/lib/lookup.c
libvorbis/lib/bitrate.c
)

add_library(vorbis_static STATIC ${vorbis_SRCS})
target_include_directories(vorbis_static PRIVATE
    ${CMAKE_CURRENT_SOURCE_DIR}/libvorbis/include 
    ${CMAKE_CURRENT_SOURCE_DIR}/libogg/include)

set(vorbisenc_SRCS
libvorbis/lib/vorbisenc.c)
add_library(vorbisenc_static STATIC ${vorbisenc_SRCS})
target_include_directories(vorbisenc_static PRIVATE
    ${CMAKE_CURRENT_SOURCE_DIR}/libvorbis/include 
    ${CMAKE_CURRENT_SOURCE_DIR}/libogg/include 
    ${CMAKE_CURRENT_SOURCE_DIR}/libvorbis/lib)

set(vorbisfile_SRCS
libvorbis/lib/vorbisfile.c)
add_library(vorbisfile_static STATIC ${vorbisfile_SRCS})
target_include_directories(vorbisfile_static PRIVATE
    ${CMAKE_CURRENT_SOURCE_DIR}/libvorbis/include 
    ${CMAKE_CURRENT_SOURCE_DIR}/libogg/include 
    ${CMAKE_CURRENT_SOURCE_DIR}/libvorbis/lib)

set(flac_SRCS
  FLAC/src/libFLAC/bitmath.c
  FLAC/src/libFLAC/bitreader.c
  FLAC/src/libFLAC/bitwriter.c
  FLAC/src/libFLAC/cpu.c
  FLAC/src/libFLAC/crc.c
  FLAC/src/libFLAC/fixed.c
  FLAC/src/libFLAC/float.c
  FLAC/src/libFLAC/format.c
#  FLAC/src/libFLAC/ia32/bitreader_asm.nasm
  FLAC/src/libFLAC/ia32/cpu_asm.nasm
  FLAC/src/libFLAC/ia32/fixed_asm.nasm
  FLAC/src/libFLAC/ia32/lpc_asm.nasm
#  FLAC/src/libFLAC/ia32/stream_encoder_asm.nasm
  FLAC/src/libFLAC/lpc.c
  FLAC/src/libFLAC/md5.c
  FLAC/src/libFLAC/memory.c
  FLAC/src/libFLAC/metadata_iterators.c
  FLAC/src/libFLAC/metadata_object.c
  FLAC/src/libFLAC/ogg_decoder_aspect.c
  FLAC/src/libFLAC/ogg_encoder_aspect.c
  FLAC/src/libFLAC/ogg_helper.c
  FLAC/src/libFLAC/ogg_mapping.c
  FLAC/src/libFLAC/stream_decoder.c
  FLAC/src/libFLAC/stream_encoder_framing.c
  FLAC/src/libFLAC/stream_encoder.c
  FLAC/src/libFLAC/window.c)

add_library(flac_static STATIC ${flac_SRCS})
# There's a problem with quotes in XCode, so we need a special case
if(APPLE)
  set_target_properties(flac_static PROPERTIES COMPILE_FLAGS
                        "-DVERSION=\\\\\"1.3.1\\\\\" -DHAVE_LROUND") 
else(APPLE)
  if(WIN32)
    set_target_properties(flac_static PROPERTIES COMPILE_FLAGS
        "-DVERSION=\\\"1.3.1\\\" -DFLAC__NO_DLL -DFLAC__HAS_OGG -DFLAC__CPU_IA32 -DFLAC__USE_3DNOW -DWIN32 -DFLAC__OVERFLOW_DETECT -D_LIB")
  else(WIN32)
    # also, Linux needs to get SIZE_MAX from stdint.h
    set_target_properties(flac_static PROPERTIES COMPILE_FLAGS
              "-DVERSION=\\\"1.3.1\\\" -DHAVE_LROUND -DHAVE_STDINT_H") 
  endif(WIN32)
endif(APPLE)
target_include_directories(flac_static PRIVATE
  ${CMAKE_CURRENT_SOURCE_DIR}/FLAC/src/libFLAC/include/ 
  ${CMAKE_CURRENT_SOURCE_DIR}/FLAC/include/ 
  ${CMAKE_CURRENT_SOURCE_DIR}/libogg/include 
  ${CMAKE_CURRENT_SOURCE_DIR}/FLAC/src/libFLAC/ia32)

set(sndfile_SRCS
  nylsf/ima_oki_adpcm.c
  nylsf/interleave.c
  nylsf/id3.c
  nylsf/audio_detect.c
  nylsf/chanmap.c
  nylsf/sndfile.c
  nylsf/common.c
  nylsf/file_io.c
  nylsf/aiff.c
  nylsf/alaw.c
  nylsf/au.c
  nylsf/avr.c
  nylsf/broadcast.c
  nylsf/caf.c
  nylsf/dither.c
  nylsf/double64.c
  nylsf/dwd.c
  nylsf/flac.c
  nylsf/float32.c
  nylsf/htk.c
  nylsf/mat4.c
  nylsf/mat5.c
  nylsf/nist.c
  nylsf/ogg.c
  nylsf/paf.c
  nylsf/pcm.c
  nylsf/pvf.c
  nylsf/raw.c
  nylsf/rx2.c
  nylsf/sd2.c
  nylsf/sds.c
  nylsf/ircam.c
  nylsf/dwvw.c
  nylsf/vox_adpcm.c
  nylsf/w64.c
  nylsf/wav_w64.c
  nylsf/wav.c
  nylsf/wve.c
  nylsf/xi.c
  nylsf/command.c
  nylsf/strings.c
  nylsf/svx.c
  nylsf/ima_adpcm.c
  nylsf/txw.c
  nylsf/ulaw.c
  nylsf/voc.c
  nylsf/ms_adpcm.c
  nylsf/g72x.c
  nylsf/gsm610.c
  nylsf/G72x/g72x.c
  nylsf/GSM610/gsm_create.c
  nylsf/GSM610/gsm_decode.c
  nylsf/GSM610/gsm_destroy.c
  nylsf/GSM610/gsm_encode.c
  nylsf/GSM610/code.c
  nylsf/GSM610/decode.c
  nylsf/GSM610/gsm_option.c
  nylsf/G72x/g721.c
  nylsf/G72x/g723_16.c
  nylsf/G72x/g723_24.c
  nylsf/G72x/g723_40.c
  nylsf/GSM610/long_term.c
  nylsf/GSM610/lpc.c
  nylsf/GSM610/preprocess.c
  nylsf/GSM610/rpe.c
  nylsf/GSM610/short_term.c
  nylsf/GSM610/table.c
  nylsf/GSM610/add.c
  nylsf/mpc2k.c
  nylsf/chunk.c
  nylsf/G72x/g721.c
  nylsf/G72x/g723_16.c
  nylsf/G72x/g723_24.c
  nylsf/G72x/g723_40.c
  nylsf/G72x/g72x.c
#  nylsf/G72x/g72x_test.c
  nylsf/GSM610/add.c
  nylsf/GSM610/code.c
  nylsf/GSM610/decode.c
  nylsf/GSM610/gsm_create.c
  nylsf/GSM610/gsm_decode.c
  nylsf/GSM610/gsm_destroy.c
  nylsf/GSM610/gsm_encode.c
  nylsf/GSM610/gsm_option.c
  nylsf/GSM610/long_term.c
  nylsf/GSM610/lpc.c
  nylsf/GSM610/preprocess.c
  nylsf/GSM610/rpe.c
  nylsf/GSM610/short_term.c
  nylsf/GSM610/table.c
  nylsf/macbinary3.c
  nylsf/macos.c
  nylsf/ogg_pcm.c
  nylsf/ogg_speex.c
  nylsf/ogg_vorbis.c
  nylsf/rf64.c
  )

add_library(sndfile_static STATIC ${sndfile_SRCS})

if(WIN32)
  set_target_properties(sndfile_static PROPERTIES COMPILE_FLAGS
              "-DOS_IS_WIN32=1")
  set(EXTRA_SNDFILE_INCLUDE ${CMAKE_CURRENT_SOURCE_DIR}/sys/win/msvc)
else(WIN32)
  set(EXTRA_SNDFILE_INCLUDE ${CMAKE_CURRENT_SOURCE_DIR}/sys/unix)
endif(WIN32)


target_include_directories(sndfile_static PRIVATE
  ${CMAKE_CURRENT_SOURCE_DIR}/nylsf
  ${CMAKE_CURRENT_SOURCE_DIR}/FLAC/include 
  ${CMAKE_CURRENT_SOURCE_DIR}/libogg/include 
  ${CMAKE_CURRENT_SOURCE_DIR}/libvorbis/include
  ${CMAKE_CURRENT_SOURCE_DIR}/cmt
  ${EXTRA_SNDFILE_INCLUDE})

# portaudio
set(portaudio_SRCS 
  portaudio/src/common/pa_ringbuffer.c
  portaudio/src/common/pa_ringbuffer.h
  portaudio/src/hostapi/skeleton/pa_hostapi_skeleton.c
#  portaudio/src/os/mac_osx/pa_mac_hostapis.c
  portaudio/include/portaudio.h
  portaudio/src/common/pa_allocation.h
  portaudio/src/common/pa_converters.c
  portaudio/src/common/pa_converters.h
  portaudio/src/common/pa_cpuload.c
  portaudio/src/common/pa_cpuload.h
  portaudio/src/common/pa_debugprint.c
  portaudio/src/common/pa_debugprint.h
  portaudio/src/common/pa_dither.c
  portaudio/src/common/pa_dither.h
  portaudio/src/common/pa_endianness.h
  portaudio/src/common/pa_front.c
  portaudio/src/common/pa_hostapi.h
  portaudio/src/common/pa_process.c
  portaudio/src/common/pa_process.h
  portaudio/src/common/pa_stream.c
  portaudio/src/common/pa_stream.h
  portaudio/src/common/pa_trace.c
  portaudio/src/common/pa_trace.h
  portaudio/src/common/pa_types.h
  portaudio/src/common/pa_util.h
  portaudio/src/common/pa_allocation.c
  portaudio/include/pa_jack.h
#  portaudio/src/common/pa_skeleton.c
  )
if(UNIX)
  set(portaudio_SRCS ${portaudio_SRCS}
      portaudio/src/os/unix/pa_unix_util.c
      portaudio/src/os/unix/pa_unix_util.h
      portaudio/src/os/unix/pa_unix_hostapis.c
      portaudio/src/os/unix/pa_unix_util.c)
  if(APPLE)
    set(portaudio_mac_SRCS 
	  portaudio/include/pa_mac_core.h
      portaudio/src/hostapi/coreaudio/pa_mac_core_utilities.c
      portaudio/src/hostapi/coreaudio/pa_mac_core_utilities.h
      portaudio/src/hostapi/coreaudio/pa_mac_core_blocking.c
      portaudio/src/hostapi/coreaudio/pa_mac_core_blocking.h
      portaudio/src/hostapi/coreaudio/pa_mac_core_internal.h
      portaudio/src/hostapi/coreaudio/pa_mac_core.c)
    set(portaudio_SRCS ${portaudio_SRCS} ${portaudio_mac_SRCS})
  else(APPLE)
    # Linux
    set(portaudio_linux_SRCS 
      portaudio/src/hostapi/alsa/pa_linux_alsa.c
      portaudio/src/hostapi/oss/pa_unix_oss.c)
    set(portaudio_SRCS ${portaudio_SRCS} ${portaudio_linux_SRCS})
  endif(APPLE)
else(UNIX)
  #WIN32
  set(portaudio_SRCS ${portaudio_SRCS} 
      portaudio/src/os/win/pa_win_hostapis.c
      portaudio/src/os/win/pa_win_util.c
      portaudio/src/os/win/pa_win_coinitialize.c
      portaudio/src/os/win/pa_win_coinitialize.h
      portaudio/src/os/win/pa_win_waveformat.c
      portaudio/src/os/win/pa_x86_plain_converters.c
      portaudio/src/os/win/pa_x86_plain_converters.h
      portaudio/src/hostapi/wasapi/pa_win_wasapi.c
      portaudio/src/hostapi/wmme/pa_win_wmme.c)

  # release will use static runtime library
  foreach(flag_var CMAKE_CXX_FLAGS_RELEASE CMAKE_C_FLAGS_RELEASE)
    if(${flag_var} MATCHES "/MD")
      string(REGEX REPLACE "/MD" "/MT" ${flag_var} "${${flag_var}}")
    endif(${flag_var} MATCHES "/MD")
  endforeach(flag_var)
  message(STATUS 
    "Note: overriding CMAKE_*_FLAGS_* to use Visual C static multithread library")
  set(VERBOSE_WARNINGS 0 CACHE BOOL "Do not use _CRT_SECURE_NO_WARNINGS to disable suggestions to use 'secure' versions of strcpy, etc.")
  if(NOT VERBOSE_WARNINGS)
    list(APPEND COMMON_DEFINITIONS "_CRT_SECURE_NO_WARNINGS")
  endif(NOT VERBOSE_WARNINGS)
endif(UNIX)

add_library(portaudio_static STATIC ${portaudio_SRCS})
target_include_directories(portaudio_static PRIVATE
  PROPERTY INCLUDE_DIRECTORIES
  ${CMAKE_CURRENT_SOURCE_DIR}/portaudio/include 
  ${CMAKE_CURRENT_SOURCE_DIR}/portaudio/src/common)

if(UNIX)
  target_include_directories(portaudio_static PRIVATE
          PROPERTY INCLUDE_DIRECTORIES
          ${CMAKE_CURRENT_SOURCE_DIR}/portaudio/src/os/unix)
if(APPLE)
else(APPLE)
    # Linux
    set(USE_ALSA 1 CACHE BOOL "Use ALSA audio rather than OSS")
    
    if(USE_ALSA)
      set(ALSA_FLAGS "-DPA_USE_ALSA=1")
    else(USE_ALSA)
      set(ALSA_FLAGS "")
    endif(USE_ALSA)
    set_target_properties(portaudio_static PROPERTIES COMPILE_FLAGS 
      "-DPA_USE_OSS=1 -DHAVE_LINUX_SOUNDCARD_H ${ALSA_FLAGS}")
endif(APPLE)
else(UNIX)
    # Windows
    set_target_properties(portaudio_static PROPERTIES COMPILE_FLAGS 
            "-DPA_USE_WMME=1 -DPA_USE_WASAPI=1")
    target_include_directories(portaudio_static PRIVATE
            PROPERTY INCLUDE_DIRECTORIES 
            ${CMAKE_CURRENT_SOURCE_DIR}/portaudio/src/os/win)
endif(UNIX)

## portaudio
set(PORTAUDIO_INCLUDE_PATH ../portaudio/include)
set(PORTAUDIO_LIBRARY portaudio_static)
include_directories(${PORTAUDIO_INCLUDE_PATH})
##


# liblo

set(liblo_SRCS
  liblo/src/send.c
  liblo/src/server.c
  liblo/src/address.c
  liblo/src/blob.c
  liblo/src/bundle.c
  liblo/src/message.c
  liblo/src/method.c
  liblo/src/pattern_match.c
  liblo/src/timetag.c
  )

add_library(lo_static STATIC ${liblo_SRCS})
if(UNIX) # including OS X...
set(LIBLO_EXTRA_INCLUDES ${CMAKE_CURRENT_SOURCE_DIR}/liblo/osx)
set_target_properties(lo_static PROPERTIES COMPILE_FLAGS -DHAVE_CONFIG_H)
else(UNIX)
set_target_properties(lo_static PROPERTIES COMPILE_FLAGS 
    "-DPACKAGE_NAME=\\\"liblo\\\" -DHAVE_SELECT -DPRINTF_LL=\\\"ll\\\"")
endif(UNIX)

target_include_directories(lo_static PRIVATE 
  ${CMAKE_CURRENT_SOURCE_DIR}/liblo 
  ${CMAKE_CURRENT_SOURCE_DIR}/liblo/src 
  ${LIBLO_EXTRA_INCLUDES})

else(USE_SOURCE_LIBS)

# sndfile
find_library(LIBSNDFILE_LIBRARY sndfile)

if(NOT LIBSNDFILE_LIBRARY)
  message(FATAL_ERROR "Nyquist requires the sndfile library")
endif()

find_path(SNDFILE_H_PATH sndfile.h)
if(SNDFILE_H_PATH)
    include_directories(${SNDFILE_H_PATH})
else()
    message(FATAL_ERROR "Could not find sndfile.h") 
endif()

if(NOT PORTAUDIO_LIBRARY)
  message(FATAL_ERROR "Nyquist requires the portaudio library")
endif()

# liblo

find_library(LIBLO_LIBRARY lo)

if(NOT LIBLO_LIBRARY)
  message(FATAL_ERROR "Nyquist requires the liblo library")
endif()
 
endif(USE_SOURCE_LIBS)

#######################
# BUILD CONFIGURATION #
#######################

# NYQUIST EXECUTABLE

set(xlisp_SRCS
  xlisp/extern.c
  xlisp/xldmem.c
  xlisp/xlbfun.c
  xlisp/xlcont.c
  xlisp/xldbug.c
  xlisp/xleval.c
  xlisp/xlfio.c
  xlisp/xlftab.c
  xlisp/xlglob.c
  xlisp/xlimage.c
  xlisp/xlinit.c
  xlisp/xlio.c
  xlisp/xlisp.c
  xlisp/xljump.c
  xlisp/xllist.c
  xlisp/xlmath.c
  xlisp/xlobj.c
  xlisp/xlpp.c
  xlisp/xlprin.c
  xlisp/xlread.c
  xlisp/xlstr.c
  xlisp/xlsubr.c
  xlisp/xlsym.c
  xlisp/xlsys.c
  xlisp/path.c
  xlisp/security.c
  )

set(tran_SRCS
  tran/abs.c
  tran/allpoles.c 
  tran/alpass.c 
  tran/alpasscv.c 
  tran/alpassvc.c 
  tran/alpassvv.c 
  tran/amosc.c
  tran/areson.c
  tran/aresoncv.c
  tran/aresonvc.c
  tran/aresonvv.c
  tran/atone.c
  tran/atonev.c
  tran/biquadfilt.c 
  tran/buzz.c
  tran/chase.c
  tran/clip.c
  tran/congen.c
  tran/const.c
  tran/coterm.c
  tran/delaycc.c 
  tran/delaycv.c
  tran/eqbandvvv.c 
  tran/exp.c
  tran/fmfb.c 
  tran/fmfbv.c 
  tran/fmosc.c
  tran/follow.c 
  tran/fromarraystream.c
  tran/fromobject.c 
  tran/gate.c
  tran/ifft.c
  tran/instrbanded.c 
  tran/instrbow.c 
  tran/instrbowedfreq.c 
  tran/instrclar.c 
  tran/instrclarall.c 
  tran/instrclarfreq.c 
  tran/instrflute.c 
  tran/instrfluteall.c 
  tran/instrflutefreq.c 
  tran/instrmandolin.c 
  tran/instrmodalbar.c 
  tran/instrsax.c 
  tran/instrsaxall.c 
  tran/instrsaxfreq.c 
  tran/instrsitar.c 
  tran/integrate.c 
  tran/log.c 
  tran/lpreson.c 
  tran/maxv.c
  tran/offset.c
  tran/oneshot.c
  tran/osc.c
  tran/partial.c
  tran/pluck.c
  tran/prod.c
  tran/pwl.c
  tran/quantize.c
  tran/recip.c
  tran/reson.c
  tran/resoncv.c
  tran/resonvc.c
  tran/resonvv.c
  tran/sampler.c
  tran/scale.c
  tran/shape.c
  tran/sine.c
  tran/siosc.c
  tran/slope.c
  tran/sqrt.c
  tran/tapf.c 
  tran/tapv.c
  tran/tone.c 
  tran/tonev.c
  tran/upsample.c
  tran/white.c
  tran/stkpitshift.c 
  tran/stkrev.c 
  tran/stkchorus.c)

set(seq_SRCS
# out of alphabetical order because seqext.h needed 
# before we generate interface for seq.c:
  nyqsrc/seqext.c 
  cmt/seq.c 
  nyqsrc/seqinterf.c 
  cmt/seqmread.c 
  cmt/seqmwrite.c 
  cmt/seqread.c 
  cmt/seqwrite.c )


set(cmt_SRCS
  cmt/cext.c
  cmt/cleanup.c
  cmt/cmdline.c
  cmt/cmtcmd.c
  cmt/moxc.c
  cmt/mem.c
  cmt/midifile.c
  cmt/midifns.c
  cmt/record.c
  cmt/tempomap.c
  cmt/timebase.c
  cmt/userio.c)

set(nyqstk_SRCS
  nyqstk/instr.cpp
  nyqstk/src/Clarinet.cpp
  nyqstk/src/Delay.cpp
  nyqstk/src/DelayL.cpp
  nyqstk/src/Envelope.cpp
  nyqstk/src/Filter.cpp
  nyqstk/src/Instrmnt.cpp
  nyqstk/src/Noise.cpp
  nyqstk/src/OneZero.cpp
  nyqstk/src/Saxofony.cpp
  nyqstk/src/Stk.cpp
  nyqstk/src/WaveLoop.cpp
  nyqstk/src/WvIn.cpp
  nyqstk/stkinit.cpp
  nyqstk/src/DelayA.cpp
  nyqstk/src/Effect.cpp
  nyqstk/src/FileRead.cpp
  nyqstk/src/FileWvIn.cpp
  nyqstk/src/Flute.cpp
  nyqstk/src/Function.cpp
  nyqstk/src/Generator.cpp
  nyqstk/src/JCRev.cpp
  nyqstk/src/JetTable.cpp
  nyqstk/src/Mandolin.cpp
  nyqstk/src/Modal.cpp
  nyqstk/src/ModalBar.cpp
  nyqstk/src/NRev.cpp
  nyqstk/src/OnePole.cpp
  nyqstk/src/PitShift.cpp
  nyqstk/src/PluckTwo.cpp
  nyqstk/src/PoleZero.cpp
  nyqstk/src/PRCRev.cpp
  nyqstk/src/ReedTable.cpp
  nyqstk/src/SineWave.cpp
  nyqstk/src/Sitar.cpp
  nyqstk/src/ADSR.cpp
  nyqstk/src/BandedWG.cpp
  nyqstk/src/BiQuad.cpp
  nyqstk/src/Bowed.cpp
  nyqstk/src/BowTable.cpp
  nyqstk/src/Chorus.cpp
  nyqstk/stkint.cpp)

set(nyqsrc_SRCS
# put sound.c first (out of alphabetical order) because of dependencies
  nyqsrc/sound.c 
  nyqsrc/add.c 
  nyqsrc/avg.c 
  nyqsrc/compose.c 
  nyqsrc/convolve.c 
  nyqsrc/downsample.c 
  nyqsrc/fft.c 
  nyqsrc/inverse.c 
  nyqsrc/lpanal.c 
  nyqsrc/multiseq.c 
  nyqsrc/nyq-osc-server.c 
  nyqsrc/phasevocoder.c 
  nyqsrc/resamp.c 
  nyqsrc/resampv.c 
  nyqsrc/samples.c 
  nyqsrc/sliderdata.c 
  nyqsrc/sndmax.c 
  nyqsrc/sndread.c 
  nyqsrc/sndseq.c 
  nyqsrc/sndsliders.c 
  nyqsrc/stoponzero.c 
  nyqsrc/trigger.c
  nyqsrc/yin.c 
)

set(nyquist_SRCS
  nyqsrc/debug.c
  nyqsrc/falloc.c
  nyqsrc/local.c
  nyqsrc/handlers.c
  nyqsrc/multiread.c
  nyqsrc/pvshell.c 
  nyqsrc/stats.c
  nyqsrc/ffilterkit.c
  nyqsrc/seqfnint.c
  nyqsrc/sndfnint.c
  nyqsrc/sndwritepa.c 
  nyqsrc/f0.cpp
  ffts/src/fftlib.c
  ffts/src/fftext.c
  ffts/src/matlib.c
  cmupv/src/cmupv.c 
  cmupv/src/internal.c 
  snd/ieeecvt.c)


list(APPEND nyquist_SRCS 
  ${xlisp_SRCS} ${tran_SRCS} ${cmt_SRCS} ${seq_SRCS} 
  ${nyqsrc_SRCS} ${nyqstk_SRCS})

if(UNIX)
  list(APPEND nyquist_SRCS
       sys/unix/osstuff.c
       sys/unix/term.c)
else(UNIX)
  list(APPEND nyquist_SRCS
       sys/win/msvc/winstuff.c
       sys/win/msvc/winfun.c)
endif(UNIX)

# Add headers using GLOB

file(GLOB xlisp_headers "${CMAKE_CURRENT_SOURCE_DIR}/xlisp/*.h")
file(GLOB tran_headers "${CMAKE_CURRENT_SOURCE_DIR}/tran/*.h")
file(GLOB cmt_headers "${CMAKE_CURRENT_SOURCE_DIR}/cmt/*.h")
file(GLOB nyqstk_headers "${CMAKE_CURRENT_SOURCE_DIR}/nyqstk/*.h")
file(GLOB nyquist_headers "${CMAKE_CURRENT_SOURCE_DIR}/nyqsrc/*.h")
set(nyquist_headers ${nyquist_headers} 
                    ${CMAKE_CURRENT_SOURCE_DIR}/ffts/src/fftext.h 
                    ${CMAKE_CURRENT_SOURCE_DIR}/ffts/src/fftlib.h 
                    ${CMAKE_CURRENT_SOURCE_DIR}/cmupv/src/cmupv.h
                    ${CMAKE_CURRENT_SOURCE_DIR}/cmupv/src/internal.h) 
#file(GLOB liblo_headers "${CMAKE_CURRENT_SOURCE_DIR}/liblo/*.h")  
#file(GLOB sndfile_headers "${CMAKE_CURRENT_SOURCE_DIR}/nylsf/sndfile.h")


list(APPEND nyquist_SRCS 
  ${xlisp_headers} ${tran_headers} ${cmt_headers} 
  ${nyqstk_headers} ${nyquist_headers})

##
link_directories(../portaudio/lib/.libs)
##

# Create executables 
add_executable(ny ${nyquist_SRCS}) 

#include directories 
target_include_directories(ny PRIVATE  
    ffts/src snd cmt xlisp tran nyqsrc nyqstk nyqstk/include
    cmupv/src portaudio/include) 

if(UNIX)
    target_include_directories(ny PRIVATE sys/unix)
else(UNIX)
    target_include_directories(ny PRIVATE sys/win/msvc)
endif(UNIX)

if(USE_SOURCE_LIBS)
##  target_include_directories(ny PRIVATE nylsf liblo portaudio/include)
  target_include_directories(ny PRIVATE nylsf liblo)
##
  target_link_libraries(ny ogg_static)
  target_link_libraries(ny vorbis_static)
  target_link_libraries(ny vorbisenc_static)
  target_link_libraries(ny vorbisfile_static)
  target_link_libraries(ny flac_static)
  target_link_libraries(ny sndfile_static)
##  target_link_libraries(ny portaudio_static)
  target_link_libraries(ny ${PORTAUDIO_LIBRARY})
##
  target_link_libraries(ny lo_static)
else()
  target_link_libraries(ny ${LIBSNDFILE_LIBRARY})
  target_link_libraries(ny ${PORTAUDIO_LIBRARY})
  target_link_libraries(ny ${LIBLO_LIBRARY})
endif()

# NYQUIST IDE

if(NOT Java_JAVAC_EXECUTABLE)
  message(WARNING "Java Development tools were not found. Not building Nyquist IDE")

else()
  file(GLOB jnyqide_SRCS "${CMAKE_CURRENT_SOURCE_DIR}/jnyqide/*.java")
  file(GLOB jnyqide_RES RELATIVE "${CMAKE_CURRENT_SOURCE_DIR}" 
            "${CMAKE_CURRENT_SOURCE_DIR}/jnyqide/*.txt")
  file(GLOB jnyqide_RES2 RELATIVE "${CMAKE_CURRENT_SOURCE_DIR}" 
            "${CMAKE_CURRENT_SOURCE_DIR}/jnyqide/*.gif")

  set(CMAKE_JAVA_COMPILE_FLAGS -target 1.6 -source 1.6)
  set(CMAKE_JAVA_JAR_ENTRY_POINT jnyqide/Main)
  # add_jar didn't work because it wants to copy jnyqide/keywords.txt
  # to jnyqide/keywords.txt in the temp directory for making the jar,
  # but jnyqide is not there. It should just be keywords that goes there
  #add_jar(jnyqide SOURCES ${jnyqide_SRCS})
  # I could not figure out how to use java, jar, and appbundler
  # directly, so I'm using ant based on Oracle appbundler example
  if (APPLE)
    add_custom_target(NyquistIDE ALL ant jar
      WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/jnyqide
      COMMENT "Building jnyqide.jar")     
    add_dependencies(NyquistIDE ny)
  else(APPLE)
    if(UNIX) # Linux
      if(USE_ALSA)
	set(SYSTEM_LSP sys/unix/alsa/system.lsp)
      else(USE_ALSA)
	set(SYSTEM_LSP sys/unix/nonalsa/system.lsp)
      endif(USE_ALSA)
      add_custom_target(jNyqIDE ALL 
          mv jnyqide/SpecialMacHandler.java jnyqide/SpecialMacHandler.hidden
        COMMAND javac jnyqide/*.java
        COMMAND 
          mv jnyqide/SpecialMacHandler.hidden jnyqide/SpecialMacHandler.java
        COMMAND rm -f jnyqide/jNyqIDE.jar
        COMMAND 
          jar -cfm jnyqide/jNyqIDE.jar jnyqide/manifest.txt jnyqide/*.class
	COMMAND rm -f jnyqide/ny
        COMMAND ln -s ../ny jnyqide/ny
	COMMAND cp ${SYSTEM_LSP} runtime
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
	COMMENT "Building jnyqide/jNyqIDE.jar")
      add_dependencies(jNyqIDE ny)
    else(UNIX) # WIN32
      add_custom_target(jNyqIDE ALL 
          comp-ide.bat
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
        COMMENT "Building jnyqide/jNyqIDE.jar")
      add_dependencies(jNyqIDE ny)
    endif(UNIX)
  endif(APPLE)
  
endif()

add_definitions("-DOSC")
add_definitions("-DSTK_NYQUIST")

set(EXTRA_NYQUIST_LIBS )
if(UNIX)
  if(APPLE)
    include_directories(${CMAKE_OSX_SYSROOT}/Developer/Headers/FlatCarbon
                        src/sys/mac)
    set(FRAMEWORK_PATH ${CMAKE_OSX_SYSROOT}/System/Library/Frameworks
        CACHE INTERNAL " " FORCE)
    set(COREAUDIO_LIB "${FRAMEWORK_PATH}/CoreAudio.framework")
    set(COREFOUNDATION_LIB "${FRAMEWORK_PATH}/CoreFoundation.framework")
    set(CORESERVICES_LIB "${FRAMEWORK_PATH}/CoreServices.framework")
    set(AUDIOTOOLBOX_LIB "${FRAMEWORK_PATH}/AudioToolbox.framework")
    set(AUDIOUNIT_LIB "${FRAMEWORK_PATH}/AudioUnit.framework")
    set(EXTRA_PA_LIBS ${COREAUDIO_LIB} ${COREFOUNDATION_LIB}
                       ${CORESERVICES_LIB}
                       ${AUDIOTOOLBOX_LIB} ${AUDIOUNIT_LIB})

    # APPLE NyquistIde.app
    add_custom_command(TARGET NyquistIDE 
      POST_BUILD
      COMMAND ant bundle-nyquist
      COMMAND sh ./finish-osx-app.sh
      WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/jnyqide)

    add_definitions("-DPA_USE_COREAUDIO")
    add_definitions("-DCMTSTUFF")
    add_definitions("-Dmacintosh")
  else(APPLE)
    # Linux
    set(EXTRA_PA_LIBS asound pthread m)
  endif(APPLE)
  # useing ../liblo/liblo_s.a. The builder must explicitly make a 
  # static liblo library with --enable-static and copy the result,
  # ../liblo/src/.libs/liblo.a, to ../liblo/liblo_s.a. Otherwise,
  # at least XCode might try to link with /usr/local/lib/liblo.dylib
  # By renaming, we gain confidence as to what is actually being linked.
  #set(LO lo_s) 
  #set(LO_PATH ../liblo)
  #set(PORTMIDI_DBG portmidi_s)
  #set(PORTMIDI_OPT portmidi_s)
else(UNIX)
  # the liblo project builds liblo.lib, not lo.lib
  # Iphlpapi is for GetAdaptersAddresses() in address.c
  set(EXTRA_PA_LIBS winmm.lib)
  set(EXTRA_NYQUIST_LIBS ws2_32.lib Iphlpapi.lib)  
endif(UNIX)

target_link_libraries(ny ${EXTRA_NYQUIST_LIBS} ${EXTRA_PA_LIBS})

# Create args for intgen  
set(sndfnint_args "")  
foreach(arg ${nyqsrc_SRCS} ${tran_SRCS}) 
   string(REPLACE ".c" ".h" arg_h ${arg}) 
    set(sndfnint_args ${sndfnint_args} ${arg_h}) 
endforeach(arg ${nyqsrc_SRCS}) 
# some extra .h files, order is important due to dependencies:
set(sndfnint_args nylsf/sndfile.h ${sndfnint_args} 
                  nyqsrc/sndfmt.h nyqsrc/sndwrite.h) 
# message(status "sndfnint_args: ${sndfnint_args}")  

# Create args for intgen  
set(seqfnint_args "")  
foreach(arg ${seq_SRCS})
   string(REPLACE ".c" ".h" arg_h ${arg}) 
    set(seqfnint_args ${seqfnint_args} ${arg_h}) 
endforeach(arg ${nyqsrc_SRCS}) 
# seqdecls.h has no corresponding .c files, so add
#   them here:
set(seqfnint_args cmt/seqdecls.h ${seqfnint_args}) 
# message(status "seqfnint_args: ${seqfnint_args}")  

add_executable(intgen misc/intgen2.c misc/cmdline.c)
target_include_directories(intgen PRIVATE sys/unix cmt)

add_custom_target(XLISP_C_Interfaces
  COMMAND intgen nyqsrc/sndfnint ${sndfnint_args}
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
  SOURCES ${sndfnint_args} ${seqfnint_args})

add_executable(syncwritetest portaudio_test/syncwrite.c)
target_include_directories(syncwritetest PRIVATE portaudio/include)
target_link_libraries(syncwritetest ${PORTAUDIO_LIBRARY})
target_link_libraries(syncwritetest ${EXTRA_PA_LIBS})
