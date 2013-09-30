cmake_minimum_required(VERSION 2.8.8)

find_package(Java COMPONENTS Development)
include(UseJava)

###########
# OPTIONS #
###########

option(USE_SOURCE_LIBS "Enabling this will disable searching for system installed libraries and instead use source-compiled versions" OFF)

################
# DEPENDENCIES #
################

if(USE_SOURCE_LIBS)

# sndfile
set(ogg_SRCS
  libogg/src/bitwise.c
  libogg/src/framing.c)

add_library(ogg_static STATIC ${ogg_SRCS})
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
set_property(TARGET vorbis_static 
  PROPERTY INCLUDE_DIRECTORIES
  ${CMAKE_CURRENT_SOURCE_DIR}/libvorbis/include 
  ${CMAKE_CURRENT_SOURCE_DIR}/libogg/include)

set(vorbisenc_SRCS
  libvorbis/lib/vorbisenc.c)
add_library(vorbisenc_static STATIC ${vorbisenc_SRCS})
set_property(TARGET vorbisenc_static 
  PROPERTY INCLUDE_DIRECTORIES
  ${CMAKE_CURRENT_SOURCE_DIR}/libvorbis/include 
  ${CMAKE_CURRENT_SOURCE_DIR}/libogg/include 
  ${CMAKE_CURRENT_SOURCE_DIR}/libvorbis/lib)

set(vorbisfile_SRCS
  libvorbis/lib/vorbisfile.c)
add_library(vorbisfile_static STATIC ${vorbisfile_SRCS})
set_property(TARGET vorbisfile_static 
  PROPERTY INCLUDE_DIRECTORIES
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
  FLAC/src/libFLAC/ia32/bitreader_asm.nasm
  FLAC/src/libFLAC/ia32/cpu_asm.nasm
  FLAC/src/libFLAC/ia32/fixed_asm.nasm
  FLAC/src/libFLAC/ia32/lpc_asm.nasm
  FLAC/src/libFLAC/ia32/stream_encoder_asm.nasm
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
  FLAC/src/libFLAC/window.c
  )

add_library(flac_static STATIC ${flac_SRCS})
set_target_properties(flac_static PROPERTIES COMPILE_FLAGS -DVERSION=\\"1.2.1\\") 
set_property(TARGET flac_static APPEND PROPERTY INCLUDE_DIRECTORIES
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
  nylsf/G72x/g72x_test.c
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
set_property(TARGET sndfile_static 
  PROPERTY INCLUDE_DIRECTORIES
  ${CMAKE_CURRENT_SOURCE_DIR}/nylsf
  ${CMAKE_CURRENT_SOURCE_DIR}/sys/unix 
  ${CMAKE_CURRENT_SOURCE_DIR}/FLAC/include 
  ${CMAKE_CURRENT_SOURCE_DIR}/libogg/include 
  ${CMAKE_CURRENT_SOURCE_DIR}/libvorbis/include)

# portaudio
set(portaudio_SRCS 
  portaudio/src/common/pa_ringbuffer.c
  portaudio/src/common/pa_ringbuffer.h
  portaudio/src/os/unix/pa_unix_util.c
  portaudio/src/os/unix/pa_unix_util.h
  portaudio/src/hostapi/coreaudio/pa_mac_core_utilities.c
  portaudio/src/hostapi/coreaudio/pa_mac_core_utilities.h
  portaudio/src/hostapi/coreaudio/pa_mac_core_blocking.c
  portaudio/src/hostapi/coreaudio/pa_mac_core_blocking.h
  portaudio/src/hostapi/coreaudio/pa_mac_core_internal.h
  portaudio/src/hostapi/coreaudio/pa_mac_core.c
  portaudio/src/os/mac_osx/pa_mac_hostapis.c
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
  portaudio/include/pa_mac_core.h
  portaudio/src/common/pa_allocation.c
  portaudio/src/common/pa_converters.c
  portaudio/src/common/pa_cpuload.c
  portaudio/src/common/pa_debugprint.c
  portaudio/src/common/pa_dither.c
  portaudio/src/common/pa_front.c
  portaudio/src/common/pa_process.c
  portaudio/src/common/pa_ringbuffer.c
  portaudio/src/common/pa_skeleton.c
  portaudio/src/common/pa_stream.c
  portaudio/src/common/pa_trace.c
  )

add_library(portaudio_static STATIC ${portaudio_SRCS})
set_property(TARGET portaudio_static
  PROPERTY INCLUDE_DIRECTORIES
  ${CMAKE_CURRENT_SOURCE_DIR}/portaudio/include 
  ${CMAKE_CURRENT_SOURCE_DIR}/portaudio/src/common)

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
set_target_properties(lo_static PROPERTIES COMPILE_FLAGS -DHAVE_CONFIG_H) 
set_property(TARGET lo_static
  PROPERTY INCLUDE_DIRECTORIES
  ${CMAKE_CURRENT_SOURCE_DIR}/liblo 
  ${CMAKE_CURRENT_SOURCE_DIR}/liblo/src 
  ${CMAKE_CURRENT_SOURCE_DIR}/liblo/osx)

else()

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

# portaudio

find_path(PORTAUDIO_INCLUDE_PATH portaudio.h)
find_library(PORTAUDIO_LIBRARY portaudio)

if(NOT PORTAUDIO_LIBRARY)
  message(FATAL_ERROR "Nyquist requires the portaudio library")
endif()

if(PORTAUDIO_INCLUDE_PATH)
  include_directories(${PORTAUDIO_INCLUDE_PATH})
else()
    message(FATAL_ERROR "Could not find portaudio.h") 
endif()

# liblo

find_library(LIBLO_LIBRARY lo)

if(NOT LIBLO_LIBRARY)
  message(FATAL_ERROR "Nyquist requires the liblo library")
endif()

endif()

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
  )

set(tran_SRCS
  tran/abs.c
  tran/alpass.c
  tran/amosc.c
  tran/areson.c
  tran/aresoncv.c
  tran/aresonvc.c
  tran/aresonvv.c
  tran/atone.c
  tran/atonev.c
  tran/buzz.c
  tran/chase.c
  tran/clip.c
  tran/congen.c
  tran/const.c
  tran/coterm.c
  tran/delaycv.c
  tran/exp.c
  tran/fmosc.c
  tran/follow.c
  tran/fromarraystream.c
  tran/fromobject.c
  tran/gate.c
  tran/ifft.c
  tran/integrate.c
  tran/log.c
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
  tran/tapv.c
  tran/tone.c
  tran/tonev.c
  tran/upsample.c
  tran/white.c
  tran/tapf.c
  tran/alpasscv.c
  tran/alpassvv.c
  tran/eqbandvvv.c
  tran/delaycc.c
  tran/alpassvc.c
  tran/allpoles.c
  tran/instrclar.c
  tran/instrsaxall.c
  tran/lpreson.c
  tran/instrclarall.c
  tran/instrsaxfreq.c
  tran/instrclarfreq.c
  tran/instrsax.c
  tran/instrbanded.c
  tran/instrbow.c
  tran/instrbowedfreq.c
  tran/instrflute.c
  tran/instrfluteall.c
  tran/instrflutefreq.c
  tran/instrsitar.c
  tran/stkpitshift.c
  tran/stkrev.c
  tran/biquadfilt.c
  tran/stkchorus.c
  tran/instrmandolin.c
  tran/instrmodalbar.c
  tran/fmfb.c
  tran/fmfbv.c)

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
  cmt/seq.c
  cmt/seqmread.c
  cmt/seqmwrite.c
  cmt/seqread.c
  cmt/seqwrite.c
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

set(liblo_SRCS
  liblo/src/blob.c
  liblo/src/bundle.c
  liblo/src/message.c
  liblo/src/method.c
  liblo/src/pattern_match.c
  liblo/src/send.c
  liblo/src/timetag.c)

set(nyquist_SRCS
  sys/unix/osstuff.c
  sys/unix/term.c
  nyqsrc/debug.c
  nyqsrc/falloc.c
  nyqsrc/add.c
  nyqsrc/local.c
  nyqsrc/downsample.c
  nyqsrc/handlers.c
  nyqsrc/multiread.c
  nyqsrc/multiseq.c
  nyqsrc/samples.c
  nyqsrc/seqext.c
  nyqsrc/seqinterf.c
  nyqsrc/sndread.c
  nyqsrc/sndseq.c
  nyqsrc/sndwritepa.c
  nyqsrc/sndmax.c
  nyqsrc/sound.c
  nyqsrc/stats.c
  nyqsrc/compose.c
  nyqsrc/inverse.c
  nyqsrc/resamp.c
  nyqsrc/resampv.c
  nyqsrc/ffilterkit.c
  nyqsrc/avg.c
  nyqsrc/fft.c
  nyqsrc/seqfnint.c
  nyqsrc/sndfnint.c
  nyqsrc/yin.c
  nyqsrc/convolve.c
  nyqsrc/nyq-osc-server.c
  nyqsrc/pvshell.c
  nyqsrc/phasevocoder.c
  nyqsrc/sliderdata.c
  nyqsrc/sndsliders.c
  nyqsrc/lpanal.c
  nyqsrc/f0.cpp
  nyqsrc/trigger.c
  ffts/src/fftlib.c
  ffts/src/fftext.c
  ffts/src/matlib.c
  snd/ieeecvt.c)


list(APPEND nyquist_SRCS 
  ${xlisp_SRCS} ${tran_SRCS} ${cmt_SRCS} ${nyqstk_SRCS}
  ${liblo_SRCS})

# Add headers using GLOB

file(GLOB xlisp_headers "${CMAKE_CURRENT_SOURCE_DIR}/xlisp/*.h")
file(GLOB tran_headers "${CMAKE_CURRENT_SOURCE_DIR}/tran/*.h")
file(GLOB cmt_headers "${CMAKE_CURRENT_SOURCE_DIR}/cmt/*.h")
file(GLOB nyqstk_headers "${CMAKE_CURRENT_SOURCE_DIR}/nyqstk/*.h")
file(GLOB liblo_headers "${CMAKE_CURRENT_SOURCE_DIR}/liblo/*.h")
file(GLOB nyquist_headers "${CMAKE_CURRENT_SOURCE_DIR}/nyqsrc/*.h")

list(APPEND nyquist_SRCS 
  ${xlisp_headers} ${tran_headers} ${cmt_headers} 
  ${nyqstk_headers} ${nyquist_headers})


#include directories
include_directories(ffts/src snd cmt sys/unix xlisp tran nyqsrc nyqstk nyqstk/include portaudio/include liblo)


# Create executables
add_executable(nyquist ${nyquist_SRCS})

if(USE_SOURCE_LIBS)
  target_link_libraries(nyquist ogg_static)
  target_link_libraries(nyquist vorbis_static)
  target_link_libraries(nyquist vorbisenc_static)
  target_link_libraries(nyquist vorbisfile_static)
  target_link_libraries(nyquist flac_static)
  target_link_libraries(nyquist sndfile_static)
  target_link_libraries(nyquist portaudio_static)
  target_link_libraries(nyquist lo_static)
else()
  target_link_libraries(nyquist ${LIBSNDFILE_LIBRARY})
  target_link_libraries(nyquist ${PORTAUDIO_LIBRARY})
  target_link_libraries(nyquist ${LIBLO_LIBRARY})
endif()

# NYQUIST IDE

if(NOT Java_JAVAC_EXECUTABLE)
  message(WARNING "Java Development tools were not found. Not building Nyquist IDE")

else()
  file(GLOB jnyqide_SRCS "${CMAKE_CURRENT_SOURCE_DIR}/jnyqide/*.java")
  #  file(GLOB jnyqide_RES RELATIVE "${CMAKE_CURRENT_SOURCE_DIR}" "${CMAKE_CURRENT_SOURCE_DIR}/jnyqide/*.txt")
  #  file(GLOB jnyqide_RES2 RELATIVE "${CMAKE_CURRENT_SOURCE_DIR}" "${CMAKE_CURRENT_SOURCE_DIR}/jnyqide/*.gif")

  set(CMAKE_JAVA_COMPILE_FLAGS -target 1.6 -source 1.6)
  set(CMAKE_JAVA_JAR_ENTRY_POINT jnyqide/Main)
  # add_jar(jnyqide SOURCES ${jnyqide_SRCS} ${jnyqide_RES} ${jnyqide_RES2})
  add_jar(jnyqide SOURCES ${jnyqide_SRCS} )

endif()

# APPLE NyquistIde.app

if(APPLE)
  add_custom_target(nyquistide-app
    COMMAND sh ${CMAKE_CURRENT_SOURCE_DIR}/create-osx-app.sh ${CMAKE_CURRENT_SOURCE_DIR}
    DEPENDS nyquist jnyqide)

endif()


################
# FINAL REPORT #
################

message("=====================\nConfiguration Report:\n=====================\n")
message("    * nyquist build target enabled")
if(Java_JAVAC_EXECUTABLE)
  message("    * jnyqide build target enabled")
  if(APPLE)
    message("\n    (To build NyquistIde.app, use 'nyquistide-app' target)")
  endif()
else()
  message("    * jnyqide build target disabled")
endif()
