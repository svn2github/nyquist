# CMakeLists.txt for nylsf
#
# Roger B. Dannenberg, 2014
#
# nylsf is "Nyquist's libsndfile", an adaptation of libsndfile for 
# cross-platform use, particularly in Nyquist.
#
# If you are building Nyquist, ignore this. Nyquist solution files already
# deal with nylsf. 
#
# This file is my attempt to support using libsndfile in other projects,
# but since nylsf has some windows-specific changes for portability,
# I'm putting all the nylsf development code here in nyquist/nylsf to
# avoid a proliferation of versions.
#

cmake_minimum_required(VERSION 2.6)

PROJECT(nylsf)
set(FRIENDS ${CMAKE_CURRENT_LIST_DIR}/..)
set(CMAKE_CONFIGURATION_TYPES "Debug;Release" CACHE STRING 
                              "eliminate MinSizeRel etc." FORCE)
set(LIBSRC
    aiff.c alaw.c au.c audio_detect.c avr.c broadcast.c
    caf.c chanmap.c chunk.c command.c common.c dither.c
    double64.c dwd.c dwvw.c file_io.c flac.c float32.c 
    gsm610.c htk.c id3.c ima_adpcm.c ima_oki_adpcm.c interleave.c
    ircam.c macbinary3.c macos.c mat4.c mat5.c mpc2k.c
    ms_adpcm.c nist.c ogg.c ogg_pcm.c ogg_speex.c ogg_vorbis.c
    paf.c pcm.c pvf.c raw.c rf64.c rx2.c
    sd2.c sds.c sf_g72x.c sndfile.c strings.c svx.c
    txw.c ulaw.c voc.c vox_adpcm.c w64.c wav.c
    wav_w64.c windows.c wve.c xi.c 

    G72x/g721.c G72x/g723_16.c G72x/g723_24.c G72x/g723_40.c 
    G72x/g72x.c
#   g72x_test.c


    GSM610/add.c GSM610/code.c GSM610/decode.c GSM610/gsm.h 
    GSM610/gsm610_priv.h GSM610/gsm_create.c GSM610/gsm_decode.c 
    GSM610/gsm_destroy.c GSM610/gsm_encode.c GSM610/gsm_option.c 
    GSM610/long_term.c GSM610/lpc.c GSM610/preprocess.c GSM610/rpe.c
    GSM610/short_term.c GSM610/table.c

    chanmap.h common.h ima_oki_adpcm.h ogg.h sfconfig.h
    sfendian.h sf_unistd.h sndfile.h wav_w64.h config.h

    G72x/g72x.h G72x/g72x_priv.h

    libogg/src/bitwise.c
    libogg/src/framing.c
    libogg/include/ogg/config_types.h
    libogg/include/ogg/ogg.h
    libogg/include/ogg/os_types.h

    libvorbis/lib/analysis.c
#    libvorbis/lib/barkmel.c
    libvorbis/lib/bitrate.c
    libvorbis/lib/block.c
    libvorbis/lib/codebook.c
    libvorbis/lib/envelope.c
    libvorbis/lib/floor0.c
    libvorbis/lib/floor1.c
    libvorbis/lib/info.c
    libvorbis/lib/lookup.c
    libvorbis/lib/lpc.c
    libvorbis/lib/lsp.c
    libvorbis/lib/mapping0.c
    libvorbis/lib/mdct.c
    libvorbis/lib/psy.c
#    libvorbis/lib/psytune.c
    libvorbis/lib/registry.c
    libvorbis/lib/res0.c
    libvorbis/lib/sharedbook.c
    libvorbis/lib/smallft.c
    libvorbis/lib/synthesis.c
#    libvorbis/lib/tone.c
    libvorbis/lib/vorbisenc.c
    libvorbis/lib/vorbisfile.c
    libvorbis/lib/window.c

    libvorbis/lib/backends.h
    libvorbis/lib/bitrate.h
    libvorbis/lib/codebook.h
    libvorbis/lib/codec_internal.h
    libvorbis/lib/envelope.h
    libvorbis/lib/highlevel.h
    libvorbis/lib/lookup.h
    libvorbis/lib/lookup_data.h
    libvorbis/lib/lpc.h
    libvorbis/lib/lsp.h
    libvorbis/lib/masking.h
    libvorbis/lib/mdct.h
    libvorbis/lib/misc.h
    libvorbis/lib/os.h
    libvorbis/lib/psy.h
    libvorbis/lib/registry.h
    libvorbis/lib/scales.h
    libvorbis/lib/smallft.h
    libvorbis/lib/window.h

    libvorbis/lib/modes/floor_all.h
    libvorbis/lib/modes/psych_11.h
    libvorbis/lib/modes/psych_16.h
    libvorbis/lib/modes/psych_44.h
    libvorbis/lib/modes/psych_8.h
    libvorbis/lib/modes/residue_16.h
    libvorbis/lib/modes/residue_44.h
    libvorbis/lib/modes/residue_44p51.h
    libvorbis/lib/modes/residue_44u.h
    libvorbis/lib/modes/residue_8.h
    libvorbis/lib/modes/setup_11.h
    libvorbis/lib/modes/setup_16.h
    libvorbis/lib/modes/setup_22.h
    libvorbis/lib/modes/setup_32.h
    libvorbis/lib/modes/setup_44.h
    libvorbis/lib/modes/setup_44p51.h
    libvorbis/lib/modes/setup_44u.h
    libvorbis/lib/modes/setup_8.h
    libvorbis/lib/modes/setup_X.h

    libvorbis/lib/books/coupled/res_books_51.h
    libvorbis/lib/books/coupled/res_books_stereo.h
    libvorbis/lib/books/floor/floor_books.h
    libvorbis/lib/books/uncoupled/res_books_uncoupled.h

    libvorbis/include/vorbis/codec.h
    libvorbis/include/vorbis/vorbisenc.h
    libvorbis/include/vorbis/vorbisfile.h

#    flac/src/libflac/bitmath.c
#    flac/src/libflac/bitreader.c
#    flac/src/libflac/bitwriter.c
#    flac/src/libflac/cpu.c
#    flac/src/libflac/crc.c
#    flac/src/libflac/fixed.c
#    flac/src/libflac/float.c
#    flac/src/libflac/format.c
#    flac/src/libflac/lpc.c
#    flac/src/libflac/md5.c
#    flac/src/libflac/memory.c
#    flac/src/libflac/metadata_iterators.c
#    flac/src/libflac/metadata_object.c
#    flac/src/libflac/ogg_decoder_aspect.c
#    flac/src/libflac/ogg_encoder_aspect.c
#    flac/src/libflac/ogg_helper.c
#    flac/src/libflac/ogg_mapping.c
#    flac/src/libflac/stream_decoder.c
#    flac/src/libflac/stream_encoder.c
#    flac/src/libflac/stream_encoder_framing.c
#    flac/src/libflac/window.c
#
#    flac/src/libflac/include/private/all.h
#    flac/src/libflac/include/private/bitmath.h
#    flac/src/libflac/include/private/bitreader.h
#    flac/src/libflac/include/private/bitwriter.h
#    flac/src/libflac/include/private/cpu.h
#    flac/src/libflac/include/private/crc.h
#    flac/src/libflac/include/private/fixed.h
#    flac/src/libflac/include/private/float.h
#    flac/src/libflac/include/private/format.h
#    flac/src/libflac/include/private/lpc.h
#    flac/src/libflac/include/private/md5.h
#    flac/src/libflac/include/private/memory.h
#    flac/src/libflac/include/private/metadata.h
#    flac/src/libflac/include/private/ogg_decoder_aspect.h
#    flac/src/libflac/include/private/ogg_encoder_aspect.h
#    flac/src/libflac/include/private/ogg_helper.h
#    flac/src/libflac/include/private/ogg_mapping.h
#    flac/src/libflac/include/private/stream_encoder_framing.h
#    flac/src/libflac/include/private/window.h
#
#    flac/src/libflac/include/protected/all.h
#    flac/src/libflac/include/protected/stream_decoder.h
#
#    flac/src/libflac/include/protected/stream_encoder.h
#
#    flac/include/flac/all.h
#    flac/include/flac/assert.h
#    flac/include/flac/callback.h
#    flac/include/flac/export.h
#    flac/include/flac/file_decoder.h
#    flac/include/flac/file_encoder.h
#    flac/include/flac/format.h
#    flac/include/flac/metadata.h
#    flac/include/flac/ordinals.h
#    flac/include/flac/seekable_stream_decoder.h
#    flac/include/flac/seekable_stream_encoder.h
#    flac/include/flac/stream_decoder.h
#    flac/include/flac/stream_encoder.h
#
#    FLAC/include/share/alloc.h
#    FLAC/include/share/getopt.h
#    FLAC/include/share/grabbag.h
#    FLAC/include/share/replaygain_analysis.h
#    FLAC/include/share/replaygain_synthesis.h
#    FLAC/include/share/utf8.h

)

add_library(sndfile-static ${LIBSRC})
set_target_properties(sndfile-static PROPERTIES OUTPUT_NAME "sndfile_s")

# DELETE THIS:
# add_library(sndfile-dynamic SHARED ${LIBSRC})
# set_target_properties(sndfile-dynamic PROPERTIES OUTPUT_NAME "sndfile")
# END DELETE

include_directories(.)
include_directories(../flac/include)
# include_directories(flac/src/libflac/include)
include_directories(libogg/include)
include_directories(libvorbis/include)

if(UNIX)
  if(APPLE)
  else(APPLE)
  endif(APPLE)
else(UNIX)
  if(WIN32)
     # the first time CMake configures, save off CMake's built-in flags
     if(NOT DEFAULT_C_FLAGS)
       set(DEFAULT_C_FLAGS ${CMAKE_C_FLAGS} CACHE 
           STRING "CMake's default C flags" FORCE)
     else(NOT DEFAULT_C_FLAGS)
       message(STATUS "DEFAULT_C_FLAGS not nil: ${DEFAULT_C_FLAGS}")
     endif(NOT DEFAULT_C_FLAGS)

     set(CMAKE_C_FLAGS "${DEFAULT_C_FLAGS} -DFLAC__NO_DLL" CACHE STRING
         "flags for C compiler" FORCE)

     # couldn't get link_directories to work, so use full path:
     # target_link_libraries(sndfile-dynamic 
     #                      ${FRIENDS}/flac/lib/debug/libflac.lib)
     include(static.cmake)
  endif(WIN32)
endif(UNIX)

