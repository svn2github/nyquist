# Microsoft Developer Studio Project File - Name="nyquist" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Console Application" 0x0103

CFG=nyquist - Win32 Release
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "nyquist.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "nyquist.mak" CFG="nyquist - Win32 Release"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "nyquist - Win32 Release" (based on "Win32 (x86) Console Application")
!MESSAGE "nyquist - Win32 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
RSC=rc.exe

!IF  "$(CFG)" == "nyquist - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir ".\WinRel"
# PROP BASE Intermediate_Dir ".\WinRel"
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir ".\WinRel"
# PROP Intermediate_Dir ".\WinRel"
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /FR /YX /c
# ADD CPP /nologo /MT /W3 /GX /O2 /I ".\xlisp" /I ".\snd" /I ".\nyqsrc" /I ".\tran" /I ".\cmt" /I ".\nyqstk" /I ".\nyqstk\include" /I ".\sys\win\msvc" /I ".\fft" /I ".\portaudio\pa_common" /D "NDEBUG" /D "WIN32" /D "_CONSOLE" /D "CMTSTUFF" /D "PA_NO_ASIO" /D "PA_NO_DS" /D "STK_NYQUIST" /YX /FD /c
# SUBTRACT CPP /Fr
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib /nologo /subsystem:console /machine:I386
# ADD LINK32 wsock32.lib winmm.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /machine:I386

!ELSEIF  "$(CFG)" == "nyquist - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir ".\WinDebug"
# PROP BASE Intermediate_Dir ".\WinDebug"
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir ".\WinDebug"
# PROP Intermediate_Dir ".\WinDebug"
# PROP Ignore_Export_Lib 0
# ADD BASE CPP /nologo /W3 /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /FR /YX /c
# ADD CPP /nologo /MTd /W3 /Gm /GX /ZI /Od /I ".\xlisp" /I ".\snd" /I ".\nyqsrc" /I ".\tran" /I ".\cmt" /I ".\nyqstk" /I ".\nyqstk\include" /I ".\sys\win\msvc" /I ".\fft" /I ".\portaudio\pa_common" /D "_DEBUG" /D "STK_NYQUIST" /D "__LITTLE_ENDIAN__" /D "WIN32" /D "_CONSOLE" /D "CMTSTUFF" /D "PA_NO_ASIO" /D "PA_NO_DS" /YX /FD /c
# SUBTRACT CPP /Fr
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib /nologo /subsystem:console /debug /machine:I386
# ADD LINK32 wsock32.lib winmm.lib advapi32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib kernel32.lib user32.lib /nologo /subsystem:console /debug /machine:I386

!ENDIF 

# Begin Target

# Name "nyquist - Win32 Release"
# Name "nyquist - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat;for;f90"
# Begin Source File

SOURCE=.\tran\abs.c
# End Source File
# Begin Source File

SOURCE=.\nyqsrc\add.c
# End Source File
# Begin Source File

SOURCE=.\tran\allpoles.c
# End Source File
# Begin Source File

SOURCE=.\tran\alpass.c
# End Source File
# Begin Source File

SOURCE=.\tran\alpasscv.c
# End Source File
# Begin Source File

SOURCE=.\tran\alpassvv.c
# End Source File
# Begin Source File

SOURCE=.\tran\amosc.c
# End Source File
# Begin Source File

SOURCE=.\tran\areson.c
# End Source File
# Begin Source File

SOURCE=.\tran\aresoncv.c
# End Source File
# Begin Source File

SOURCE=.\tran\aresonvc.c
# End Source File
# Begin Source File

SOURCE=.\tran\aresonvv.c
# End Source File
# Begin Source File

SOURCE=.\tran\atone.c
# End Source File
# Begin Source File

SOURCE=.\tran\atonev.c
# End Source File
# Begin Source File

SOURCE=.\snd\audiopa.c
# End Source File
# Begin Source File

SOURCE=.\nyqsrc\avg.c
# End Source File
# Begin Source File

SOURCE=.\tran\biquad.c
# End Source File
# Begin Source File

SOURCE=.\tran\buzz.c
# End Source File
# Begin Source File

SOURCE=.\cmt\cext.c
# End Source File
# Begin Source File

SOURCE=.\tran\chase.c
# End Source File
# Begin Source File

SOURCE=.\nyqstk\src\Clarinet.cpp
# End Source File
# Begin Source File

SOURCE=.\cmt\cleanup.c
# End Source File
# Begin Source File

SOURCE=.\tran\clip.c
# End Source File
# Begin Source File

SOURCE=.\cmt\cmdline.c
# End Source File
# Begin Source File

SOURCE=.\cmt\cmtcmd.c
# End Source File
# Begin Source File

SOURCE=.\nyqsrc\compose.c
# End Source File
# Begin Source File

SOURCE=.\tran\congen.c
# End Source File
# Begin Source File

SOURCE=.\tran\const.c
# End Source File
# Begin Source File

SOURCE=.\nyqsrc\convolve.c
# End Source File
# Begin Source File

SOURCE=.\tran\coterm.c
# End Source File
# Begin Source File

SOURCE=.\nyqsrc\debug.c
# End Source File
# Begin Source File

SOURCE=.\nyqstk\src\Delay.cpp
# End Source File
# Begin Source File

SOURCE=.\tran\delaycc.c
# End Source File
# Begin Source File

SOURCE=.\tran\delaycv.c
# End Source File
# Begin Source File

SOURCE=.\nyqstk\src\DelayL.cpp
# End Source File
# Begin Source File

SOURCE=.\nyqsrc\downsample.c
# End Source File
# Begin Source File

SOURCE=.\nyqstk\src\Envelope.cpp
# End Source File
# Begin Source File

SOURCE=.\tran\eqbandvvv.c
# End Source File
# Begin Source File

SOURCE=.\tran\exp.c
# End Source File
# Begin Source File

SOURCE=.\xlisp\extern.c
# End Source File
# Begin Source File

SOURCE=.\nyqsrc\falloc.c
# End Source File
# Begin Source File

SOURCE=.\nyqsrc\ffilterkit.c
# End Source File
# Begin Source File

SOURCE=.\nyqsrc\fft.c
# End Source File
# Begin Source File

SOURCE=.\fft\fftn.c
# End Source File
# Begin Source File

SOURCE=.\nyqstk\src\Filter.cpp
# End Source File
# Begin Source File

SOURCE=.\tran\fmosc.c
# End Source File
# Begin Source File

SOURCE=.\tran\follow.c
# End Source File
# Begin Source File

SOURCE=.\tran\fromarraystream.c
# End Source File
# Begin Source File

SOURCE=.\tran\fromobject.c
# End Source File
# Begin Source File

SOURCE=.\tran\gate.c
# End Source File
# Begin Source File

SOURCE=.\nyqsrc\handlers.c
# End Source File
# Begin Source File

SOURCE=.\snd\ieeecvt.c
# End Source File
# Begin Source File

SOURCE=.\tran\ifft.c
# End Source File
# Begin Source File

SOURCE=.\nyqstk\instr.cpp
# End Source File
# Begin Source File

SOURCE=.\tran\instrclar.c
# End Source File
# Begin Source File

SOURCE=.\tran\instrclarall.c
# End Source File
# Begin Source File

SOURCE=.\tran\instrclarfreq.c
# End Source File
# Begin Source File

SOURCE=.\nyqstk\src\Instrmnt.cpp
# End Source File
# Begin Source File

SOURCE=.\tran\instrsax.c
# End Source File
# Begin Source File

SOURCE=.\tran\instrsaxall.c
# End Source File
# Begin Source File

SOURCE=.\tran\instrsaxfreq.c
# End Source File
# Begin Source File

SOURCE=.\tran\integrate.c
# End Source File
# Begin Source File

SOURCE=.\nyqsrc\inverse.c
# End Source File
# Begin Source File

SOURCE=.\nyqsrc\local.c
# End Source File
# Begin Source File

SOURCE=.\tran\log.c
# End Source File
# Begin Source File

SOURCE=.\nyqsrc\lpanal.c
# End Source File
# Begin Source File

SOURCE=.\tran\lpreson.c
# End Source File
# Begin Source File

SOURCE=.\tran\maxv.c
# End Source File
# Begin Source File

SOURCE=.\cmt\mem.c
# End Source File
# Begin Source File

SOURCE=.\cmt\midifile.c
# End Source File
# Begin Source File

SOURCE=.\cmt\midifns.c
# End Source File
# Begin Source File

SOURCE=.\cmt\moxc.c
# End Source File
# Begin Source File

SOURCE=.\nyqsrc\multiread.c
# End Source File
# Begin Source File

SOURCE=.\nyqsrc\multiseq.c
# End Source File
# Begin Source File

SOURCE=.\nyqstk\src\Noise.cpp
# End Source File
# Begin Source File

SOURCE=.\tran\offset.c
# End Source File
# Begin Source File

SOURCE=.\tran\oneshot.c
# End Source File
# Begin Source File

SOURCE=.\nyqstk\src\OneZero.cpp
# End Source File
# Begin Source File

SOURCE=.\tran\osc.c
# End Source File
# Begin Source File

SOURCE=.\tran\partial.c
# End Source File
# Begin Source File

SOURCE=.\xlisp\path.c
# End Source File
# Begin Source File

SOURCE=.\tran\pluck.c
# End Source File
# Begin Source File

SOURCE=.\tran\prod.c
# End Source File
# Begin Source File

SOURCE=.\tran\pwl.c
# End Source File
# Begin Source File

SOURCE=.\tran\quantize.c
# End Source File
# Begin Source File

SOURCE=.\tran\recip.c
# End Source File
# Begin Source File

SOURCE=.\cmt\record.c
# End Source File
# Begin Source File

SOURCE=.\nyqstk\src\ReedTabl.cpp
# End Source File
# Begin Source File

SOURCE=.\nyqsrc\resamp.c
# End Source File
# Begin Source File

SOURCE=.\nyqsrc\resampv.c
# End Source File
# Begin Source File

SOURCE=.\tran\reson.c
# End Source File
# Begin Source File

SOURCE=.\tran\resoncv.c
# End Source File
# Begin Source File

SOURCE=.\tran\resonvc.c
# End Source File
# Begin Source File

SOURCE=.\tran\resonvv.c
# End Source File
# Begin Source File

SOURCE=.\pa\pablio\ringbuffer.c
# End Source File
# Begin Source File

SOURCE=.\tran\sampler.c
# End Source File
# Begin Source File

SOURCE=.\nyqsrc\samples.c
# End Source File
# Begin Source File

SOURCE=.\nyqstk\src\Saxofony.cpp
# End Source File
# Begin Source File

SOURCE=.\tran\scale.c
# End Source File
# Begin Source File

SOURCE=.\cmt\seq.c
# End Source File
# Begin Source File

SOURCE=.\nyqsrc\seqext.c
# End Source File
# Begin Source File

SOURCE=.\nyqsrc\seqfnint.c
# End Source File
# Begin Source File

SOURCE=.\nyqsrc\seqinterf.c
# End Source File
# Begin Source File

SOURCE=.\cmt\seqmread.c
# End Source File
# Begin Source File

SOURCE=.\cmt\seqmwrite.c
# End Source File
# Begin Source File

SOURCE=.\cmt\seqread.c
# End Source File
# Begin Source File

SOURCE=.\cmt\seqwrite.c
# End Source File
# Begin Source File

SOURCE=.\tran\shape.c
# End Source File
# Begin Source File

SOURCE=.\tran\sine.c
# End Source File
# Begin Source File

SOURCE=.\tran\siosc.c
# End Source File
# Begin Source File

SOURCE=.\tran\slope.c
# End Source File
# Begin Source File

SOURCE=.\snd\snd.c
# End Source File
# Begin Source File

SOURCE=.\snd\sndcvt.c
# End Source File
# Begin Source File

SOURCE=.\nyqsrc\sndfail.c
# End Source File
# Begin Source File

SOURCE=.\nyqsrc\sndfnint.c
# End Source File
# Begin Source File

SOURCE=.\snd\sndheader.c
# End Source File
# Begin Source File

SOURCE=.\nyqsrc\sndmax.c
# End Source File
# Begin Source File

SOURCE=.\nyqsrc\sndread.c
# End Source File
# Begin Source File

SOURCE=.\nyqsrc\sndseq.c
# End Source File
# Begin Source File

SOURCE=.\snd\sndwin32.c
# End Source File
# Begin Source File

SOURCE=.\nyqsrc\sndwritepa.c
# End Source File
# Begin Source File

SOURCE=.\nyqsrc\sound.c
# End Source File
# Begin Source File

SOURCE=.\tran\sqrt.c
# End Source File
# Begin Source File

SOURCE=.\nyqsrc\stats.c
# End Source File
# Begin Source File

SOURCE=.\nyqstk\src\Stk.cpp
# End Source File
# Begin Source File

SOURCE=.\nyqstk\stkinit.cpp
# End Source File
# Begin Source File

SOURCE=.\tran\tapf.c
# End Source File
# Begin Source File

SOURCE=.\tran\tapv.c
# End Source File
# Begin Source File

SOURCE=.\cmt\tempomap.c
# End Source File
# Begin Source File

SOURCE=.\cmt\timebase.c
# End Source File
# Begin Source File

SOURCE=.\tran\tone.c
# End Source File
# Begin Source File

SOURCE=.\tran\tonev.c
# End Source File
# Begin Source File

SOURCE=.\tran\upsample.c
# End Source File
# Begin Source File

SOURCE=.\cmt\userio.c
# End Source File
# Begin Source File

SOURCE=.\nyqstk\src\WaveLoop.cpp
# End Source File
# Begin Source File

SOURCE=.\tran\white.c
# End Source File
# Begin Source File

SOURCE=.\sys\win\msvc\winfun.c
# End Source File
# Begin Source File

SOURCE=.\sys\win\msvc\winstuff.c
# End Source File
# Begin Source File

SOURCE=.\nyqstk\src\WvIn.cpp
# End Source File
# Begin Source File

SOURCE=.\xlisp\xlbfun.c
# End Source File
# Begin Source File

SOURCE=.\xlisp\xlcont.c
# End Source File
# Begin Source File

SOURCE=.\xlisp\xldbug.c
# End Source File
# Begin Source File

SOURCE=.\xlisp\xldmem.c
# End Source File
# Begin Source File

SOURCE=.\xlisp\xleval.c
# End Source File
# Begin Source File

SOURCE=.\xlisp\xlfio.c
# End Source File
# Begin Source File

SOURCE=.\xlisp\xlftab.c
# End Source File
# Begin Source File

SOURCE=.\xlisp\xlglob.c
# End Source File
# Begin Source File

SOURCE=.\xlisp\xlimage.c
# End Source File
# Begin Source File

SOURCE=.\xlisp\xlinit.c
# End Source File
# Begin Source File

SOURCE=.\xlisp\xlio.c
# End Source File
# Begin Source File

SOURCE=.\xlisp\xlisp.c
# End Source File
# Begin Source File

SOURCE=.\xlisp\xljump.c
# End Source File
# Begin Source File

SOURCE=.\xlisp\xllist.c
# End Source File
# Begin Source File

SOURCE=.\xlisp\xlmath.c
# End Source File
# Begin Source File

SOURCE=.\xlisp\xlobj.c
# End Source File
# Begin Source File

SOURCE=.\xlisp\xlpp.c
# End Source File
# Begin Source File

SOURCE=.\xlisp\xlprin.c
# End Source File
# Begin Source File

SOURCE=.\xlisp\xlread.c
# End Source File
# Begin Source File

SOURCE=.\xlisp\xlstr.c
# End Source File
# Begin Source File

SOURCE=.\xlisp\xlsubr.c
# End Source File
# Begin Source File

SOURCE=.\xlisp\xlsym.c
# End Source File
# Begin Source File

SOURCE=.\xlisp\xlsys.c
# End Source File
# Begin Source File

SOURCE=.\nyqsrc\yin.c
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl;fi;fd"
# Begin Source File

SOURCE=.\tran\abs.h
# End Source File
# Begin Source File

SOURCE=.\nyqsrc\add.h
# End Source File
# Begin Source File

SOURCE=.\tran\allpoles.h
# End Source File
# Begin Source File

SOURCE=.\tran\alpass.h
# End Source File
# Begin Source File

SOURCE=.\tran\alpassvv.h
# End Source File
# Begin Source File

SOURCE=.\tran\amosc.h
# End Source File
# Begin Source File

SOURCE=.\tran\areson.h
# End Source File
# Begin Source File

SOURCE=.\tran\aresoncv.h
# End Source File
# Begin Source File

SOURCE=.\tran\aresonvc.h
# End Source File
# Begin Source File

SOURCE=.\tran\aresonvv.h
# End Source File
# Begin Source File

SOURCE=.\tran\atone.h
# End Source File
# Begin Source File

SOURCE=.\tran\atonev.h
# End Source File
# Begin Source File

SOURCE=.\snd\audiont.h
# End Source File
# Begin Source File

SOURCE=.\snd\audiowin32.h
# End Source File
# Begin Source File

SOURCE=.\nyqsrc\avg.h
# End Source File
# Begin Source File

SOURCE=.\tran\biquad.h
# End Source File
# Begin Source File

SOURCE=.\tran\buzz.h
# End Source File
# Begin Source File

SOURCE=.\cmt\cext.h
# End Source File
# Begin Source File

SOURCE=.\tran\chase.h
# End Source File
# Begin Source File

SOURCE=.\nyqstk\include\Clarinet.h
# End Source File
# Begin Source File

SOURCE=.\cmt\cleanup.h
# End Source File
# Begin Source File

SOURCE=.\tran\clip.h
# End Source File
# Begin Source File

SOURCE=.\cmt\cmdline.h
# End Source File
# Begin Source File

SOURCE=.\cmt\cmtcmd.h
# End Source File
# Begin Source File

SOURCE=.\nyqsrc\compose.h
# End Source File
# Begin Source File

SOURCE=.\tran\congen.h
# End Source File
# Begin Source File

SOURCE=.\tran\const.h
# End Source File
# Begin Source File

SOURCE=.\nyqsrc\convolve.h
# End Source File
# Begin Source File

SOURCE=.\tran\coterm.h
# End Source File
# Begin Source File

SOURCE=.\nyqsrc\debug.h
# End Source File
# Begin Source File

SOURCE=.\nyqstk\include\Delay.h
# End Source File
# Begin Source File

SOURCE=.\tran\delay.h
# End Source File
# Begin Source File

SOURCE=.\tran\delaycc.h
# End Source File
# Begin Source File

SOURCE=.\tran\delaycv.h
# End Source File
# Begin Source File

SOURCE=.\nyqstk\include\DelayL.h
# End Source File
# Begin Source File

SOURCE=.\nyqsrc\downsample.h
# End Source File
# Begin Source File

SOURCE=.\nyqstk\include\Envelope.h
# End Source File
# Begin Source File

SOURCE=.\tran\exp.h
# End Source File
# Begin Source File

SOURCE=.\xlisp\extern.h
# End Source File
# Begin Source File

SOURCE=.\nyqsrc\falloc.h
# End Source File
# Begin Source File

SOURCE=.\nyqsrc\ffilterkit.h
# End Source File
# Begin Source File

SOURCE=.\nyqsrc\fft.h
# End Source File
# Begin Source File

SOURCE=.\fft\fftn.h
# End Source File
# Begin Source File

SOURCE=.\nyqstk\include\Filter.h
# End Source File
# Begin Source File

SOURCE=.\tran\fmosc.h
# End Source File
# Begin Source File

SOURCE=.\tran\follow.h
# End Source File
# Begin Source File

SOURCE=.\tran\fromarraystream.h
# End Source File
# Begin Source File

SOURCE=.\tran\fromobject.h
# End Source File
# Begin Source File

SOURCE=.\tran\gate.h
# End Source File
# Begin Source File

SOURCE=.\nyqstk\globals.h
# End Source File
# Begin Source File

SOURCE=.\snd\ieeecvt.h
# End Source File
# Begin Source File

SOURCE=.\tran\ifft.h
# End Source File
# Begin Source File

SOURCE=.\nyqstk\instr.h
# End Source File
# Begin Source File

SOURCE=.\tran\instrclar.h
# End Source File
# Begin Source File

SOURCE=.\tran\instrclarfreq.h
# End Source File
# Begin Source File

SOURCE=.\nyqstk\include\Instrmnt.h
# End Source File
# Begin Source File

SOURCE=.\tran\instrsax.h
# End Source File
# Begin Source File

SOURCE=.\tran\instrsaxfreq.h
# End Source File
# Begin Source File

SOURCE=.\tran\integrate.h
# End Source File
# Begin Source File

SOURCE=.\nyqsrc\inverse.h
# End Source File
# Begin Source File

SOURCE=.\tran\log.h
# End Source File
# Begin Source File

SOURCE=.\tran\lpanal.h
# End Source File
# Begin Source File

SOURCE=.\tran\lpreson.h
# End Source File
# Begin Source File

SOURCE=.\tran\maxv.h
# End Source File
# Begin Source File

SOURCE=.\cmt\mem.h
# End Source File
# Begin Source File

SOURCE=.\cmt\midifile.h
# End Source File
# Begin Source File

SOURCE=.\cmt\midifns.h
# End Source File
# Begin Source File

SOURCE=.\cmt\moxc.h
# End Source File
# Begin Source File

SOURCE=.\nyqsrc\multiread.h
# End Source File
# Begin Source File

SOURCE=.\nyqsrc\multiseq.h
# End Source File
# Begin Source File

SOURCE=.\nyqstk\include\Noise.h
# End Source File
# Begin Source File

SOURCE=.\tran\offset.h
# End Source File
# Begin Source File

SOURCE=.\tran\oneshot.h
# End Source File
# Begin Source File

SOURCE=.\nyqstk\include\OneZero.h
# End Source File
# Begin Source File

SOURCE=.\tran\osc.h
# End Source File
# Begin Source File

SOURCE=.\tran\partial.h
# End Source File
# Begin Source File

SOURCE=.\tran\prod.h
# End Source File
# Begin Source File

SOURCE=.\tran\pwl.h
# End Source File
# Begin Source File

SOURCE=.\tran\quantize.h
# End Source File
# Begin Source File

SOURCE=.\tran\recip.h
# End Source File
# Begin Source File

SOURCE=.\cmt\record.h
# End Source File
# Begin Source File

SOURCE=.\nyqstk\include\ReedTabl.h
# End Source File
# Begin Source File

SOURCE=.\nyqsrc\resamp.h
# End Source File
# Begin Source File

SOURCE=.\nyqsrc\resampv.h
# End Source File
# Begin Source File

SOURCE=.\tran\reson.h
# End Source File
# Begin Source File

SOURCE=.\tran\resoncv.h
# End Source File
# Begin Source File

SOURCE=.\tran\resonvc.h
# End Source File
# Begin Source File

SOURCE=.\tran\resonvv.h
# End Source File
# Begin Source File

SOURCE=.\tran\sampler.h
# End Source File
# Begin Source File

SOURCE=.\nyqsrc\samples.h
# End Source File
# Begin Source File

SOURCE=.\nyqstk\include\Saxofony.h
# End Source File
# Begin Source File

SOURCE=.\tran\scale.h
# End Source File
# Begin Source File

SOURCE=.\cmt\seq.h
# End Source File
# Begin Source File

SOURCE=.\nyqsrc\seqext.h
# End Source File
# Begin Source File

SOURCE=.\nyqsrc\seqinterf.h
# End Source File
# Begin Source File

SOURCE=.\cmt\seqmread.h
# End Source File
# Begin Source File

SOURCE=.\cmt\seqmwrite.h
# End Source File
# Begin Source File

SOURCE=.\cmt\seqread.h
# End Source File
# Begin Source File

SOURCE=.\cmt\seqwrite.h
# End Source File
# Begin Source File

SOURCE=.\tran\shape.h
# End Source File
# Begin Source File

SOURCE=.\tran\sine.h
# End Source File
# Begin Source File

SOURCE=.\tran\siosc.h
# End Source File
# Begin Source File

SOURCE=.\tran\slope.h
# End Source File
# Begin Source File

SOURCE=.\snd\snd.h
# End Source File
# Begin Source File

SOURCE=.\snd\sndheader.h
# End Source File
# Begin Source File

SOURCE=.\nyqsrc\sndmax.h
# End Source File
# Begin Source File

SOURCE=.\nyqsrc\sndread.h
# End Source File
# Begin Source File

SOURCE=.\nyqsrc\sndseq.h
# End Source File
# Begin Source File

SOURCE=.\snd\sndwin32.h
# End Source File
# Begin Source File

SOURCE=.\nyqsrc\sndwrite.h
# End Source File
# Begin Source File

SOURCE=.\nyqsrc\sound.h
# End Source File
# Begin Source File

SOURCE=.\tran\sqrt.h
# End Source File
# Begin Source File

SOURCE=.\nyqstk\include\Stk.h
# End Source File
# Begin Source File

SOURCE=.\nyqstk\stkinit.h
# End Source File
# Begin Source File

SOURCE=.\tran\tapv.h
# End Source File
# Begin Source File

SOURCE=.\cmt\tempomap.h
# End Source File
# Begin Source File

SOURCE=.\cmt\timebase.h
# End Source File
# Begin Source File

SOURCE=.\tran\tone.h
# End Source File
# Begin Source File

SOURCE=.\tran\tonev.h
# End Source File
# Begin Source File

SOURCE=.\tran\upsample.h
# End Source File
# Begin Source File

SOURCE=.\cmt\userio.h
# End Source File
# Begin Source File

SOURCE=.\nyqstk\include\WaveLoop.h
# End Source File
# Begin Source File

SOURCE=.\tran\white.h
# End Source File
# Begin Source File

SOURCE=.\sys\win\msvc\winfun.h
# End Source File
# Begin Source File

SOURCE=.\nyqstk\include\WvIn.h
# End Source File
# Begin Source File

SOURCE=.\xlisp\xldmem.h
# End Source File
# Begin Source File

SOURCE=.\xlisp\xlisp.h
# End Source File
# End Group
# Begin Group "Resource Files"

# PROP Default_Filter "ico;cur;bmp;dlg;rc2;rct;bin;cnt;rtf;gif;jpg;jpeg;jpe"
# End Group
# Begin Group "portaudio-c"

# PROP Default_Filter "c"
# Begin Source File

SOURCE=.\portaudio\pa_common\pa_allocation.c
# End Source File
# Begin Source File

SOURCE=.\portaudio\pa_common\pa_converters.c
# End Source File
# Begin Source File

SOURCE=.\portaudio\pa_common\pa_cpuload.c
# End Source File
# Begin Source File

SOURCE=.\portaudio\pa_common\pa_dither.c
# End Source File
# Begin Source File

SOURCE=.\portaudio\pa_common\pa_front.c
# End Source File
# Begin Source File

SOURCE=.\portaudio\pa_common\pa_process.c
# End Source File
# Begin Source File

SOURCE=.\portaudio\pa_common\pa_skeleton.c
# End Source File
# Begin Source File

SOURCE=.\portaudio\pa_common\pa_stream.c
# End Source File
# Begin Source File

SOURCE=.\portaudio\pa_win\pa_win_hostapis.c
# End Source File
# Begin Source File

SOURCE=.\portaudio\pa_win\pa_win_util.c
# End Source File
# Begin Source File

SOURCE=.\portaudio\pa_win_wmme\pa_win_wmme.c
# End Source File
# End Group
# Begin Group "portaudio-h"

# PROP Default_Filter "h"
# Begin Source File

SOURCE=.\portaudio\pa_common\pa_allocation.h
# End Source File
# Begin Source File

SOURCE=.\portaudio\pa_common\pa_converters.h
# End Source File
# Begin Source File

SOURCE=.\portaudio\pa_common\pa_cpuload.h
# End Source File
# Begin Source File

SOURCE=.\portaudio\pa_common\pa_dither.h
# End Source File
# Begin Source File

SOURCE=.\portaudio\pa_common\pa_process.h
# End Source File
# Begin Source File

SOURCE=.\portaudio\pa_common\pa_stream.h
# End Source File
# Begin Source File

SOURCE=.\portaudio\pa_common\pa_util.h
# End Source File
# Begin Source File

SOURCE=.\portaudio\pa_win_wmme\pa_win_wmme.h
# End Source File
# Begin Source File

SOURCE=.\portaudio\pa_common\portaudio.h
# End Source File
# End Group
# End Target
# End Project
