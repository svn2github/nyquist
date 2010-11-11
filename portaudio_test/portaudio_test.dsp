# Microsoft Developer Studio Project File - Name="portaudio_test" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Console Application" 0x0103

CFG=portaudio_test - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "portaudio_test.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "portaudio_test.mak" CFG="portaudio_test - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "portaudio_test - Win32 Release" (based on "Win32 (x86) Console Application")
!MESSAGE "portaudio_test - Win32 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
RSC=rc.exe

!IF  "$(CFG)" == "portaudio_test - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /c
# ADD CPP /nologo /W3 /GX /O2 /I "../portaudio/pa_common" /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /c
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /machine:I386

!ELSEIF  "$(CFG)" == "portaudio_test - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /GZ /c
# ADD CPP /nologo /W3 /Gm /GX /ZI /Od /I "../portaudio/pa_common" /D "_CONSOLE" /D "_MBCS" /D "PA_NO_ASIO" /D "PA_NO_DS" /D "WIN32" /D "_DEBUG" /D "DEBUG_INPUT" /YX /FD /GZ /c
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /debug /machine:I386 /pdbtype:sept
# ADD LINK32 ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib kernel32.lib user32.lib winmm.lib /nologo /subsystem:console /debug /machine:I386 /pdbtype:sept

!ENDIF 

# Begin Target

# Name "portaudio_test - Win32 Release"
# Name "portaudio_test - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat"
# Begin Source File

SOURCE=..\portaudio\pa_common\pa_allocation.c
# End Source File
# Begin Source File

SOURCE=..\portaudio\pa_common\pa_converters.c
# End Source File
# Begin Source File

SOURCE=..\portaudio\pa_common\pa_cpuload.c
# End Source File
# Begin Source File

SOURCE=..\portaudio\pa_common\pa_dither.c
# End Source File
# Begin Source File

SOURCE=..\portaudio\pa_common\pa_front.c
# End Source File
# Begin Source File

SOURCE=..\portaudio\pa_common\pa_process.c
# End Source File
# Begin Source File

SOURCE=..\portaudio\pa_common\pa_skeleton.c
# End Source File
# Begin Source File

SOURCE=..\portaudio\pa_common\pa_stream.c
# End Source File
# Begin Source File

SOURCE=..\portaudio\pa_win\pa_win_hostapis.c
# End Source File
# Begin Source File

SOURCE=..\portaudio\pa_win\pa_win_util.c
# End Source File
# Begin Source File

SOURCE=..\portaudio\pa_win_wmme\pa_win_wmme.c
# End Source File
# Begin Source File

SOURCE=..\portaudio\pa_tests\patest_read_record.c
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl"
# Begin Source File

SOURCE=..\portaudio\pa_common\pa_allocation.h
# End Source File
# Begin Source File

SOURCE=..\portaudio\pa_common\pa_converters.h
# End Source File
# Begin Source File

SOURCE=..\portaudio\pa_common\pa_cpuload.h
# End Source File
# Begin Source File

SOURCE=..\portaudio\pa_common\pa_dither.h
# End Source File
# Begin Source File

SOURCE=..\portaudio\pa_common\pa_process.h
# End Source File
# Begin Source File

SOURCE=..\portaudio\pa_common\pa_stream.h
# End Source File
# Begin Source File

SOURCE=..\portaudio\pa_common\pa_util.h
# End Source File
# Begin Source File

SOURCE=..\portaudio\pa_win_wmme\pa_win_wmme.h
# End Source File
# Begin Source File

SOURCE=..\portaudio\pa_common\portaudio.h
# End Source File
# End Group
# Begin Group "Resource Files"

# PROP Default_Filter "ico;cur;bmp;dlg;rc2;rct;bin;rgs;gif;jpg;jpeg;jpe"
# End Group
# End Target
# End Project
