rem erase everything but source so we can save sources to web

cd ..\..

rmdir /s /q nyqrel
rmdir /s /q nyqrelide
rmdir /s /q nyqrelwin
rmdir /s /q misc\filelist_Release
rmdir /s /q misc\packer_Release
rmdir /s /q misc\intgen_win32
rmdir /s /q misc\unpacker_Release

del *.pac
del *.ncb
del unpacker.exe
del packer.exe
del mt.dep
del jnyqide\*.class
del nyquist.exe
del jnyqide\*.jar
del *.jar

rmdir /s /q ..\nyquist-backup
mkdir ..\nyquist-backup
move idesetup ..\nyquist-backup
move winsetup ..\nyquist-backup
move setup ..\nyquist-backup

move NyqWinDebug ..\nyquist-backup
move NyqWinRel ..\nyquist-backup
move WinDebug ..\nyquist-backup
move WinRel ..\nyquist-backup
move portaudio_test ..\nyquist-backup
move jones ..\nyquist-backup
move sjlib_103_DOS.tgz ..\nyquist-backup
move sjlib_DOS_104.tgz ..\nyquist-backup
move demos\plight ..\nyquist-backup
move nyqide ..\nyquist-backup

del nyqide\nyquist.exe
del nyquist.opt

rmdir /s /q misc\filelist_Debug
rmdir /s /q misc\intgen_Win32\WinDebug
rmdir /s /q misc\packer_Debug
rmdir /s /q misc\unpacker_Debug
rmdir /s /q liblo\ser-to-osc\Debug
rmdir /s /q liblo\ser-to-osc\Release
rmdir /s /q liblo\test-client\Debug
rmdir /s /q liblo\test-client\Release

cd misc\cmu

