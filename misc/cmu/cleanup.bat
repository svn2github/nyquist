rem erase everything but source so we can save sources to web

cd ..\..

rmdir /s /q nyqrel
rmdir /s /q nyqrelide
rmdir /s /q nyqrelwin

del *.pac
del *.ncb
del unpacker.exe
del packer.exe

rmdir /s /q ..\nyquist-backup
mkdir ..\nyquist-backup
move idesetup ..\nyquist-backup
move winsetup ..\nyquist-backup
move setup ..\nyquist-backup

move NyqWinDebug ..\nyquist-backup
move NyqWinRel ..\nyquist-backup
move WinDebug ..\nyquist-backup
move WinRel ..\nyquist-backup

del nyqide\nyquist.exe
del nyquist.opt

rmdir /s /q misc\filelist_Debug
rmdir /s /q misc\intgen_Win32\WinDebug
rmdir /s /q misc\packer_Debug
rmdir /s /q misc\unpacker_Debug

cd misc\cmu

