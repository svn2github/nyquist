copy ..\..\setup\setupnyrun.exe g:\user\rbd\music\web\nyquist\setupnyrun229.exe
copy ..\..\winsetup\setupnywinrun.exe g:\user\rbd\music\web\nyquist\setupnywinrun229.exe
copy ..\..\idesetup\setupnyiderun.exe g:\user\rbd\music\web\nyquist\setupnyiderun229.exe
copy new.html g:\user\rbd\music\web\music.software.html
call cleanup.bat
echo "In d:\rbd, make nyquist.zip from nyquist now..."
pause
move ..\..\..\nyquist.zip ..\..\..\nyquist229.zip
copy ..\..\..\nyquist229.zip g:\user\rbd\music\web\nyquist\nyqsrc229.zip
call restore.bat
