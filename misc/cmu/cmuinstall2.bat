copy ..\..\setup\setupnyqrun.exe q:\web\nyquist\setupnyqrun231.exe
copy ..\..\setup\setupnyqwinrun.exe q:\web\nyquist\setupnyqwinrun231.exe
copy ..\..\setup\setupnyqiderun.exe q:\web\nyquist\setupnyqiderun231.exe
copy new.html q:\web\music.software.html
call cleanup.bat
echo "In d:\rbd, make nyquist.zip from nyquist now...then type return to the pause..."
pause
move ..\..\..\nyquist.zip ..\..\..\nyquist231.zip
copy ..\..\..\nyquist231.zip q:\web\nyquist\nyqsrc231.zip
call restore.bat
