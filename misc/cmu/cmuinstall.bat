rem run this to update the nyquist website

copy q:\web\music.software.html music.software.html
rem edit music.software.html and build install.bat
del /q cmuinstall2.bat
..\..\winrel\nyquist.exe
cmuinstall2.bat
