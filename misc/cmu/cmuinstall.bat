rem run this to update the nyquist website

copy g:\user\rbd\music\web\music.software.html music.software.html
rem edit music.software.html and build install.bat
del /q cmuinstall2.bat
..\..\nyqrel\nyquist.exe
cmuinstall2.bat
