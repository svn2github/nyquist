rem restore what cleanup.bat did

cd ..\..
move ..\nyquist-backup\idesetup idesetup
move ..\nyquist-backup\winsetup winsetup
move ..\nyquist-backup\setup setup
move ..\nyquist-backup\NyqWinDebug NyqWinDebug
move ..\nyquist-backup\NyqWinRel NyqWinRel
move ..\nyquist-backup\WinDebug WinDebug
move ..\nyquist-backup\WinRel WinRel

cd misc\cmu
