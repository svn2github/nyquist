rmdir /s /q nyqrelwin
mkdir nyqrelwin
xcopy runtime nyqrelwin\runtime /s /i
del nyqrelwin\runtime\CVS /q
rmdir nyqrelwin\runtime\CVS /q
copy sys\win\msvc\system.lsp nyqrelwin\runtime
xcopy doc nyqrelwin\doc /s /i
del nyqrelwin\doc\CVS /q
rmdir nyqrelwin\doc\CVS /q
xcopy lib nyqrelwin\lib /s /i
del nyqrelwin\lib\CVS /q
rmdir nyqrelwin\lib\CVS /q
xcopy demos nyqrelwin\demos /s /i
del nyqrelwin\demos\CVS /q
rmdir nyqrelwin\demos\CVS /q
del nyqrelwin\demos\pmorales\CVS /q
rmdir nyqrelwin\demos\pmorales\CVS /q
del nyqrelwin\demos\jones\docs /q
rmdir nyqrelwin\demos\jones\docs /q
del nyqrelwin\demos\jones\nydoc /q
rmdir nyqrelwin\demos\jones\nydoc /q
del nyqrelwin\demos\jones\sjlib /q
rmdir nyqrelwin\demos\jones\sjlib /q
del nyqrelwin\demos\jones /q
rmdir nyqrelwin\demos\jones /q
del nyqrelwin\demos\plight\kit /q
rmdir nyqrelwin\demos\plight\kit /q
del nyqrelwin\demos\plight /q
rmdir nyqrelwin\demos\plight /q
copy NyqWinRel\nyqwin.exe nyqrelwin
copy liblo\test-client\Release\osc-test-client.exe nyqrelwin
copy liblo\ser-to-osc\Release\ser-to-osc.exe nyqrelwin
copy advantages nyqrelwin
copy readme.txt nyqrelwin
copy todo.txt nyqrelwin
copy license.txt nyqrelwin
copy FLAC\win32\libFLAC.dll nyqrelwin
