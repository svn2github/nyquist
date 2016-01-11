rmdir /s /q nyqrelide
mkdir nyqrelide
xcopy runtime nyqrelide\runtime /s /i
del nyqrelide\runtime\CVS /q
rmdir nyqrelide\runtime\CVS /q
copy sys\win\msvc\system.lsp nyqrelide\runtime
xcopy doc nyqrelide\doc /s /i
copy sys\win\README.txt nqqrelide\doc\readme-win.txt
del nyqrelide\doc\CVS /q
rmdir nyqrelide\doc\CVS /q
xcopy lib nyqrelide\lib /s /i
del nyqrelide\lib\CVS /q
rmdir nyqrelide\lib\CVS /q
xcopy demos nyqrelide\demos /s /i
del nyqrelide\demos\jones\docs /q
rmdir nyqrelide\demos\jones\docs /q
del nyqrelide\demos\jones\nydoc /q
rmdir nyqrelide\demos\jones\nydoc /q
del nyqrelide\demos\jones\sjlib /q
rmdir nyqrelide\demos\jones\sjlib /q
del nyqrelide\demos\jones /q
rmdir nyqrelide\demos\jones /q
del nyqrelide\demos\plight\kit /q
rmdir nyqrelide\demos\plight\kit /q
del nyqrelide\demos\plight /q
rmdir nyqrelide\demos\plight /q
del nyqrelide\demos\CVS /q
rmdir nyqrelide\demos\CVS /q
del nyqrelide\demos\pmorales\CVS /q
rmdir nyqrelide\demos\pmorales\CVS /q
copy liblo\test-client\Release\osc-test-client.exe nyqrelide
copy liblo\ser-to-osc\Release\ser-to-osc.exe nyqrelide
copy NyqIDE\NyqIDE.exe nyqrelide
copy NyqIDE\doc\Tips.htm nyqrelide\doc
copy NyqIDE\doc\NyquistIDE.gif nyqrelide\doc
copy NyqIDE\doc\nyqide_plot.gif nyqrelide\doc
copy advantages.txt nyqrelide
copy readme.txt nyqrelide
copy todo.txt nyqrelide
copy license.txt nyqrelide
mkdir nyqrelide\jnyqide
copy Release\ny.exe nyqrelide\jnyqide\nyquist.exe
copy jnyqide\NyquistWords.txt nyqrelide\jnyqide
copy jnyqide\jNyqIDE.jar nyqrelide\jnyqide
copy jnyqide.bat nyqrelide\jnyqide.bat
