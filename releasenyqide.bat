rmdir /s /q nyqrelide
mkdir nyqrelide
xcopy runtime nyqrelide\runtime /s /i
copy sys\win\msvc\system.lsp nyqrelide\runtime
xcopy doc nyqrelide\doc /s /i
copy sys\win\README.txt nyqrelide\doc\readme-win.txt
xcopy lib nyqrelide\lib /s /i
xcopy demos nyqrelide\demos /s /i
rem copy liblo\test-client\Release\osc-test-client.exe nyqrelide
rem copy liblo\ser-to-osc\Release\ser-to-osc.exe nyqrelide
rem copy NyqIDE\NyqIDE.exe nyqrelide
rem copy NyqIDE\doc\Tips.htm nyqrelide\doc
rem copy NyqIDE\doc\NyquistIDE.gif nyqrelide\doc
rem copy NyqIDE\doc\nyqide_plot.gif nyqrelide\doc
copy advantages.txt nyqrelide
copy Readme.txt nyqrelide
rem copy todo.txt nyqrelide
copy license.txt nyqrelide
mkdir nyqrelide\jnyqide
copy Release\ny.exe nyqrelide\jnyqide\nyquist.exe
copy jnyqide\nycon.png nyqrelide\jnyqide
copy jnyqide\NyquistWords.txt nyqrelide\jnyqide
copy jnyqide\jNyqIDE.jar nyqrelide\jnyqide
copy jnyqide.bat nyqrelide\jnyqide.bat
