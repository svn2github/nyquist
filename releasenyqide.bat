rmdir /s /q nyqrelide
mkdir nyqrelide
xcopy runtime nyqrelide\runtime /s /i
del nyqrelide\runtime\CVS /q
rmdir nyqrelide\runtime\CVS /q
xcopy doc nyqrelide\doc /s /i
del nyqrelide\doc\CVS /q
rmdir nyqrelide\doc\CVS /q
xcopy lib nyqrelide\lib /s /i
del nyqrelide\lib\CVS /q
rmdir nyqrelide\lib\CVS /q
xcopy demos nyqrelide\demos /s /i
del nyqrelide\demos\CVS /q
rmdir nyqrelide\demos\CVS /q
del nyqrelide\demos\pmorales\CVS /q
rmdir nyqrelide\demos\pmorales\CVS /q
copy WinRel\nyquist.exe nyqrelide
copy NyqIDE\NyqIDE.exe nyqrelide
copy NyqIDE\doc\Tips.htm nyqrelide\doc
copy NyqIDE\doc\NyquistIDE.gif nyqrelide\doc
copy advantages nyqrelide
copy readme.txt nyqrelide
copy todo.txt nyqrelide
copy license.txt nyqrelide

