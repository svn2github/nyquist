rmdir /s /q nyqrelwin
mkdir nyqrelwin
xcopy runtime nyqrelwin\runtime /s /i
del nyqrelwin\runtime\CVS /q
rmdir nyqrelwin\runtime\CVS /q
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
copy NyqWinRel\nyqwin.exe nyqrelwin
copy advantages nyqrelwin
copy readme.txt nyqrelwin
copy todo.txt nyqrelwin
copy license.txt nyqrelwin

