rmdir /s /q nyqrel
mkdir nyqrel
xcopy runtime nyqrel\runtime /s /i
del nyqrel\runtime\CVS /q
rmdir nyqrel\runtime\CVS /q
xcopy doc nyqrel\doc /s /i
del nyqrel\doc\CVS /q
rmdir nyqrel\doc\CVS /q
xcopy lib nyqrel\lib /s /i
del nyqrel\lib\CVS /q
rmdir nyqrel\lib\CVS /q
xcopy demos nyqrel\demos /s /i
del nyqrel\demos\CVS /q
rmdir nyqrel\demos\CVS /q
del nyqrel\demos\pmorales\CVS /q
rmdir nyqrel\demos\pmorales\CVS /q
del nyqrel\demos\pmorales\temp.wav /q
copy WinRel\nyquist.exe nyqrel
copy advantages nyqrel
copy readme.txt nyqrel
copy todo.txt nyqrel
copy license.txt nyqrel
