rmdir /s /q nyqrel
mkdir nyqrel
xcopy runtime nyqrel\runtime /s /i
del nyqrel\runtime\CVS /q
rmdir nyqrel\runtime\CVS /q
copy sys\win\msvc\system.lsp nyqrel\runtime
xcopy doc nyqrel\doc /s /i
del nyqrel\doc\CVS /q
rmdir nyqrel\doc\CVS /q
xcopy lib nyqrel\lib /s /i
del nyqrel\lib\CVS /q
rmdir nyqrel\lib\CVS /q
xcopy demos nyqrel\demos /s /i
del nyqrel\demos\jones\docs /q
rmdir nyqrel\demos\jones\docs /q
del nyqrel\demos\jones\nydoc /q
rmdir nyqrel\demos\jones\nydoc /q
del nyqrel\demos\jones\sjlib /q
rmdir nyqrel\demos\jones\sjlib /q
del nyqrel\demos\jones /q
rmdir nyqrel\demos\jones /q
del nyqrel\demos\plight\kit /q
rmdir nyqrel\demos\plight\kit /q
del nyqrel\demos\plight /q
rmdir nyqrel\demos\plight /q
del nyqrel\demos\CVS /q
rmdir nyqrel\demos\CVS /q
del nyqrel\demos\pmorales\CVS /q
rmdir nyqrel\demos\pmorales\CVS /q
copy WinRel\nyquist.exe nyqrel
copy liblo\test-client\Release\osc-test-client.exe nyqrel
copy liblo\ser-to-osc\Release\ser-to-osc.exe nyqrel
copy advantages.txt nyqrel
copy readme.txt nyqrel
copy todo.txt nyqrel
copy license.txt nyqrel
