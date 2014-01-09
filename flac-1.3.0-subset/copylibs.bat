rem copylibs.bat - move flac libs to places expected by 
rem    Dannenberg's applications

mkdir lib
mkdir lib\release
mkdir lib\debug

copy objs\debug\lib\libFLAC_static.lib lib\debug\libflac_s.lib
copy objs\release\lib\libFLAC_static.lib lib\release\libflac_s.lib
copy objs\debug\lib\win_utf8_io.lib lib\debug\win_utf8_io_s.lib
copy objs\release\lib\win_utf8_io.lib lib\release\win_utf8_io_s.lib

copy x64\Release\libFLAC_dynamic.dll lib\release\libflac.dll
copy x64\Release\libFLAC_dynamic.exp lib\release\libflac.exp
copy x64\Release\libFLAC_dynamic.lib lib\release\libflac.lib
copy x64\Release\libFLAC_dynamic.pdb lib\release\libflac.pdb

copy x64\Debug\libFLAC_dynamic.dll lib\debug\libflac.dll
copy x64\Debug\libFLAC_dynamic.exp lib\debug\libflac.exp
copy x64\Debug\libFLAC_dynamic.lib lib\debug\libflac.lib
copy x64\Debug\libFLAC_dynamic.pdb lib\debug\libflac.pdb

