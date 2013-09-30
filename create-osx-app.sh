#!/bin/sh

SRC_ROOT=$1
JNYQIDE_ROOT=$SRC_ROOT/jnyqide
echo "Reading from src root: $SRC_ROOT"

NYQUIST_APP=NyquistIDE.app
CONTENTS_FOLDER=$NYQUIST_APP/Contents
RES_FOLDER=$CONTENTS_FOLDER/Resources
JAVA_FOLDER=$RES_FOLDER/Java
EXE_FOLDER=$CONTENTS_FOLDER/MacOS

rm -r NyquistIDE.app

mkdir -p $EXE_FOLDER
mkdir -p $JAVA_FOLDER

cp ${SRC_ROOT}/macosxproject/Nyquist.icns $RES_FOLDER
cp jnyqide.jar $JAVA_FOLDER/
cp nyquist $JAVA_FOLDER/ny
cp /System/Library/Frameworks/JavaVM.framework/Resources/MacOS/JavaApplicationStub $EXE_FOLDER/NyquistIDE


#cp ${JNYQIDE_ROOT}/mac-os-x-link-script.sh $RES_FOLDER
cp ${JNYQIDE_ROOT}/closefile.gif $JAVA_FOLDER/
cp ${JNYQIDE_ROOT}/help.gif $JAVA_FOLDER/
cp ${JNYQIDE_ROOT}/openfile.gif $JAVA_FOLDER/
cp ${JNYQIDE_ROOT}/NyquistWords.txt $JAVA_FOLDER/

cp -r ${SRC_ROOT}/runtime $JAVA_FOLDER/
cp -r ${SRC_ROOT}/lib $JAVA_FOLDER/
cp -r ${SRC_ROOT}/demos $JAVA_FOLDER/
cp ${SRC_ROOT}/sys/unix/osx/system.lsp $JAVA_FOLDER/

cp ${SRC_ROOT}/macosxproject/NyquistIDE-Info.plist $CONTENTS_FOLDER/Info.plist

echo "APPL????"  > $CONTENTS_FOLDER/PkgInfo
