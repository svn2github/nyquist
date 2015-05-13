#!/bin/sh

SRC_ROOT=..
JNYQIDE_ROOT=$SRC_ROOT/jnyqide
# Note: $CONFIGURATION comes from Xcode
echo "Reading from src root: $SRC_ROOT, CONFIGURATION=$CONFIGURATION"

NYQUIST_APP=NyquistIDE.app
CONTENTS_FOLDER=$NYQUIST_APP/Contents
RES_FOLDER=$CONTENTS_FOLDER/Resources
JAVA_FOLDER=$CONTENTS_FOLDER/Java
EXE_FOLDER=$CONTENTS_FOLDER/MacOS

# cp ${SRC_ROOT}/macosxproject/Nyquist.icns $RES_FOLDER
# cp jnyqide.jar $JAVA_FOLDER/
cp $SRC_ROOT/macosxproject/build/$CONFIGURATION/ny $JAVA_FOLDER/ny

cp ${JNYQIDE_ROOT}/mac-os-x-link-script.sh $JAVA_FOLDER
cp ${JNYQIDE_ROOT}/closefile.gif $JAVA_FOLDER/
cp ${JNYQIDE_ROOT}/help.gif $JAVA_FOLDER/
cp ${JNYQIDE_ROOT}/openfile.gif $JAVA_FOLDER/
cp ${JNYQIDE_ROOT}/NyquistWords.txt $JAVA_FOLDER/

svn export ${SRC_ROOT}/runtime $JAVA_FOLDER/runtime
svn export ${SRC_ROOT}/lib $JAVA_FOLDER/lib
svn export ${SRC_ROOT}/demos $JAVA_FOLDER/demos
svn export ${SRC_ROOT}/doc $JAVA_FOLDER/doc
cp ${SRC_ROOT}/sys/unix/osx/system.lsp $JAVA_FOLDER/
