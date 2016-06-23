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

mkdir -p $NYQUIST_APP $CONTENTS_FOLDER $RES_FOLDER $JAVA_FOLDER $EXE_FOLDER

echo "BEFORE make-osx-app.sh"
ls -l $EXE_FOLDER

cd $JNYQIDE_ROOT
echo "current directory (should be JNYQIDE_ROOT):"
pwd
javac -XDignore.symbol.file=true *.java 

# Important: run jar in parent dir because everything is in package jnyqide
cd ..
jar -cfm jnyqide/jnyqide.jar jnyqide/manifest.txt jnyqide/*.class
cd jnyqide

cp Info.plist $CONTENTS_FOLDER
cp open_jnyqide_jar $EXE_FOLDER
chmod +x $EXE_FOLDER/open_jnyqide_jar

echo "copied open_jnyqide_jar to $EXE_FOLDER"
ls -l $EXE_FOLDER

cp ${SRC_ROOT}/macosxproject/Nyquist.icns $RES_FOLDER
cp jnyqide.jar $JAVA_FOLDER/
cp $SRC_ROOT/macosxproject/build/$CONFIGURATION/ny $JAVA_FOLDER/ny

cp ${JNYQIDE_ROOT}/mac-os-x-link-script.sh $JAVA_FOLDER
cp ${JNYQIDE_ROOT}/closefile.gif $JAVA_FOLDER/
cp ${JNYQIDE_ROOT}/help.gif $JAVA_FOLDER/
cp ${JNYQIDE_ROOT}/openfile.gif $JAVA_FOLDER/
cp ${JNYQIDE_ROOT}/NyquistWords.txt $JAVA_FOLDER/

rm -rf $JAVA_FOLDER/{runtime,lib,demos,doc}
svn export ${SRC_ROOT}/runtime $JAVA_FOLDER/runtime
svn export ${SRC_ROOT}/lib $JAVA_FOLDER/lib
svn export ${SRC_ROOT}/demos $JAVA_FOLDER/demos
svn export ${SRC_ROOT}/doc $JAVA_FOLDER/doc
cp ${SRC_ROOT}/sys/unix/osx/system.lsp $JAVA_FOLDER/

echo "ENDING make-osx-app.sh"
ls -l $EXE_FOLDER
