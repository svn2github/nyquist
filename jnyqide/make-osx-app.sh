#!/bin/sh

SRC_ROOT=..
INSTALL_ROOT=$SRC_ROOT/nyquist-install
rm -rf $INSTALL_ROOT
NYQUIST_ROOT=$INSTALL_ROOT/nyquist
mkdir -p $INSTALL_ROOT $NYQUIST_ROOT

JNYQIDE_ROOT=.
# Note: $CONFIGURATION comes from Xcode
echo "Reading from src root: $SRC_ROOT, CONFIGURATION=$CONFIGURATION"

NYQUIST_APP=${INSTALL_ROOT}/NyquistIDE.app
CONTENTS_FOLDER=$NYQUIST_APP/Contents
RES_FOLDER=$CONTENTS_FOLDER/Resources
JAVA_FOLDER=$CONTENTS_FOLDER/Java
EXE_FOLDER=$CONTENTS_FOLDER/MacOS

cd $INSTALL_ROOT

mkdir -p $NYQUIST_APP $CONTENTS_FOLDER $RES_FOLDER $JAVA_FOLDER $EXE_FOLDER

cd ../jnyqide

echo "BEFORE make-osx-app.sh"
ls -l $EXE_FOLDER

cd $JNYQIDE_ROOT
echo "current directory (should be JNYQIDE_ROOT):"
pwd
BOOTCLASSPATH=/Library/Java/JavaVirtualMachines/1.6.0.jdk/Contents/Home/lib/jce.jar:/Library/Java/JavaVirtualMachines/1.6.0.jdk/Contents/Home/lib/dt.jar

# execute immediately on compilation error
set -e
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
cp $SRC_ROOT/$CONFIGURATION/ny $JAVA_FOLDER/ny

# cp ${JNYQIDE_ROOT}/mac-os-x-link-script.sh $JAVA_FOLDER
cp ${JNYQIDE_ROOT}/closefile.gif $JAVA_FOLDER/
cp ${JNYQIDE_ROOT}/help.gif $JAVA_FOLDER/
cp ${JNYQIDE_ROOT}/openfile.gif $JAVA_FOLDER/
cp ${JNYQIDE_ROOT}/NyquistWords.txt $JAVA_FOLDER/

rm -rf ${NYQUIST_ROOT}
mkdir ${NYQUIST_ROOT}
svn export ${SRC_ROOT}/runtime $NYQUIST_ROOT/runtime
svn export ${SRC_ROOT}/lib $NYQUIST_ROOT/lib
rm -rf ${NYQUIST_ROOT}/lib/moog
svn export ${SRC_ROOT}/demos ${NYQUIST_ROOT}/demos
svn export ${SRC_ROOT}/doc ${NYQUIST_ROOT}/doc
cp ${SRC_ROOT}/sys/mac/README.txt ${NYQUIST_ROOT}/doc/
cp ${SRC_ROOT}/sys/unix/osx/system.lsp ${NYQUIST_ROOT}/runtime

rm -rf ${SRC_ROOT}/nyquist-install.dmg
rm -rf ${SRC_ROOT}/tmp.dmg
mkdir ${INSTALL_ROOT}/.background
cp ${SRC_ROOT}/misc/installer-background.jpg ${INSTALL_ROOT}/.background/background.jpg
cp ${SRC_ROOT}/misc/installer-DS_Store ${INSTALL_ROOT}/.DS_Store
pushd ${SRC_ROOT}
hdiutil makehybrid -hfs -hfs-volume-name nyquist-install -hfs-openfolder nyquist-install nyquist-install -o tmp.dmg
hdiutil convert -format UDZO tmp.dmg -o nyquist-install.dmg
rm -rf Icon.rsrc
echo "read 'icns' (-16455) \"macosxproject/Nyquist.icns\";" >> Icon.rsrc
Rez -a Icon.rsrc -o nyquist-install.dmg
SetFile -a C nyquist-install.dmg
rm Icon.rsrc
popd
echo "created nyquist-install.dmg -- maybe you should add version number"
echo "ENDING make-osx-app.sh"
