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
cp ${SRC_ROOT}/advantages.txt $NYQUIST_ROOT/
cp ${SRC_ROOT}/Readme.txt $NYQUIST_ROOT/
cp ${SRC_ROOT}/license.txt $NYQUIST_ROOT/

svn export ${SRC_ROOT}/runtime $NYQUIST_ROOT/runtime
svn export ${SRC_ROOT}/lib $NYQUIST_ROOT/lib
rm -rf ${NYQUIST_ROOT}/lib/moog
svn export ${SRC_ROOT}/demos ${NYQUIST_ROOT}/demos
svn export ${SRC_ROOT}/doc ${NYQUIST_ROOT}/doc
cp ${SRC_ROOT}/sys/mac/README.txt ${NYQUIST_ROOT}/doc/readme-mac.txt
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
rm -rf ${SRC_ROOT}/tmp.dmg
popd
echo "created nyquist-install.dmg -- maybe you should add version number"
echo "ENDING make-osx-app.sh"

# 
# June 2008, Guy Hoffman
# 
# here is the step-by-step instruction to making a dmg to distribute 
# your software that will open in icon mode, with a background image,
# and symbolic links (aliases) to drop your file in.
# 
# for people who don't want to "read more" or trust os x magic.
# 
# just a step-by-step. follow me.
# 
#     Open Disk Utility
#     Press on 'New Image'. Use the following options:
#         Pick a Volume Name - we will from now call it NAME
#         Volume Size - should be enough to contain your files. Note 
#           that you can pick a custom size, not just the preset ones. 
#           Don't worry if it's bigger than you think you need. There's
#           a some overhead. The final file will be smaller.
#         Leave everything else as is. Make sure the Image Format is 
#           "read/write disk image".
#     Save as NAME.dmg
#     If it's not already open, open the new dmg file. This creates a 
#       Disk Volume mount, and an icon on the Desktop
#     Using the terminal, create a directory called .background inside
#       the Volume. Your new Volume can be found under /Volumes/NAME/ -
#       the directory can be any name, the dot just makes sure it's not
#       visible in the Finder
#     Put your background image in that directory
#     Double-click on the Disk Volume icon on the Desktop, to open it 
#       as most users will
#     Arrange the folder as you like it - remove the menu bar, change
#       it to icon mode, make aliases to your install folders and drag
#       them into the window, and using Cmd-J open the Display Preferences
#      to choose the background image. Make sure you select the one that's
#      inside the Volume's background folder.
#     Eject (Unmount) the Volume
#     Mount the volume again (by double-clicking on the dmg file)
#     Now make a new directory somewhere, let's call it staging
#     Put all the same files you put in the Volume into staging, 
#       including the .background directory with the image.
#     In the terminal, copy the .DS_Store file from the Volume to staging
#     You can now eject/unmount the Volume
#     In the terminal, go to the directory containing staging and run the
#       following two commands (credit goes to jwz)
#         hdiutil makehybrid -hfs -hfs-volume-name NAME -hfs-openfolder staging staging -o tmp.dmg
#         hdiutil convert -format UDZO tmp.dmg -o NAME.dmg
#         Remember, NAME is your App/Volume/Dist name, all the rest is verbatim
#     You can now remove tmp.dmg
# 
# Voilà! You now have a properly sized, compressed, read-only dmg file
#   that will open as you want it with a background image and links to folders.
# 
# For comments, suggestions, and bug-reports, please write guyθmedia·mit·edu
