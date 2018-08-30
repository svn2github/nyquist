#!/bin/sh

echo "BEGIN makejar.sh"

SRC_ROOT=..
JNYQIDE_ROOT=$SRC_ROOT/jnyqide
# Note: $CONFIGURATION comes from Xcode

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

echo "ENDING makejar.sh"
