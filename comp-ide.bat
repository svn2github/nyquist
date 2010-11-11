rem comp-ide.bat -- compile the jnyqide on Windows: just type comp-ide
rem
rem On windows, we compile everything but SpecialMacHandler.java
rem
cd jnyqide
ren SpecialMacHandler.java SpecialMacHandler.hidden
javac *.java
ren SpecialMacHandler.hidden SpecialMacHandler.java
cd ..
rem jnyqide\jNyqIDE.jar
jar -cfm jnyqide\jNyqIDE.jar jnyqide/manifest.txt jnyqide/*.class
