# A simple makefile for a Hello World Java program

# Where are the source files?
NYQ = $(SOURCE_ROOT)/..
SRC = $(NYQ)/jnyqide

# Where to build output?
BLD = $(BUILT_PRODUCTS_DIR)
# Where are the class files?
CLASS = $(BLD)

# Define a makefile variable for the java compiler
JCC = javac

# Use the -g flag for debugging information
ifeq ($(CONFIGURATION),Debug)
	DBG_FLAG = -g
else
	DBG_FLAG = 
endif

# Define a makefile variable for compilation flags
JFLAGS = $(DBG_FLAG) -d $(CLASS)

default: NyquistIDE.app

$(CLASS)/%.class : $(SRC)/%.java
# make build directory for class files in case it does not exist
	mkdir -p $(CLASS)
	$(JCC) $(JFLAGS) $<

# To start over from scratch, type 'make clean'. 
# Removes all .class files, so that the next make rebuilds them
#
clean:
	$(RM) $(CLASS)/*.class

$(BLD)/NyquistIDE.jar: $(wildcard $(CLASS)/jnyqide/*.class) $(SRCROOT)/Makefile
	$(JCC) $(JFLAGS) $(SRC)/*.java
	cd $(CLASS); jar -cfm NyquistIDE.jar $(SOURCE_ROOT)/manifest.txt \
            jnyqide/*.class


NyquistIDE.app: $(BLD)/NyquistIDE.jar Info.plist
	rm -rf $(BLD)/NyquistIDE.app
	mkdir -p $(BLD)/NyquistIDE.app/Contents/Resources/Java
	cp $(BLD)/NyquistIDE.jar $(BLD)/NyquistIDE.app/Contents/Resources/Java/
	cp -Rp $(SRC)/NyquistWords.txt \
	    $(BLD)/NyquistIDE.app/Contents/Resources/Java/
	cp -Rp $(SRC)/*.gif \
	    $(BLD)/NyquistIDE.app/Contents/Resources/Java/
	svn export $(NYQ)/demos \
	    $(BLD)/NyquistIDE.app/Contents/Resources/Java/demos
	svn export $(NYQ)/lib \
	    $(BLD)/NyquistIDE.app/Contents/Resources/Java/lib
	cp -Rp $(BUILT_PRODUCTS_DIR)/ny \
            $(BLD)/NyquistIDE.app/Contents/Resources/Java/
	cp -Rp osc-test-client $(BLD)/NyquistIDE.app/Contents/Resources/Java/
	svn export $(NYQ)/runtime \
	    $(BLD)/NyquistIDE.app/Contents/Resources/Java/runtime
	cp -Rp ser-to-osc $(BLD)/NyquistIDE.app/Contents/Resources/Java/
	cp -Rp $(NYQ)/sys/unix/osx/system.lsp \
	    $(BLD)/NyquistIDE.app/Contents/Resources/Java/
	cp Info.plist $(BLD)/NyquistIDE.app/Contents/
	cp PkgInfo $(BLD)/NyquistIDE.app/Contents/
	mkdir -p $(BLD)/NyquistIDE.app/Contents/MacOS
	cp JavaApplicationStub $(BLD)/NyquistIDE.app/Contents/MacOS
	cp Nyquist.icns $(BLD)/NyquistIDE.app/Contents/Resources
