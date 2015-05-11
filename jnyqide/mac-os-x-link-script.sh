#!/bin/sh
# create symbolic links in nyquist to 
#   NyquistIDE.app/Contents/Resources/Java/lib and demos and doc
# nyquist directory containing links will go into the parent
#   directory of NyquistIDE.app
#
# Roger B. Dannenberg
# Feb, 2013
#
# to be included in NyquistIDE.app/Contents/Java/
# and run from Java
#
echo "**** Starting mac-os-x-link-script.sh ****"
echo "PWD is ${PWD}"

real_lib=${PWD}/lib
real_demos=${PWD}/demos
real_doc=${PWD}/doc
cd ..
contents_path=$PWD
# sanity check: is current directory as expected?
if [ `basename $PWD` != "Contents" ]; then
    exit 1
fi
# look for nyquist directory
cd ../..
basedir=`pwd`
if [ -d "nyquist" ]; then
    echo "found nyquist directory for symbolic links"
    cd nyquist
else
    mkdir nyquist
    if [ -d "nyquist" ]; then
        echo "made nyquist directory for symbolic links"
        cd nyquist
    else
        exit 2
    fi
fi


# replace nyquist path in target path with .. to get relative
#     symbolic link path (# means remove matching prefix)
# real_lib is something like /Applications/NyquistIDE.app/Contents/Java/lib
# basedir is something like /Applications/
# ${real_lib#$basedir} is like /NyquistIDE.app/Contents/Java/lib
# put .. in front to get relative path to lib:
ideal=..${real_lib#$basedir}
# see if symbolic links already exist
if [ -s lib ]; then
    link=`readlink lib`
    if [ $ideal == $link ]; then
        echo "lib link exists"
    else
        echo "attempt to remove bad lib link to " $link
        rm lib
        ln -s $ideal
    fi
else
    ln -s $ideal    
fi

# do the same with demos directory
ideal=..${real_demos#$basedir}
if [ -s demos ]; then
    link=`readlink demos`
    if [ $ideal == $link ]; then
        echo "demos link exists"
    else
        echo "attempt to remove bad demos link to " $link
        rm demos
        ln -s $ideal    
    fi
else
    ln -s $ideal    
fi

# do the same with doc directory
ideal=..${real_doc#$basedir}
if [ -s doc ]; then
    link=`readlink doc`
    if [ $ideal == $link ]; then
        echo "doc link exists"
    else
        echo "attempt to remove bad demos link to " $link
        rm doc
        ln -s $ideal    
    fi
else
    ln -s $ideal    
fi

echo "You can delete this" > This_nyquist_directory_was_created_by_NyquistIDE.app-to-provide-handy-links-into-the-app-bundle

echo "end of mac-os-x-link-script"
exit 0
