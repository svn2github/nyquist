#!/bin/sh
# create symbolic links in nyquist to 
#   NyquistIDE.app/Contents/Resources/Java/lib and demos
#
# Roger B. Dannenberg
# Feb, 2013
#
# to be included in NyquistIDE.app/Contents/Resources/Java/
# and run from Java
#

real_lib=${PWD}/lib
real_demos=${PWD}/demos
cd ../..
contents_path=$PWD
# sanity check: is current directory as expected?
if [ `basename $PWD` != "Contents" ]; then
    exit 1
fi
# look for nyquist directory
cd ../..
if [ -d "nyquist" ]; then
    echo "found nyquist directory for symbolic links"
    cd nyquist
else
    exit 2
fi
# replace nyquist path in target path with .. to get relative
#     symbolic link path (# means remove matching prefix)
ideal=../${real_lib#`dirname $PWD`}
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
ideal=../${real_demos#`dirname $PWD`}
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

echo "end of mac-os-x-link-script"
exit 0
