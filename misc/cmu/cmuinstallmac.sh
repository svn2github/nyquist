# cmuinstallmac.sh -- to update website with Mac OS X version of Nyquist

ls ~/nyquist/*/*~
echo "build jNyqIDE deployment project with Xcode and type return"
read
cd ~/tmp/Applications
## can't remove and replace plight -- if you do, it won't work. 
## I don't know why. Also, the following fails without sudo...
# rm -rf NyquistIDE.app/Contents/Resources/Java/demos/plight
rm -rf nyquist
mkdir nyquist
mkdir nyquist/doc
cp ~/nyquist/doc/* nyquist/doc
echo "type the version string, e.g. \"232\" : "
read versionstring
tar cvfz "nyqosx"$versionstring".tgz" NyquistIDE.app nyquist
mv nyqosx*.tgz ~/nyquist
# Make source release
cd ~/nyquist
rm -rf nyquist
mkdir nyquist
cvs export -DNOW nyquist
rm -rf nyquist/demos/plight
zip -r "nyqsrc"$(versionstring)".zip" nyquist
# THE FOLLOWING PUTS THE VERSION AT CMU, BUT NOW RELEASES GO TO SOURCEFORGE
#scp "nyqosx"$versionstring".tgz" rbd@linux.gp.cs.cmu.edu:music/web/nyquist/
# HERE IS THE LINE FOR SOURCEFORGE
echo "when sftp connects..."
echo "> cd uploads"
echo "> put nyqosx"$versionstring".tgz"
echo "> put nyqsrc"$versionstring".zip"
echo "> exit"
sftp rbd@frs.sourceforge.net
echo "after sftp'ing mac, windows, and source release files, go to"
echo "Admin : File Releases : Add Release to make a new release"

