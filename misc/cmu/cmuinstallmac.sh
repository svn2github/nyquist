# cmuinstallmac.sh -- to update website with Mac OS X version of Nyquist
# run this like this: source cmuinstallmac.sh

ls ~/nyquist/*/*~
echo "build jNyqIDE deployment project with Xcode and type return"
read
mkdir -p ~/tmp/Applications
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
# to get NyquistIDE.app in the right place in the zip file, move it to here
mv ~/nyquist/macosxproject/build/Deployment/NyquistIDE.app .
tar cvfz "nyqosx"$versionstring".tgz" NyquistIDE.app nyquist
# restore NyquistIDE.app to its original location
mv NyquistIDE.app ~/nyquist/macosxproject/build/Deployment/NyquistIDE.app
mv nyqosx*.tgz ~/nyquist
# Make source release
cd ~/nyquist
rm -rf nyquist
svn export -r BASE . nyquist
rm -rf nyquist/demos/plight
zip -r "nyqsrc"$versionstring".zip" nyquist
# THE FOLLOWING PUTS THE VERSION AT CMU, BUT NOW RELEASES GO TO SOURCEFORGE
#scp "nyqosx"$versionstring".tgz" rbd@linux.gp.cs.cmu.edu:music/web/nyquist/
# HERE IS THE LINE FOR SOURCEFORGE
#echo "when sftp connects..."
#echo "> put nyqosx"$versionstring".tgz"
#echo "> put nyqsrc"$versionstring".zip"
#echo "> exit"
#sftp rbd@frs.sourceforge.net
#echo "after sftp'ing mac, windows, and source release files, go to"
#echo "Admin : File Releases : Add Release to make a new release"

echo go to sourceforge.net/projects/nyquist, Files, open nyquist
echo Add Folder for current version, click the folder to open it
echo Add File and browse to ~/nyquist/nyqsrcNNN.zip
echo Add File and browse to ~/nyquist/nyqosxNNN.zip
