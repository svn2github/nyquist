#
# this is a script to post the plight drum machine to the web
#
# usage: on the Mac, source install-plight.sh
#
cd ../../demos
zip -r plight.zip plight
scp plight.zip rbd@linux.gp.cs.cmu.edu:music/web/nyquist/plight-1.zip
rm plight.zip
cd ../misc/cmu
