
# source this to update the nyquist website

scp rbd@linux.gp.cs.cmu.edu:music/web/music.software.html music.software.html
# edit music.software.html and build install.bat
rm -f cmuinstall2.sh
../../ny
s cmuinstall2.sh
