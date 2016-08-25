# version=3.10
# echo "pandoc"
# pandoc -V geometry:"top=1in, bottom=1in, left=1in, right=1in" -N --template=mytemplate.tex --variable mainfont=Georgia --variable sansfont=Arial --variable monofont="Lucida Console" --variable fontsize=11pt --variable version=$version ../../doc/nyquistman.md --latex-engine=xelatex --include-before-body include-before.tex --toc -o nyquistman.tex
echo "ny"
ny > /dev/null
echo "pdflatex"
pdflatex nyquistman.tex
makeindex nyquistman
#  > /dev/null
open nyquistman.pdf
