#!/bin/bash

echo "generating the graphs";
wiz parse -m=program -f=prog.txt > prog.dot;
wiz parse -m=edit -f=a.txt > a.dot;
wiz parse -m=edit -f=b.txt > b.dot;
wiz product -p=prog.txt -a=a.txt -b=b.txt -m=merge.txt > product.dot;
dot -Tpdf prog.dot > graphs/prog.pdf;
dot -Tpdf a.dot > graphs/a.pdf;
dot -Tpdf b.dot > graphs/b.pdf;
dot -Tpdf product.dot > graphs/product.pdf;
rm *.dot;
echo "generating the encodings";
#wiz merge -p=prog.txt -a=a.txt -b=b.txt -m=merge.txt -o=vcs/merge_enc.smt2;
wiz finermerge -p=prog.txt -a=a.txt -b=b.txt -m=merge.txt -o=vcs/finermerge_enc.smt2;
echo "calling solver";
#time z3 vcs/merge_enc.smt2 fixedpoint.engine=duality;
time z3 vcs/finermerge_enc.smt2 fixedpoint.engine=duality;
