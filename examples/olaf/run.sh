#!/bin/bash

echo "generating the graphs";
wiz parse -m=program -f=prog_orig.txt > prog.dot;
wiz parse -m=edit -f=prog_a.txt > a.dot;
wiz parse -m=edit -f=prog_b.txt > b.dot;
wiz parse -m=edit -f=prog_merge.txt > merge.dot;
wiz product -p=prog_orig.txt -a=prog_a.txt -b=prog_b.txt -m=prog_merge.txt > product.dot;
dot -Tpdf prog.dot > graphs/prog.pdf;
dot -Tpdf a.dot > graphs/a.pdf;
dot -Tpdf b.dot > graphs/b.pdf;
dot -Tpdf merge.dot > graphs/merge.pdf;
dot -Tpdf product.dot > graphs/product.pdf;
rm *.dot;
echo "generating the encodings";
wiz merge -p=prog_orig.txt -a=prog_a.txt -b=prog_b.txt -m=prog_merge.txt -o=vcs/merge_enc.smt2;
wiz finermerge -p=prog_orig.txt -a=prog_a.txt -b=prog_b.txt -m=prog_merge.txt -o=vcs/finermerge_enc.smt2;
echo "calling solver";
time z3 vcs/merge_enc.smt2 fixedpoint.engine=duality;
time z3 vcs/finermerge_enc.smt2 fixedpoint.engine=duality;
