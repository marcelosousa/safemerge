#!/bin/bash
echo "generating the graphs";
wiz parse -m=program -f=$1_orig.txt > prog.dot;
wiz parse -m=edit -f=$1_a.txt > a.dot;
wiz parse -m=edit -f=$1_b.txt > b.dot;
wiz parse -m=edit -f=$1_merge.txt > merge.dot;
wiz product -p=$1_orig.txt -a=$1_a.txt -b=$1_b.txt -m=$1_merge.txt > product.dot;
mkdir graphs
dot -Tpdf prog.dot > graphs/prog.pdf;
dot -Tpdf a.dot > graphs/a.pdf;
dot -Tpdf b.dot > graphs/b.pdf;
dot -Tpdf merge.dot > graphs/merge.pdf;
dot -Tpdf product.dot > graphs/product.pdf;
#rm *.dot;
mkdir vcs;
echo "generating the encodings";
#wiz merge -p=$1_orig.txt -a=$1_a.txt -b=$1_b.txt -m=$1_merge.txt -o=vcs/merge_enc.smt2;
wiz finermerge -p=$1_orig.txt -a=$1_a.txt -b=$1_b.txt -m=$1_merge.txt -o=vcs/finermerge_enc.smt2;
echo "calling solver";
#time z3 vcs/merge_enc.smt2 fixedpoint.engine=duality;
time z3 vcs/finermerge_enc.smt2 fixedpoint.engine=duality;
