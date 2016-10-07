#motivating example

Correct merge
wiz finermerge -p=prog.txt -a=a.txt -b=b.txt -m=m.txt -o=a.smt2
c:\temp\semantic-merge-github\merge-z3\z3-master\z3-master\build\z3.exe a.smt2 fixedpoint.engine=duality

Incorrect merge
wiz finermerge -p=prog.txt -a=a.txt -b=b_err.txt -m=m_err.txt -o=a.smt2
c:\temp\semantic-merge-github\merge-z3\z3-master\z3-master\build\z3.exe a.smt2 fixedpoint.engine=duality
