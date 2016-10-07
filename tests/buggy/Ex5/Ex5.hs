module Ex4 where

import Types


{---------------------------- 

parent:
-------
n0:  skip [n11,n21,n31,n41,n51,def]
n11: assume (grade = 'A') [n12]
n12: r = 4 [ret]
n21: assume (grade = 'B') [n22]
n22: r = 3 [ret]
n31: assume (grade = 'C') [n32]
n32: r = 2 [ret]
n41: assume (grade = 'D') [n22]
n42: r = 1 [ret]
n51: assume (grade = 'F') [n22]
n52: r = 0 [ret]
def: skip [ret]
ret: ret = r [exit]

edit a:
-------
n12 |-> 
    n12' : ret = 4 [exit]
n22 |-> 
    n22' : ret = 3 [exit]
n32 |-> 
    n32' : ret = 2 [exit]
n42 |-> 
    n42' : ret = 1 [exit]
n52 |-> 
    n52' : ret = 0 [exit]

edit b:
-------
def |-> 
    def' : r = -1 [ret]     

merge edit:
-----------
n12 |-> 
    n12' : ret = 4 [exit]
n22 |-> 
    n22' : ret = 3 [exit]
n32 |-> 
    n32' : ret = 2 [exit]
n42 |-> 
    n42' : ret = 1 [exit]
n52 |-> 
    n52' : ret = 0 [exit]
def |-> 
    def' : r = -1 [ret]     
ret |->
    ret' : skip [exit]

-------------------------------------}
