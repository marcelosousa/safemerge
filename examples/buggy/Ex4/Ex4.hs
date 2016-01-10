module Ex4 where

import Types


{---------------------------- 

parent:
-------
n0:  skip [n11,n21,n31,n41,n51,def]
n11: assume (grade = 'A') [n12]
n12: r = 4 [exit]
n21: assume (grade = 'B') [n22]
n22: r = 3 [exit]
n31: assume (grade = 'C') [n32]
n32: r = 2 [exit]
n41: assume (grade = 'D') [n22]
n42: r = 1 [exit]
n51: assume (grade = 'F') [n22]
n52: r = 0 [exit]
def: skip [exit]

edit a:
-------
n0  |-> 
     n0_0 : skip [n11',n21',n31',n41',n51',def]
n11 |-> 
     n11' : assume (g = 'A') [n12]
n21 |-> 
     n21' : assume (g = 'B') [n22]
n21 |-> 
     n31' : assume (g = 'C') [n32]
n41 |-> 
     n41' : assume (g = 'D') [n42]
n51 |-> 
     n51' : assume (g = 'F') [n52]

edit b:
-------
n0  |-> 
     n0_0 : g = -1  [n0_1]
     n0_1 : skip [n11,n21,n31,n41,n51,def']
def |-> 
     def' : r = g [exit]

merge edit:
-----------

n0  |->
     n0_0 : g = -1  [n0_1]
     n0_1 : skip [n11',n21',n31',n41',n51',def']
n11 |-> 
     n11' : assume (g = 'A') [n12]
n21 |-> 
     n21' : assume (g = 'B') [n22]
n21 |-> 
     n31' : assume (g = 'C') [n32]
n41 |-> 
     n41' : assume (g = 'D') [n42]
n51 |-> 
     n51' : assume (g = 'F') [n52]

def |-> 
     def' : r = g [exit]

----------------------------------}
