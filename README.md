progmerge
=========

High-level procedure
--------------------
  (x) For each merge in the git log whose changeset specifies concurrent modifications to the same method  
      (x) Done with a lightweight ast difference tool  
  (x) Obtain the tuple (FileName, ClassName, MethodName, (Hash-Base, Hash-A, Hash-B, Hash-M)  
  (x) Load in memory the 4 versions of ClassName  
  (x) Compute the 4-way diff between the versions of MethodName. The output is (ProgramWithHoles, Edit-Base, Edit-A, Edit-B, Edit-M):  
      (x) Scope of the holes is at the statement level. 
  (x) Generate a ClassInfo object composed of the field and method information  
  (x) Generate the initial objects of the 4 versions and constraints that specify that they are in the same state:  
      (x) Requires pairing of fields and is sensitive to refactoring/renaming of fields  
  (x) Partition the program with holes in segments of blocks of statements (which do not contain any hole) by statements
      that contain a hole (conditionals or loops or straight line modifications):  
      (x) Perform a dependence analysis in each complete segment and replace the segment with calls to uninterpreted functions 
          that capture the changes  
          (x) If these blocks make method calls, it is possible that we need to call uninterpreted functions specific to versions if the methods have been modified   
  (x) Revisit the product construction for the edit scripts  
  (x) Verification algorithm:  
      (x) Support for containers (modelling)  
      (x) Support for loops (houdini style)  
      (-) Object instances
      (-) Matching of methods   

 
Dependence analysis  
------------------
  (x) Basic protype  
  (x) Test the CFG construction with the constructs that we should handle  
      (x) Make a Java class with all of them  
  (-) Flow-sensitivity for the dependence analysis  
  (-) Fixpoint computation for (mutually) recursive calls 
  (x) Test for correctness and proper support of fields and containers  
  (-) Support for print.out (consider those as outputs)


(x) Make a library out of Liff so that it can be used from the main verification tool  
(x) AST re-write pass to get rid of for loops  
(x) Pass that annotates each Stmt with [PID]  
(-) Pretty printing for the new AST  

Benchmarks
----------

About 50 benchmarks (8-10 instances x 5 projects)   
 Description of the benchmarks   
 - Agilefant:
 - Voldemort
 - Libgdx
 - Apache Storm?
 - Android 
 - Netty
 - PMD
 - DrJava
  
