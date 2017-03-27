progmerge
=========

High-level procedure
--------------------
  (x) For each merge in the git log whose changeset specifies concurrent modifications to the same method
      (x) Done with a lightweight ast difference tool
  (x) Obtain the tuple (FileName, ClassName, MethodName, (Hash-Base, Hash-A, Hash-B, Hash-M)
  (x) Load in memory the 4 versions of ClassName
  (x) Compute the 4-way diff between the versions of MethodName. The output is (ProgramWithHoles, Edit-Base, Edit-A, Edit-B, Edit-M):
      (-) Scope of the holes is at the statement level. If we have a hole inside a loop body, consider the entire loop a hole? The answer should be no.
  (-) Generate a ClassInfo object composed of the field and method information (the exact information is to be seen)
  (-) Generate the initial objects of the 4 versions and constraints that specify that they are in the same state:
      (-) Requires pairing of fields and is sensitive to refactoring/renaming of fields
  (-) Partition the program with holes in segments of blocks of statements (which do not contain any hole) by statements
      that contain a hole (conditionals or loops or straight line modifications):
      (-) Perform a dependence analysis in each complete segment and replace the segment with calls to uninterpreted functions 
          that capture the changes
          (-) If these blocks make method calls, it is possible that we need to call uninterpreted functions specific to versions if the methods have been modified 
  (-) Revisit the product construction for the edit scripts
  (-) Verification algorithm:
      (-) Support for containers (modelling)
      (-) Support for loops (houdini style)
      (-) Matching of methods 

 
Dependence analysis
------------------
  (x) Basic protype
  (-) Test the CFG construction with the constructs that we should handle
      (-) Make a Java class with all of them
  (-) Flow-sensitivity for the dependence analysis
  (-) Fixpoint computation for (mutually) recursive calls
  (-) Test for correctness and proper support of fields and containers
