class Example { 
/* public int foo(int i)
 {
   if (i % 2)
   {
     return 1;
   } else {
     return 0;
   }
 }*/

 public int example(int n)
 {
   // int n = 5;
   int time = 0;
   int brk  = 0;
 
   for (int i=0; ((i < n) && (brk == 0)); i++)
   {
     if (foo(n) == 1)
     {
       brk = 1;
     } else {
       time = 3;
     }
   }

   time = 1;

   return time;
 }
}
