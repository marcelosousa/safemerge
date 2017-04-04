class RequestCounter{ 
 private int x;
 private int field;

/*
 public int getfield(int x)
 {
   int z = x;
     z += x;
   for (int y = 0; y <= x; y++)
   {
     z += x;
    // if (z > 10)
    //   break;

     if (z > 100)
       return y;

     z--;
   }
   
   return (z + x); 
 }
*/

 public int getfield(int x)
 {
   switch (x) {
     default: z = 1; break;
     case 1: this.field = 1;
     case 3: y = 2;
   }
/*   
   int y = 0;
   int z = 0;
   if (x > 0)
   {
     if (y > 4)
       this.field = y;
   } else
   { 
     z = 0;
   }
   while (x > 0)
   {
     x--;
     y = z + 1;
   }
*/
   return x;
 }
/*
 public void setField()
 {
   this.field = this.x;
 }


 void getThroughput()
 {
  // int x = 0;
  // int y = 1;
  // int z = x + y;
  // this.field = 0;
  // this.setField();
  int oldv = this.getField(); 
  oldv = 0;
  return oldv;
 }
*/
}
