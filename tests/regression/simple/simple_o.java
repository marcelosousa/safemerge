class RequestCounter{ 
 private int field;

/*
 public void setField()
 {
   this.field = 1;
 }
*/
 public int getField(int x)
 {
   int y = 0;
   int z = 0;
   /*
   switch (x) {
     case 1: this.field = 1;
             break;
     case 2: y = 1;
             break;
     case 3: y = 2;
     default: z = 1;
              break;
   }
   
   while (x > 0)
   {
     

   }
   */
   if (x > 0)
   {
     if (y > 4)
       this.field = y;
   } else
   { 
     z = 0;
   }

   return x;
 }

/*
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
