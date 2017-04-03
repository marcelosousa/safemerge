class RequestCounter{ 
 private int field;

 public void setField()
 {
   this.field = 1;
 }

 public int getField(int x)
 {
   if (x > 0)
   {
     return this.field;
   } else
   { 
     return 0;
   }
 }

 void getThroughput() {
  // int x = 0;
  // int y = 1;
  // int z = x + y;
  // this.field = 0;
  // this.setField();
  int oldv = this.getField(); 
  oldv = 0;
  return oldv;
}
}
