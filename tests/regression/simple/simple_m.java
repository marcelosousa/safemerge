class RequestCounter{ 
 private int field;

 public void setField()
 {
   this.field = 1;
   return;
 }

 public int getField()
 {
   return this.field;
 }

 void getThroughput() {
  // int x = 0;
  // int y = 1;
  // int z = x + y;
  // this.field = 0;
  // this.setField();
  int oldv = this.getField(); 
  oldv = 1;
  return oldv;
}
}
