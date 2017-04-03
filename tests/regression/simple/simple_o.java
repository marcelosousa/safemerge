class RequestCounter{ 
 private int field;

 void getThroughput() {
 // int x = 0;
 // int y = 1;
 // int z = x + y;
  int oldv = getValidAccumulator();
  this.field = 0;
  return oldv;
}
}
