class RequestCounter{ 
 void getThroughput() {
  Accumulator oldv = getValidAccumulator();
  double elapsed = foo(oldv); 
  if (elapsed > 0.0F)
  {
    return oldv.count(elapsed);
  }
  else
  {
    return -1.0F;
  }
}
}
