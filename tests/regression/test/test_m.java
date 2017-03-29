class RequestCounter{ 
 void getThroughput() {
  Accumulator oldv = getValidAccumulator();
  double elapsed = bar(oldv);
  if (elapsed > 0.0F)
  {
    return oldv.count(elapsed);
  }
  else
  {
    return 0.0F;
  }
}
}
