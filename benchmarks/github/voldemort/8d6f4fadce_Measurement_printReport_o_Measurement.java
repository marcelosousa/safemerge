class Measurement{ 
 void printReport() {
  Results result = generateResults();
  NumberFormat nf = NumberFormat.getInstance();
  nf.setMaximumFractionDigits(4);
  out.println(("[" + getName() + "]\tOperations: " + operations));
  out.println(("[" + getName() + "]\tAverage(ms): " + nf.format(((double) totalLatency / (double) operations))));
  out.println(("[" + getName() + "]\tMin(ms): " + nf.format(minLatency)));
  out.println(("[" + getName() + "]\tMax(ms): " + nf.format(maxLatency)));
  out.println(("[" + getName() + "]\tMedian(ms): " + nf.format(result.medianLatency)));
  out.println(("[" + getName() + "]\t95th(ms): " + nf.format(result.q95Latency)));
  out.println(("[" + getName() + "]\t99th(ms): " + nf.format(result.q99Latency)));
  if (!this.summaryOnly)
  {
    for (Integer I : returnCodes.keySet()) {
                                             int[] val = returnCodes.get(I);
                                             out.println(("[" + getName() + "]\tReturn: " + I + "\t" + val[0]));
                                           }
    for (int i = 0 ; i < _buckets ; i++)
    {
      out.println(("[" + getName() + "]: " + i + "\t" + histogram[i]));
    }
    out.println(("[" + getName() + "]: >" + _buckets + "\t" + histogramOverflow));
  }
}
}