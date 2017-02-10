class AbstractStoreClientFactory{ 
 void close() {
  this.threadPool.shutdown();
  try
  {
    if (!this.threadPool.awaitTermination(10, TimeUnit.SECONDS))
      this.threadPool.shutdownNow();
  }
  catch (InterruptedException e)
  {
    this.threadPool.shutdownNow();
  }
  if (failureDetector != null)
  {
    failureDetector.destroy();
    if (isJmxEnabled)
    {
      JmxUtils.unregisterMbean(JmxUtils.createObjectName(JmxUtils.getPackageName(failureDetector.getClass()), (JmxUtils.getClassName(failureDetector.getClass()) + JmxUtils.getJmxId(jmxId))));
      JmxUtils.unregisterMbean(JmxUtils.createObjectName(JmxUtils.getPackageName(threadPool.getClass()), (JmxUtils.getClassName(threadPool.getClass()) + JmxUtils.getJmxId(jmxId))));
      JmxUtils.unregisterMbean(JmxUtils.createObjectName("voldemort.store.stats.aggregate", ("aggregate-perf" + JmxUtils.getJmxId(jmxId))));
    }
  }
  stopClientAsyncSchedulers();
}
}