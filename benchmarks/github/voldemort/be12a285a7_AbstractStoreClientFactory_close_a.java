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
    failureDetector.destroy();
  stopClientAsyncSchedulers();
}
}