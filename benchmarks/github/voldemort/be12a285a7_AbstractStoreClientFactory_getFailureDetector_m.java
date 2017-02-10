class AbstractStoreClientFactory{ 
 void getFailureDetector() {
  if (this.cluster == null)
  {
    String clusterXml = bootstrapMetadataWithRetries(MetadataStore.CLUSTER_KEY, bootstrapUrls);
    this.cluster = clusterMapper.readCluster(new StringReader(clusterXml), false);
  }
  FailureDetector result = failureDetector;
  if (result == null)
  {
    logger.debug("Failure detector is null. Creating a new FD.");
    synchronized (this)
    {
      result = failureDetector;
      if (result == null)
      {
        failureDetector = result = initFailureDetector(config, this.cluster);
        if (isJmxEnabled)
        {
          JmxUtils.registerMbean(failureDetector, JmxUtils.createObjectName(JmxUtils.getPackageName(failureDetector.getClass()), (JmxUtils.getClassName(failureDetector.getClass()) + JmxUtils.getJmxId(jmxId))));
        }
      }
    }
  }
  else
  {
    logger.debug("Failure detector already exists. Updating the state and flushing cached verifier stores.");
    synchronized (this)
    {
      failureDetector.getConfig().setCluster(this.cluster);
      failureDetector.getConfig().getStoreVerifier().flushCachedStores();
    }
  }
  return result;
}
}