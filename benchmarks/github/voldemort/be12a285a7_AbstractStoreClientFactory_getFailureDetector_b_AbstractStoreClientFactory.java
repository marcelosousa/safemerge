class AbstractStoreClientFactory{ 
 void getFailureDetector() {
  FailureDetector result = failureDetector;
  if (result == null)
  {
    String clusterXml = bootstrapMetadataWithRetries(MetadataStore.CLUSTER_KEY, bootstrapUrls);
    Cluster cluster = clusterMapper.readCluster(new StringReader(clusterXml), false);
    synchronized (this)
    {
      result = failureDetector;
      if (result == null)
      {
        failureDetector = result = initFailureDetector(config, cluster.getNodes());
        if (isJmxEnabled)
        {
          JmxUtils.registerMbean(failureDetector, JmxUtils.createObjectName(JmxUtils.getPackageName(failureDetector.getClass()), (JmxUtils.getClassName(failureDetector.getClass()) + JmxUtils.getJmxId(jmxId))));
        }
      }
    }
  }
  return result;
}
}