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
        JmxUtils.registerMbean(failureDetector, JmxUtils.createObjectName(JmxUtils.getPackageName(failureDetector.getClass()), (JmxUtils.getClassName(failureDetector.getClass()) + jmxId())));
      }
    }
  }
  return result;
}
}