class ThresholdFailureDetectorTest{ 
 void createFailureDetector() {
  FailureDetectorConfig failureDetectorConfig = new FailureDetectorConfig().setImplementationClassName(ThresholdFailureDetector.class.getName()).setBannagePeriod(BANNAGE_MILLIS).setNodes(cluster.getNodes()).setStoreVerifier(create(cluster.getNodes())).setTime(time);
  return create(failureDetectorConfig, true);
}
}