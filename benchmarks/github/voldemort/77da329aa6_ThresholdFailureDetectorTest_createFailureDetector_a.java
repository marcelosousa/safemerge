class ThresholdFailureDetectorTest{ 
 void createFailureDetector() {
  FailureDetectorConfig failureDetectorConfig = new FailureDetectorConfig().setImplementationClassName(ThresholdFailureDetector.class.getName()).setThresholdInterval(1000).setAsyncRecoveryInterval(250).setNodes(cluster.getNodes()).setStoreVerifier(create(cluster.getNodes())).setTime(time);
  return create(failureDetectorConfig, true);
}
}