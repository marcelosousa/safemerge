class StorageService{ 
 void registerEngine() {
  Cluster cluster = this.metadata.getCluster();
  storeRepository.addStorageEngine(engine);
  Store<ByteArray, byte[], byte[]> store = engine;
  if (voldemortConfig.isVerboseLoggingEnabled())
    store = new LoggingStore<ByteArray, byte[], byte[]>(store, cluster.getName(), SystemTime.INSTANCE);
  if (!"slop".equals(store.getName()))
  {
    if (voldemortConfig.isRedirectRoutingEnabled())
      store = new RedirectingStore(store, metadata, storeRepository, failureDetector, storeFactory);
    if (voldemortConfig.isMetadataCheckingEnabled())
      store = new InvalidMetadataCheckingStore(metadata.getNodeId(), store, metadata);
  }
  if (voldemortConfig.isStatTrackingEnabled())
  {
    StatTrackingStore<ByteArray, byte[], byte[]> statStore = new StatTrackingStore<ByteArray, byte[], byte[]>(store, this.storeStats);
    store = statStore;
    if (voldemortConfig.isJmxEnabled())
    {
      MBeanServer mbeanServer = ManagementFactory.getPlatformMBeanServer();
      ObjectName name = JmxUtils.createObjectName(JmxUtils.getPackageName(store.getClass()), store.getName());
      synchronized (mbeanServer)
      {
        if (mbeanServer.isRegistered(name))
          JmxUtils.unregisterMbean(mbeanServer, name);
        JmxUtils.registerMbean(mbeanServer, JmxUtils.createModelMBean(new StoreStatsJmx(statStore.getStats())), name);
      }
    }
  }
  storeRepository.addLocalStore(store);
}
}