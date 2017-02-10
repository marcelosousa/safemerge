class StorageService{ 
 void openStore() {
  logger.info(("Opening store '" + storeDef.getName() + "' (" + storeDef.getType() + ")."));
  StorageConfiguration config = storageConfigs.get(storeDef.getType());
  if (config == null)
    throw new ConfigurationException("Attempt to open store " + storeDef.getName() + " but " + storeDef.getType() + " storage engine of type " + storeDef.getType() + " has not been enabled.");
  if (storeDef.getType().compareTo(ReadOnlyStorageConfiguration.TYPE_NAME) == 0)
  {
    final RoutingStrategy routingStrategy = new RoutingStrategyFactory().updateRoutingStrategy(storeDef, metadata.getCluster());
    ((ReadOnlyStorageConfiguration) config).setRoutingStrategy(routingStrategy);
  }
  final StorageEngine<ByteArray, byte[]> engine = config.getStore(storeDef.getName());
  if (storeDef.getType().compareTo(ReadOnlyStorageConfiguration.TYPE_NAME) == 0)
  {
    metadata.addMetadataStoreListener(storeDef.getName(), new MetadataStoreListener()
                                                          {
                                                            public void updateRoutingStrategy (RoutingStrategy updatedRoutingStrategy)
                                                            {
                                                              ((ReadOnlyStorageEngine) engine).setRoutingStrategy(updatedRoutingStrategy);
                                                            }
                                                          });
  }
  try
  {
    registerEngine(engine);
    if (voldemortConfig.isServerRoutingEnabled())
      registerNodeStores(storeDef, metadata.getCluster(), voldemortConfig.getNodeId());
    if (storeDef.hasRetentionPeriod())
      scheduleCleanupJob(storeDef, engine);
  }
  catch (Exception e)
  {
    unregisterEngine(storeDef, engine);
    throw new VoldemortException(e);
  }
}
}