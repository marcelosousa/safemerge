class StorageService{ 
 void openStore() {
  logger.info(("Opening store '" + storeDef.getName() + "' (" + storeDef.getType() + ")."));
  StorageEngine<ByteArray, byte[], byte[]> engine = getStorageEngine(storeDef.getName(), storeDef.getType());
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