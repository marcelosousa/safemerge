{
  ExecutorService executors = Executors.newFixedThreadPool(parallelTransfers, new ThreadFactory()
                                                                              {
                                                                                public Thread newThread (Runnable r)
                                                                                {
                                                                                  Thread thread = new Thread(r);
                                                                                  thread.setName("restore-data-thread");
                                                                                  return thread;
                                                                                }
                                                                              });
  try
  {
    List<StoreDefinition> storeDefList = getRemoteStoreDefList(nodeId).getValue();
    Cluster cluster = getRemoteCluster(nodeId).getValue();
    List<String> writableStores = RebalanceUtils.getWritableStores(storeDefList);
    for (StoreDefinition def : storeDefList) {
                                               if (writableStores.contains(def.getName()))
                                               {
                                                 restoreStoreFromReplication(nodeId, cluster, def, executors);
                                               }
                                             }
  }
  finally {
            executors.shutdown();
            try
            {
              executors.awaitTermination(adminClientConfig.getRestoreDataTimeout(), TimeUnit.SECONDS);
            }
            catch (InterruptedException e)
            {
              logger.error("Interrupted while waiting restoreDataFromReplications to finish ..");
            }
            logger.info("Finished restoring data.");
          }
}