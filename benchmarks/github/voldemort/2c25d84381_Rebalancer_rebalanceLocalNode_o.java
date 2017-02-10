class Rebalancer{ 
 void rebalanceLocalNode() {
  if (!acquireRebalancingPermit())
  {
    RebalancePartitionsInfo info = metadataStore.getRebalancingStealInfo();
    throw new VoldemortException("Node " + metadataStore.getCluster().getNodeById(info.getStealerId()) + " is already rebalancing from " + info.getDonorId() + " rebalanceInfo:" + info);
  }
  int requestId = asyncRunner.getUniqueRequestId();
  asyncRunner.submitOperation(requestId, new AsyncOperation(requestId, stealInfo.toString())
                                         {
                                           @Override
                                           public void operate () throws Exception
                                           {
                                             AdminClient adminClient = RebalanceUtils.createTempAdminClient(voldemortConfig, metadataStore.getCluster());
                                             try
                                             {
                                               logger.info(("Rebalancer: rebalance " + stealInfo + " starting."));
                                               checkAndCreateRedirectingSocketStore(storeName, adminClient.getAdminClientCluster().getNodeById(stealInfo.getDonorId()));
                                               checkCurrentState(metadataStore, stealInfo);
                                               setRebalancingState(metadataStore, stealInfo);
                                               migratePartitionsAsyncId = adminClient.migratePartitions(stealInfo.getDonorId(), metadataStore.getNodeId(), storeName, stealInfo.getPartitionList(), null);
                                               adminClient.waitForCompletion(metadataStore.getNodeId(), migratePartitionsAsyncId, voldemortConfig.getAdminSocketTimeout(), TimeUnit.SECONDS);
                                               logger.info(("Rebalancer: rebalance " + stealInfo + " completed successfully."));
                                               metadataStore.cleanAllRebalancingState();
                                               if (voldemortConfig.isDeleteAfterRebalancingEnabled())
                                               {
                                                 logger.warn("Deleting data from donorNode after rebalancing !!");
                                                 adminClient.deletePartitions(stealInfo.getDonorId(), storeName, stealInfo.getPartitionList(), null);
                                                 logger.info(("Deleted partitions " + stealInfo.getPartitionList() + " from donorNode:" + stealInfo.getDonorId()));
                                               }
                                             }
                                             finally {
                                                       releaseRebalancingPermit();
                                                       adminClient.stop();
                                                       migratePartitionsAsyncId = (-1);
                                                     }
                                           }
                                           @Override
                                           @JmxGetter(name = "asyncTaskStatus")
                                           public AsyncOperationStatus getStatus ()
                                           {
                                             if (-1 != migratePartitionsAsyncId)
                                               try
                                               {
                                                 updateStatus(asyncRunner.getStatus(migratePartitionsAsyncId));
                                               }
                                               catch (Exception e)
                                               {
                                               }
                                             return super.getStatus();
                                           }
                                         });
  return requestId;
}
}