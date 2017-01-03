{
  if (!acquireRebalancingPermit())
  {
    RebalancePartitionsInfo info = metadataStore.getRebalancingStealInfo();
    throw new VoldemortException("Node " + metadataStore.getCluster().getNodeById(info.getStealerId()) + " is already rebalancing from " + info.getDonorId() + " rebalanceInfo:" + info);
  }
  int requestId = asyncRunner.getUniqueRequestId();
  asyncRunner.submitOperation(requestId, new AsyncOperation(requestId, stealInfo.toString())
                                         {
                                           private int migratePartitionsAsyncId = (-1);
                                           private String currentStore = null;
                                           @Override
                                           public void operate () throws Exception
                                           {
                                             AdminClient adminClient = RebalanceUtils.createTempAdminClient(voldemortConfig, metadataStore.getCluster());
                                             try
                                             {
                                               checkCurrentState(metadataStore, stealInfo);
                                               logger.info(("Rebalancer: rebalance " + stealInfo + " starting."));
                                               List<String> tempUnbalancedStoreList = new ArrayList<String>(stealInfo.getUnbalancedStoreList());
                                               for (String storeName : ImmutableList.copyOf(stealInfo.getUnbalancedStoreList())) {
                                                                                                                                   try
                                                                                                                                   {
                                                                                                                                     rebalanceStore(storeName, adminClient, stealInfo);
                                                                                                                                     tempUnbalancedStoreList.remove(storeName);
                                                                                                                                     stealInfo.setUnbalancedStoreList(tempUnbalancedStoreList);
                                                                                                                                   }
                                                                                                                                   catch (Exception e)
                                                                                                                                   {
                                                                                                                                     logger.warn(("rebalanceSubTask:" + stealInfo + " failed for store:" + storeName), e);
                                                                                                                                   }
                                                                                                                                 }
                                               if (stealInfo.getUnbalancedStoreList().isEmpty())
                                               {
                                                 logger.info(("Rebalancer: rebalance " + stealInfo + " completed successfully."));
                                                 metadataStore.cleanAllRebalancingState();
                                               }
                                               else
                                               {
                                                 throw new VoldemortException("Rebalancer: Failed to rebalance all stores, unbalanced stores:" + stealInfo.getUnbalancedStoreList() + " rebalanceInfo:" + stealInfo);
                                               }
                                             }
                                             finally {
                                                       releaseRebalancingPermit();
                                                       adminClient.stop();
                                                       migratePartitionsAsyncId = (-1);
                                                     }
                                           }
                                           private void rebalanceStore (String storeName, AdminClient adminClient, RebalancePartitionsInfo stealInfo) throws Exception
                                           {
                                             createRedirectingSocketStore(storeName, adminClient.getAdminClientCluster().getNodeById(stealInfo.getDonorId()));
                                             setRebalancingState(metadataStore, stealInfo);
                                             updateStatus(("starting partitions migration for store:" + storeName));
                                             currentStore = storeName;
                                             migratePartitionsAsyncId = adminClient.migratePartitions(stealInfo.getDonorId(), metadataStore.getNodeId(), storeName, stealInfo.getPartitionList(), null);
                                             adminClient.waitForCompletion(metadataStore.getNodeId(), migratePartitionsAsyncId, voldemortConfig.getAdminSocketTimeout(), TimeUnit.SECONDS);
                                             if (stealInfo.isDeleteDonorPartitions())
                                             {
                                               logger.warn("Deleting data from donorNode after rebalancing !!");
                                               adminClient.deletePartitions(stealInfo.getDonorId(), storeName, stealInfo.getPartitionList(), null);
                                               logger.info(("Deleted partitions " + stealInfo.getPartitionList() + " from donorNode:" + stealInfo.getDonorId()));
                                             }
                                             updateStatus(("partitions migration for store:" + storeName + " completed."));
                                             migratePartitionsAsyncId = (-1);
                                             currentStore = null;
                                           }
                                           @Override
                                           @JmxGetter(name = "asyncTaskStatus")
                                           public AsyncOperationStatus getStatus ()
                                           {
                                             if ((-1 != migratePartitionsAsyncId && null) != currentStore)
                                               try
                                               {
                                                 updateStatus(("partitions migration for store:" + currentStore + " status:" + asyncRunner.getStatus(migratePartitionsAsyncId)));
                                               }
                                               catch (Exception e)
                                               {
                                               }
                                             return super.getStatus();
                                           }
                                         });
  return requestId;
}