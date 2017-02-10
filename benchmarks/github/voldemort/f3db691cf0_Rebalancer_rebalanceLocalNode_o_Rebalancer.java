class Rebalancer{ 
 void rebalanceLocalNode() {
  int requestId = asyncRunner.getUniqueRequestId();
  asyncRunner.submitOperation(requestId, new AsyncOperation(requestId, stealInfo.toString())
                                         {
                                           private int fetchAndUpdateAsyncId = (-1);
                                           @Override
                                           public void operate () throws Exception
                                           {
                                             synchronized (metadataStore)
                                             {
                                               checkCurrentState(metadataStore, stealInfo);
                                               setRebalancingState(metadataStore, stealInfo);
                                             }
                                             AdminClient adminClient = RebalanceUtils.createTempAdminClient(config, metadataStore.getCluster());
                                             try
                                             {
                                               fetchAndUpdateAsyncId = adminClient.fetchAndUpdateStreams(stealInfo.getDonorId(), metadataStore.getNodeId(), storeName, stealInfo.getPartitionList(), null);
                                               logger.info(("rebalance internal async Id:" + fetchAndUpdateAsyncId));
                                               adminClient.waitForCompletion(metadataStore.getNodeId(), fetchAndUpdateAsyncId, (24 * 60 * 60), TimeUnit.SECONDS);
                                             }
                                             finally {
                                                       adminClient.stop();
                                                     }
                                             metadataStore.cleanAllRebalancingState();
                                           }
                                           @Override
                                           @JmxGetter(name = "asyncTaskStatus")
                                           public AsyncOperationStatus getStatus ()
                                           {
                                             if (-1 != fetchAndUpdateAsyncId && !asyncRunner.isComplete(fetchAndUpdateAsyncId))
                                               updateStatus(asyncRunner.getStatus(fetchAndUpdateAsyncId));
                                             return super.getStatus();
                                           }
                                         });
  logger.info(("rebalance node request_id:" + requestId));
  return requestId;
}
}