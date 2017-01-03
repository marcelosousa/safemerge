{
  if (donorNodePlans.size() == 0)
  {
    logger.info("Nothing to move around");
    return;
  }
  int t = 0;
  for (List<RebalancePartitionsInfo> rebalancePartitionsInfos : donorNodePlans.values()) t += rebalancePartitionsInfos.size();
  final int total = t * storeNames.size();
  final AtomicInteger completed = new AtomicInteger(0);
  final long startedAt = System.currentTimeMillis();
  logger.info(("Changing state of donor nodes " + donorNodePlans.keySet()));
  final CountDownLatch latch = new CountDownLatch(stealerNodeIds.size());
  try
  {
    changeToGrandfather();
    for (final int stealerNodeId : stealerNodeIds) {
                                                     executor.submit(new Runnable()
                                                                     {
                                                                       public void run ()
                                                                       {
                                                                         try
                                                                         {
                                                                           RebalanceNodePlan nodePlan = stealerNodePlans.get(stealerNodeId);
                                                                           if (nodePlan == null)
                                                                           {
                                                                             logger.info(("No plan for stealer node id " + stealerNodeId));
                                                                             return;
                                                                           }
                                                                           List<RebalancePartitionsInfo> partitionInfo = nodePlan.getRebalanceTaskList();
                                                                           logger.info(("Working on stealer node id " + stealerNodeId));
                                                                           for (String storeName : storeNames) {
                                                                                                                 logger.info(("- Working on store " + storeName));
                                                                                                                 HashMap<Integer, Integer> nodeIdToRequestId = Maps.newHashMap();
                                                                                                                 Set<Pair<Integer, Integer>> pending = Sets.newHashSet();
                                                                                                                 for (RebalancePartitionsInfo r : partitionInfo) {
                                                                                                                                                                   logger.info(("-- Started migration from donorId " + r.getDonorId() + " to " + stealerNodeId));
                                                                                                                                                                   logger.info(r);
                                                                                                                                                                   if (!simulation)
                                                                                                                                                                   {
                                                                                                                                                                     int attemptId = adminClient.migratePartitions(r.getDonorId(), stealerNodeId, storeName, r.getPartitionList(), null);
                                                                                                                                                                     nodeIdToRequestId.put(r.getDonorId(), attemptId);
                                                                                                                                                                     pending.add(Pair.create(r.getDonorId(), attemptId));
                                                                                                                                                                   }
                                                                                                                                                                 }
                                                                                                                 while ((!pending.isEmpty()))
                                                                                                                 {
                                                                                                                   long delay = 1000;
                                                                                                                   Set<Pair<Integer, Integer>> currentPending = ImmutableSet.copyOf(pending);
                                                                                                                   for (Pair<Integer, Integer> pair : currentPending) {
                                                                                                                                                                        AsyncOperationStatus status = adminClient.getAsyncRequestStatus(stealerNodeId, pair.getSecond());
                                                                                                                                                                        logger.info(("Status of move from " + pair.getFirst() + " to " + stealerNodeId + ": " + status.getStatus()));
                                                                                                                                                                        if (status.hasException())
                                                                                                                                                                        {
                                                                                                                                                                          throw new VoldemortException(status.getException());
                                                                                                                                                                        }
                                                                                                                                                                        if (status.isComplete())
                                                                                                                                                                        {
                                                                                                                                                                          logger.info(("-- Completed migration from donorId " + pair.getFirst() + " to " + stealerNodeId));
                                                                                                                                                                          logger.info(("-- " + completed.incrementAndGet() + " out of " + total + " tasks completed"));
                                                                                                                                                                          pending.remove(pair);
                                                                                                                                                                          long velocity = (System.currentTimeMillis() - startedAt) / completed.get();
                                                                                                                                                                          long eta = (total - completed.get()) * velocity / Time.MS_PER_SECOND;
                                                                                                                                                                          logger.info(("-- Estimated " + eta + " seconds until completion"));
                                                                                                                                                                        }
                                                                                                                                                                      }
                                                                                                                   try
                                                                                                                   {
                                                                                                                     Thread.sleep(delay);
                                                                                                                     if (delay < 30000)
                                                                                                                       delay *= 2;
                                                                                                                   }
                                                                                                                   catch (InterruptedException e)
                                                                                                                   {
                                                                                                                     throw new VoldemortException(e);
                                                                                                                   }
                                                                                                                 }
                                                                                                               }
                                                                         }
                                                                         catch (Exception e)
                                                                         {
                                                                           logger.error(e, e);
                                                                           while ((latch.getCount() > 0))
                                                                             latch.countDown();
                                                                           executor.shutdownNow();
                                                                           throw new VoldemortException(e);
                                                                         }
                                                                         finally {
                                                                                   latch.countDown();
                                                                                 }
                                                                       }
                                                                     });
                                                     logger.info("===============================================");
                                                   }
  }
  catch (Exception e)
  {
    logger.error(e, e);
    executor.shutdownNow();
  }
  finally {
            if (!(executor.isShutdown() || executor.isTerminated()))
            {
              try
              {
                latch.await();
              }
              catch (InterruptedException e)
              {
                logger.error(e, e);
                throw new VoldemortException(e);
              }
              finally {
                        if (donorStates != null && transitionToNormal)
                        {
                          changeToNormal();
                        }
                        executor.shutdown();
                      }
            }
          }
}