class MigratePartitions{ 
 void migrate() {
  final HashMultimap<String, RebalancePartitionsInfo> completedTasks = HashMultimap.create();
  final AtomicInteger completed = new AtomicInteger(0);
  if (new File(checkpointFolder).exists())
  {
    if (!Utils.isReadableDir(checkpointFolder))
    {
      logger.error(("The checkpoint folder " + checkpointFolder + " cannot be read"));
      return;
    }
    logger.info("Check point file exists, parsing it...");
    for (String storeName : storeNames) {
                                          if (!new File(checkpointFolder, storeName).exists())
                                          {
                                            logger.info(("No check point file for store " + storeName));
                                            completedTasks.putAll(storeName, new ArrayList<RebalancePartitionsInfo>());
                                          }
                                          else
                                          {
                                            List<String> completedTasksLines = null;
                                            try
                                            {
                                              completedTasksLines = FileUtils.readLines(new File(checkpointFolder, storeName));
                                            }
                                            catch (IOException e)
                                            {
                                              logger.error(("Could not read the check point file for store name " + storeName), e);
                                              return;
                                            }
                                            for (String completedTaskLine : completedTasksLines) {
                                                                                                   completedTasks.put(storeName, RebalancePartitionsInfo.create(completedTaskLine));
                                                                                                 }
                                            logger.info(("Completed tasks for " + storeName + " = " + completedTasks.get(storeName).size()));
                                            completed.addAndGet(completedTasks.get(storeName).size());
                                          }
                                        }
  }
  else
  {
    logger.info(("Check point folder does not exist. Starting a new one at " + checkpointFolder));
    Utils.mkdirs(new File(checkpointFolder));
  }
  logger.info(" ============================================== ");
  if (donorNodePlans.size() == 0)
  {
    logger.info("Nothing to move around");
    return;
  }
  int t = 0;
  for (List<RebalancePartitionsInfo> rebalancePartitionsInfos : donorNodePlans.values()) t += rebalancePartitionsInfos.size();
  final int total = t * storeNames.size();
  final long startedAt = System.currentTimeMillis();
  logger.info(("Changing state of donor nodes " + donorNodePlans.keySet()));
  final CountDownLatch latch = new CountDownLatch(stealerNodeIds.size());
  try
  {
    changeToGrandfather();
    final AtomicInteger numStealersCompleted = new AtomicInteger(0);
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
                                                                                                                 Set<Pair<Integer, Integer>> pending = Sets.newHashSet();
                                                                                                                 HashMap<Pair<Integer, Integer>, RebalancePartitionsInfo> pendingTasks = Maps.newHashMap();
                                                                                                                 for (RebalancePartitionsInfo r : partitionInfo) {
                                                                                                                                                                   if (completedTasks.get(storeName).contains(r))
                                                                                                                                                                   {
                                                                                                                                                                     logger.info(("-- Not doing task from donorId " + r.getDonorId() + " to " + r.getStealerId() + " with store " + storeName + " since it is already done"));
                                                                                                                                                                     continue;
                                                                                                                                                                   }
                                                                                                                                                                   logger.info(("-- Started migration from donorId " + r.getDonorId() + " to " + stealerNodeId + " for store " + storeName));
                                                                                                                                                                   if (!simulation)
                                                                                                                                                                   {
                                                                                                                                                                     int attemptId = adminClient.migratePartitions(r.getDonorId(), stealerNodeId, storeName, r.getPartitionList(), null);
                                                                                                                                                                     pending.add(Pair.create(r.getDonorId(), attemptId));
                                                                                                                                                                     pendingTasks.put(Pair.create(r.getDonorId(), attemptId), r);
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
                                                                                                                                                                          logger.info(("-- Completed migration from donorId " + pair.getFirst() + " to " + stealerNodeId + " for store " + storeName));
                                                                                                                                                                          logger.info(("-- " + completed.incrementAndGet() + " out of " + total + " tasks completed"));
                                                                                                                                                                          pending.remove(pair);
                                                                                                                                                                          BufferedWriter out = null;
                                                                                                                                                                          try
                                                                                                                                                                          {
                                                                                                                                                                            out = new BufferedWriter(new FileWriter(new File(checkpointFolder, storeName), true));
                                                                                                                                                                            out.write((pendingTasks.get(pair).toJsonString() + "\n"));
                                                                                                                                                                          }
                                                                                                                                                                          catch (Exception e)
                                                                                                                                                                          {
                                                                                                                                                                            logger.error(("Failure while writing check point for store " + storeName + ". Emitting it here "));
                                                                                                                                                                            logger.error(("Checkpoint failure (" + storeName + "):" + pendingTasks.get(pair).toJsonString()));
                                                                                                                                                                          }
                                                                                                                                                                          finally {
                                                                                                                                                                                    if (out != null)
                                                                                                                                                                                    {
                                                                                                                                                                                      out.flush();
                                                                                                                                                                                      out.close();
                                                                                                                                                                                    }
                                                                                                                                                                                  }
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
                                                                           logger.error(("Exception for stealer node " + stealerNodeId), e);
                                                                           while ((latch.getCount() > 0))
                                                                             latch.countDown();
                                                                           executor.shutdownNow();
                                                                           throw new VoldemortException(e);
                                                                         }
                                                                         finally {
                                                                                   latch.countDown();
                                                                                   logger.info(("Number of stealers completed - " + numStealersCompleted.incrementAndGet()));
                                                                                 }
                                                                       }
                                                                     });
                                                   }
    latch.await();
  }
  catch (Exception e)
  {
    logger.error("Exception in full process", e);
    executor.shutdownNow();
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