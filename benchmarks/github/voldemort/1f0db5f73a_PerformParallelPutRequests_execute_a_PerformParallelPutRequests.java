class PerformParallelPutRequests{ 
 void execute() {
  Node master = pipelineData.getMaster();
  Versioned<byte[]> versionedCopy = pipelineData.getVersionedCopy();
  if (logger.isDebugEnabled())
    logger.debug(("Serial put requests determined master node as " + master.getId() + ", submitting remaining requests in parallel"));
  List<Node> nodes = pipelineData.getNodes();
  int firstParallelNodeIndex = nodes.indexOf(master) + 1;
  int attempts = nodes.size() - firstParallelNodeIndex;
  int blocks = Math.min((preferred - 1), attempts);
  final Map<Integer, Response<ByteArray, Object>> responses = new ConcurrentHashMap<Integer, Response<ByteArray, Object>>();
  final CountDownLatch attemptsLatch = new CountDownLatch(attempts);
  final CountDownLatch blocksLatch = new CountDownLatch(blocks);
  if (logger.isTraceEnabled())
    logger.trace(("Attempting " + attempts + " " + pipeline.getOperation().getSimpleName() + " operations in parallel"));
  for (int i = firstParallelNodeIndex ; i < firstParallelNodeIndex + attempts ; i++)
  {
    final Node node = nodes.get(i);
    pipelineData.incrementNodeIndex();
    NonblockingStoreCallback callback = new NonblockingStoreCallback()
                                        {
                                          public void requestComplete (Object result, long requestTime)
                                          {
                                            if (logger.isTraceEnabled())
                                              logger.trace((pipeline.getOperation().getSimpleName() + " response received (" + requestTime + " ms.) from node " + node.getId()));
                                            responses.put(node.getId(), new Response<ByteArray, Object>(node, key, result, requestTime));
                                            attemptsLatch.countDown();
                                            blocksLatch.countDown();
                                          }
                                        };
    if (logger.isTraceEnabled())
      logger.trace(("Submitting " + pipeline.getOperation().getSimpleName() + " request on node " + node.getId()));
    NonblockingStore store = nonblockingStores.get(node.getId());
    store.submitPutRequest(key, versionedCopy, transforms, callback);
  }
  try
  {
    long ellapsedNs = System.nanoTime() - pipelineData.getStartTimeNs();
    long remainingNs = timeoutMs * Time.NS_PER_MS - ellapsedNs;
    if (remainingNs > 0)
      blocksLatch.await(remainingNs, TimeUnit.NANOSECONDS);
  }
  catch (InterruptedException e)
  {
    if (logger.isEnabledFor(Level.WARN))
      logger.warn(e, e);
  }
  for (Entry<Integer, Response<ByteArray, Object>> responseEntry : responses.entrySet()) {
                                                                                           Response<ByteArray, Object> response = responseEntry.getValue();
                                                                                           if (response.getValue() instanceof Exception)
                                                                                           {
                                                                                             if (handleResponseError(response, pipeline, failureDetector))
                                                                                               return;
                                                                                           }
                                                                                           else
                                                                                           {
                                                                                             pipelineData.incrementSuccesses();
                                                                                             failureDetector.recordSuccess(response.getNode(), response.getRequestTime());
                                                                                             pipelineData.getZoneResponses().add(response.getNode().getZoneId());
                                                                                             responses.remove(responseEntry.getKey());
                                                                                           }
                                                                                         }
  boolean quorumSatisfied = true;
  if (pipelineData.getSuccesses() < required)
  {
    long ellapsedNs = System.nanoTime() - pipelineData.getStartTimeNs();
    long remainingNs = timeoutMs * Time.NS_PER_MS - ellapsedNs;
    if (remainingNs > 0)
    {
      try
      {
        attemptsLatch.await(remainingNs, TimeUnit.NANOSECONDS);
      }
      catch (InterruptedException e)
      {
        if (logger.isEnabledFor(Level.WARN))
          logger.warn(e, e);
      }
      for (Entry<Integer, Response<ByteArray, Object>> responseEntry : responses.entrySet()) {
                                                                                               Response<ByteArray, Object> response = responseEntry.getValue();
                                                                                               if (response.getValue() instanceof Exception)
                                                                                               {
                                                                                                 if (handleResponseError(response, pipeline, failureDetector))
                                                                                                   return;
                                                                                               }
                                                                                               else
                                                                                               {
                                                                                                 pipelineData.incrementSuccesses();
                                                                                                 failureDetector.recordSuccess(response.getNode(), response.getRequestTime());
                                                                                                 pipelineData.getZoneResponses().add(response.getNode().getZoneId());
                                                                                                 responses.remove(responseEntry.getKey());
                                                                                               }
                                                                                             }
    }
    if (pipelineData.getSuccesses() < required)
    {
      pipelineData.setFatalError(new InsufficientOperationalNodesException((required + " " + pipeline.getOperation().getSimpleName() + "s required, but only " + pipelineData.getSuccesses() + " succeeded"), pipelineData.getFailures()));
      pipeline.addEvent(Event.ERROR);
      quorumSatisfied = false;
    }
  }
  if (quorumSatisfied)
  {
    if (pipelineData.getZonesRequired() != null)
    {
      int zonesSatisfied = pipelineData.getZoneResponses().size();
      if (zonesSatisfied >= pipelineData.getZonesRequired() + 1)
      {
        pipeline.addEvent(completeEvent);
      }
      else
      {
        long timeMs = (System.nanoTime() - pipelineData.getStartTimeNs()) / Time.NS_PER_MS;
        if (timeoutMs - timeMs > 0)
        {
          try
          {
            attemptsLatch.await((timeoutMs - timeMs), TimeUnit.MILLISECONDS);
          }
          catch (InterruptedException e)
          {
            if (logger.isEnabledFor(Level.WARN))
              logger.warn(e, e);
          }
          for (Entry<Integer, Response<ByteArray, Object>> responseEntry : responses.entrySet()) {
                                                                                                   Response<ByteArray, Object> response = responseEntry.getValue();
                                                                                                   if (response.getValue() instanceof Exception)
                                                                                                   {
                                                                                                     if (handleResponseError(response, pipeline, failureDetector))
                                                                                                       return;
                                                                                                   }
                                                                                                   else
                                                                                                   {
                                                                                                     pipelineData.incrementSuccesses();
                                                                                                     failureDetector.recordSuccess(response.getNode(), response.getRequestTime());
                                                                                                     pipelineData.getZoneResponses().add(response.getNode().getZoneId());
                                                                                                     responses.remove(responseEntry.getKey());
                                                                                                   }
                                                                                                 }
        }
        if (pipelineData.getZoneResponses().size() >= pipelineData.getZonesRequired() + 1)
        {
          pipeline.addEvent(completeEvent);
        }
        else
        {
          pipelineData.setFatalError(new InsufficientZoneResponsesException((pipelineData.getZonesRequired() + 1 + " " + pipeline.getOperation().getSimpleName() + "s required zone, but only " + zonesSatisfied + " succeeded")));
          pipeline.addEvent(Event.ERROR);
        }
      }
    }
    else
    {
      pipeline.addEvent(completeEvent);
    }
  }
}
}