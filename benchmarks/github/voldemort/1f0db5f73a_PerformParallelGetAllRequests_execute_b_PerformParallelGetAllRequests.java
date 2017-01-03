{
  int attempts = pipelineData.getNodeToKeysMap().size();
  final Map<Integer, Response<Iterable<ByteArray>, Object>> responses = new ConcurrentHashMap<Integer, Response<Iterable<ByteArray>, Object>>();
  final CountDownLatch latch = new CountDownLatch(attempts);
  if (logger.isTraceEnabled())
    logger.trace(("Attempting " + attempts + " " + pipeline.getOperation().getSimpleName() + " operations in parallel"));
  for (Map.Entry<Node, List<ByteArray>> entry : pipelineData.getNodeToKeysMap().entrySet()) {
                                                                                              final Node node = entry.getKey();
                                                                                              final Collection<ByteArray> keys = entry.getValue();
                                                                                              NonblockingStoreCallback callback = new NonblockingStoreCallback()
                                                                                                                                  {
                                                                                                                                    public void requestComplete (Object result, long requestTime)
                                                                                                                                    {
                                                                                                                                      if (logger.isTraceEnabled())
                                                                                                                                        logger.trace((pipeline.getOperation().getSimpleName() + " response received (" + requestTime + " ms.) from node " + node.getId()));
                                                                                                                                      Response<Iterable<ByteArray>, Object> response = new Response<Iterable<ByteArray>, Object>(node, keys, result, requestTime);
                                                                                                                                      responses.put(node.getId(), response);
                                                                                                                                      latch.countDown();
                                                                                                                                      if ((pipeline.isFinished() && response.getValue()) instanceof Exception)
                                                                                                                                        handleResponseError(response, pipeline, failureDetector);
                                                                                                                                    }
                                                                                                                                  };
                                                                                              if (logger.isTraceEnabled())
                                                                                                logger.trace(("Submitting " + pipeline.getOperation().getSimpleName() + " request on node " + node.getId()));
                                                                                              NonblockingStore store = nonblockingStores.get(node.getId());
                                                                                              store.submitGetAllRequest(keys, callback, timeoutMs);
                                                                                            }
  try
  {
    latch.await((timeoutMs * 3), TimeUnit.MILLISECONDS);
  }
  catch (InterruptedException e)
  {
    if (logger.isEnabledFor(Level.WARN))
      logger.warn(e, e);
  }
  for (Response<Iterable<ByteArray>, Object> response : responses.values()) {
                                                                              if (response.getValue() instanceof Exception)
                                                                              {
                                                                                if (handleResponseError(response, pipeline, failureDetector))
                                                                                  return;
                                                                              }
                                                                              else
                                                                              {
                                                                                Map<ByteArray, List<Versioned<byte[]>>> values = (Map<ByteArray, List<Versioned<byte[]>>>) response.getValue();
                                                                                for (ByteArray key : response.getKey()) {
                                                                                                                          MutableInt successCount = pipelineData.getSuccessCount(key);
                                                                                                                          successCount.increment();
                                                                                                                          List<Versioned<byte[]>> retrieved = values.get(key);
                                                                                                                          if (retrieved != null)
                                                                                                                          {
                                                                                                                            List<Versioned<byte[]>> existing = pipelineData.getResult().get(key);
                                                                                                                            if (existing == null)
                                                                                                                              pipelineData.getResult().put(key, Lists.newArrayList(retrieved));
                                                                                                                            else
                                                                                                                              existing.addAll(retrieved);
                                                                                                                          }
                                                                                                                          HashSet<Integer> zoneResponses = null;
                                                                                                                          if (pipelineData.getKeyToZoneResponse().containsKey(key))
                                                                                                                          {
                                                                                                                            zoneResponses = pipelineData.getKeyToZoneResponse().get(key);
                                                                                                                          }
                                                                                                                          else
                                                                                                                          {
                                                                                                                            zoneResponses = new HashSet<Integer>();
                                                                                                                          }
                                                                                                                          zoneResponses.add(response.getNode().getZoneId());
                                                                                                                        }
                                                                                pipelineData.getResponses().add(new Response<Iterable<ByteArray>, Map<ByteArray, List<Versioned<byte[]>>>>(response.getNode(), response.getKey(), values, response.getRequestTime()));
                                                                                failureDetector.recordSuccess(response.getNode(), response.getRequestTime());
                                                                              }
                                                                            }
  pipeline.addEvent(completeEvent);
}