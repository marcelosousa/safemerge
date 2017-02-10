class AdminServiceRequestHandler{ 
 void handleFetchAndUpdate() {
  final int nodeId = request.getNodeId();
  final List<Integer> partitions = request.getPartitionsList();
  final VoldemortFilter filter = request.hasFilter() ? getFilterFromRequest(request.getFilter(), voldemortConfig, networkClassLoader) : new DefaultVoldemortFilter();
  final String storeName = request.getStore();
  int requestId = asyncService.getUniqueRequestId();
  VAdminProto.AsyncOperationStatusResponse.Builder response = VAdminProto.AsyncOperationStatusResponse.newBuilder().setRequestId(requestId).setComplete(false).setDescription("Fetch and update").setStatus("started");
  try
  {
    asyncService.submitOperation(requestId, new AsyncOperation(requestId, "Fetch and Update")
                                            {
                                              private final AtomicBoolean running = new AtomicBoolean(true);
                                              @Override
                                              public void stop ()
                                              {
                                                running.set(false);
                                              }
                                              @Override
                                              public void operate ()
                                              {
                                                AdminClient adminClient = RebalanceUtils.createTempAdminClient(voldemortConfig, metadataStore.getCluster(), 4, 2);
                                                try
                                                {
                                                  StorageEngine<ByteArray, byte[], byte[]> storageEngine = getStorageEngine(storeRepository, storeName);
                                                  Iterator<Pair<ByteArray, Versioned<byte[]>>> entriesIterator = adminClient.fetchEntries(nodeId, storeName, partitions, filter, false);
                                                  updateStatus("Initated fetchPartitionEntries");
                                                  EventThrottler throttler = new EventThrottler(voldemortConfig.getStreamMaxWriteBytesPerSec());
                                                  for (long i = 0 ; (running.get() && entriesIterator.hasNext()) ; i++)
                                                  {
                                                    Pair<ByteArray, Versioned<byte[]>> entry = entriesIterator.next();
                                                    ByteArray key = entry.getFirst();
                                                    Versioned<byte[]> value = entry.getSecond();
                                                    try
                                                    {
                                                      storageEngine.put(key, value, null);
                                                    }
                                                    catch (ObsoleteVersionException e)
                                                    {
                                                      logger.debug("migratePartition threw ObsoleteVersionException, Ignoring.");
                                                    }
                                                    throttler.maybeThrottle((key.length() + valueSize(value)));
                                                    if (i % 1000 == 0)
                                                    {
                                                      updateStatus((i + " entries processed"));
                                                    }
                                                  }
                                                }
                                                finally {
                                                          adminClient.stop();
                                                        }
                                              }
                                            });
  }
  catch (VoldemortException e)
  {
    response.setError(ProtoUtils.encodeError(errorCodeMapper, e));
    logger.error(("handleFetchAndUpdate failed for request(" + request.toString() + ")"), e);
  }
  return response.build();
}
}