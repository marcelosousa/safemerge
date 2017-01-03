{
  final int nodeId = request.getNodeId();
  Cluster cluster = metadataStore.getCluster();
  final List<Integer> partitions = request.getPartitionsList();
  final VoldemortFilter filter = request.hasFilter() ? getFilterFromRequest(request.getFilter()) : new DefaultVoldemortFilter();
  final String storeName = request.getStore();
  int requestId = lastOperationId.getAndIncrement();
  VAdminProto.AsyncOperationStatusResponse.Builder response = VAdminProto.AsyncOperationStatusResponse.newBuilder().setRequestId(requestId).setComplete(false).setDescription("Fetch and update").setStatus("started");
  try
  {
    asyncRunner.startRequest(requestId, new AsyncOperation(requestId, "Fetch and Update")
                                        {
                                          @Override
                                          public void operate ()
                                          {
                                            AdminClient adminClient = createTempAdminClient();
                                            try
                                            {
                                              StorageEngine<ByteArray, byte[]> storageEngine = getStorageEngine(storeName);
                                              Iterator<Pair<ByteArray, Versioned<byte[]>>> entriesIterator = adminClient.fetchPartitionEntries(nodeId, storeName, partitions, filter);
                                              updateStatus("Initated fetchPartitionEntries");
                                              EventThrottler throttler = new EventThrottler(voldemortConfig.getStreamMaxWriteBytesPerSec());
                                              for (long i = 0 ; entriesIterator.hasNext() ; i++)
                                              {
                                                Pair<ByteArray, Versioned<byte[]>> entry = entriesIterator.next();
                                                storageEngine.put(entry.getFirst(), entry.getSecond());
                                                throttler.maybeThrottle(entrySize(entry));
                                                if (i % 1000 == 0)
                                                {
                                                  updateStatus((i + " entries processed"));
                                                }
                                              }
                                            }
                                            finally {
                                                      adminClient.close();
                                                    }
                                          }
                                          private AdminClient createTempAdminClient ()
                                          {
                                            ClientConfig config = new ClientConfig();
                                            config.setMaxConnectionsPerNode(1);
                                            config.setMaxThreads(1);
                                            config.setConnectionTimeout(voldemortConfig.getAdminConnectionTimeout(), TimeUnit.MILLISECONDS);
                                            config.setSocketTimeout(voldemortConfig.getAdminSocketTimeout(), TimeUnit.MILLISECONDS);
                                            config.setSocketBufferSize(voldemortConfig.getAdminSocketBufferSize());
                                            return new ProtoBuffAdminClientRequestFormat(metadataStore.getCluster(), config);
                                          }
                                        });
  }
  catch (VoldemortException e)
  {
    response.setError(ProtoUtils.encodeError(errorCodeMapper, e));
  }
  return response.build();
}