{
  final int nodeId = request.getNodeId();
  Cluster cluster = metadataStore.getCluster();
  Node remoteNode = cluster.getNodeById(nodeId);
  String adminUrl = remoteNode.getSocketUrl().toString();
  final List<Integer> partitions = request.getPartitionsList();
  final AdminClientFactory adminClientFactory = new AdminClientFactory(new ClientConfig().setBootstrapUrls(adminUrl));
  final VoldemortFilter filter = request.hasFilter() ? getFilterFromRequest(request.getFilter()) : new DefaultVoldemortFilter();
  final String storeName = request.getStore();
  int requestId = lastOperationId.getAndIncrement();
  VAdminProto.AsyncOperationStatusResponse.Builder response = VAdminProto.AsyncOperationStatusResponse.newBuilder().setRequestId(requestId).setComplete(false).setDescription("Fetch and update").setStatus("started");
  try
  {
    asyncRunner.startRequest(requestId, new AsyncOperation(requestId, "Fetch and Update")
                                        {
                                          public void apply ()
                                          {
                                            StorageEngine<ByteArray, byte[]> storageEngine = getStorageEngine(storeName);
                                            AdminClient adminClient = adminClientFactory.getAdminClient();
                                            Iterator<Pair<ByteArray, Versioned<byte[]>>> entriesIterator = adminClient.fetchPartitionEntries(nodeId, storeName, partitions, filter);
                                            updateStatus("Initated fetchPartitionEntries");
                                            EventThrottler throttler = new EventThrottler(streamMaxBytesWritesPerSec);
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
                                        });
  }
  catch (VoldemortException e)
  {
    response.setError(ProtoUtils.encodeError(errorCodeMapper, e));
  }
  return response.build();
}