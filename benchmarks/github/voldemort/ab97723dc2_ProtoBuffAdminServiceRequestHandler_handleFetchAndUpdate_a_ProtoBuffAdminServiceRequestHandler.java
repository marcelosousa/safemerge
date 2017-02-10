class ProtoBuffAdminServiceRequestHandler{ 
 void handleFetchAndUpdate() {
  final int nodeId = request.getNodeId();
  Cluster cluster = metadataStore.getCluster();
  Node remoteNode = cluster.getNodeById(nodeId);
  String adminUrl = remoteNode.getSocketUrl().toString();
  final List<Integer> partitions = request.getPartitionsList();
  final VoldemortFilter filter = request.hasFilter() ? getFilterFromRequest(request.getFilter()) : new DefaultVoldemortFilter();
  final String storeName = request.getStore();
  String requestId = "fetchAndUpdate" + requestToId(storeName, nodeId, partitions);
  VAdminProto.InitiateFetchAndUpdateResponse.Builder response = VAdminProto.InitiateFetchAndUpdateResponse.newBuilder().setRequestId(requestId);
  try
  {
    asyncRunner.startRequest(requestId, new AsyncOperation()
                                        {
                                          public void run ()
                                          {
                                            setStatus("Started");
                                            StorageEngine<ByteArray, byte[]> storageEngine = getStorageEngine(storeName);
                                            AdminClient adminClient = null;
                                            Iterator<Pair<ByteArray, Versioned<byte[]>>> entriesIterator = adminClient.fetchPartitionEntries(nodeId, storeName, partitions, filter);
                                            setStatus("Initated fetchPartitionEntries");
                                            EventThrottler throttler = new EventThrottler(streamMaxBytesWritesPerSec);
                                            for (long i = 0 ; entriesIterator.hasNext() ; i++)
                                            {
                                              Pair<ByteArray, Versioned<byte[]>> entry = entriesIterator.next();
                                              storageEngine.put(entry.getFirst(), entry.getSecond());
                                              throttler.maybeThrottle(entrySize(entry));
                                              if (i % 1000 == 0)
                                              {
                                                setStatus((i + " entries processed"));
                                              }
                                            }
                                            setStatus("Finished processing");
                                            setComplete();
                                          }
                                        });
  }
  catch (VoldemortException e)
  {
    response.setError(ProtoUtils.encodeError(errorCodeMapper, e));
  }
  return response.build();
}
}