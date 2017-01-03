{
  int matchedEntries = 0;
  RoutingStrategy routing = new ConsistentRoutingStrategy(cluster.getNodes(), 1);
  Store<ByteArray, byte[], byte[]> store = getSocketStore(testStoreName, node.getHost(), node.getSocketPort());
  for (Entry<String, String> entry : testEntries.entrySet()) {
                                                               ByteArray keyBytes = new ByteArray(ByteUtils.getBytes(entry.getKey(), "UTF-8"));
                                                               List<Integer> partitions = routing.getPartitionList(keyBytes.get());
                                                               if (null != unavailablePartitions && unavailablePartitions.containsAll(partitions))
                                                               {
                                                                 try
                                                                 {
                                                                   List<Versioned<byte[]>> value = store.get(keyBytes, null);
                                                                   assertEquals("unavailable partitons should return zero size list.", 0, value.size());
                                                                 }
                                                                 catch (InvalidMetadataException e)
                                                                 {
                                                                 }
                                                               }
                                                               else
                                                                 if (null != availablePartitions && availablePartitions.containsAll(partitions))
                                                                 {
                                                                   List<Versioned<byte[]>> values = store.get(keyBytes, null);
                                                                   assertEquals("Expecting exactly one version", 1, values.size());
                                                                   Versioned<byte[]> value = values.get(0);
                                                                   assertEquals("Value version should match", new VectorClock(), value.getVersion());
                                                                   assertEquals("Value bytes should match", entry.getValue(), ByteUtils.getString(value.getValue(), "UTF-8"));
                                                                   matchedEntries++;
                                                                 }
                                                                 else
                                                                 {
                                                                 }
                                                             }
  if ((null != availablePartitions && availablePartitions.size()) > 0)
    assertNotSame("CheckGetEntries should match some entries.", 0, matchedEntries);
}