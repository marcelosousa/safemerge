class RoutedStoreTest{ 
 void testPutTimeout() {
  int timeout = 50;
  StoreDefinition definition = new StoreDefinitionBuilder().setName("test").setType("foo").setKeySerializer(new SerializerDefinition("test")).setValueSerializer(new SerializerDefinition("test")).setRoutingPolicy(RoutingTier.CLIENT).setRoutingStrategyType(RoutingStrategyType.CONSISTENT_STRATEGY).setReplicationFactor(3).setPreferredReads(3).setRequiredReads(3).setPreferredWrites(3).setRequiredWrites(3).build();
  Map<Integer, Store<ByteArray, byte[]>> stores = new HashMap<Integer, Store<ByteArray, byte[]>>();
  List<Node> nodes = new ArrayList<Node>();
  int totalDelay = 0;
  for (int i = 0 ; i < 3 ; i++)
  {
    int delay = (4 + i) * timeout;
    totalDelay += delay;
    Store<ByteArray, byte[]> store = new SleepyStore<ByteArray, byte[]>(delay, new InMemoryStorageEngine<ByteArray, byte[]>("test"));
    stores.put(i, store);
    List<Integer> partitions = Arrays.asList(i);
    nodes.add(new Node(i, "none", 0, 0, 0, partitions));
  }
  setFailureDetector(stores);
  routedStoreThreadPool = Executors.newFixedThreadPool(3);
  RoutedStoreFactory routedStoreFactory = new RoutedStoreFactory(isPipelineRoutedStoreEnabled, routedStoreThreadPool, timeout);
  RoutedStore routedStore = routedStoreFactory.create(new Cluster("test", nodes), definition, stores, true, failureDetector);
  long start = System.currentTimeMillis();
  try
  {
    routedStore.put(new ByteArray("test".getBytes()), new Versioned<byte[]>(new byte[] {
                                                                                         1,
                                                                                       }));
    fail("Should have thrown");
  }
  catch (InsufficientOperationalNodesException e)
  {
    long elapsed = System.currentTimeMillis() - start;
    assertTrue((elapsed + " < " + totalDelay), (elapsed < totalDelay));
  }
}
}