class ReadOnlyStorageEngineTestInstance{ 
 void create() {
  Map<String, String> data = createTestData(testSize);
  JsonReader reader = makeTestDataReader(data, baseDir);
  List<Node> nodes = new ArrayList<Node>();
  for (int i = 0 ; i < numNodes ; i++)
  {
    nodes.add(new Node(i, "localhost", (8080 + i), (6666 + i), (7000 + i), Arrays.asList((4 * i), (4 * i + 1), (4 * i + 2), (4 * i + 3))));
  }
  Cluster cluster = new Cluster("test", nodes);
  StoreDefinition storeDef = new StoreDefinitionBuilder().setName("test").setType(ReadOnlyStorageConfiguration.TYPE_NAME).setKeySerializer(keySerDef).setValueSerializer(valueSerDef).setRoutingPolicy(RoutingTier.CLIENT).setRoutingStrategyType(RoutingStrategyType.CONSISTENT_STRATEGY).setReplicationFactor(repFactor).setPreferredReads(1).setRequiredReads(1).setPreferredWrites(1).setRequiredWrites(1).build();
  RoutingStrategy router = new RoutingStrategyFactory().updateRoutingStrategy(storeDef, cluster);
  File outputDir = TestUtils.createTempDir(baseDir);
  JsonStoreBuilder storeBuilder = new JsonStoreBuilder(reader, cluster, storeDef, router, outputDir, null, testSize / 5, 1, 2, 10000, false);
  storeBuilder.build();
  File nodeDir = TestUtils.createTempDir(baseDir);
  @SuppressWarnings("unchecked")
  Serializer<String> keySerializer = (Serializer<String>) new DefaultSerializerFactory().getSerializer(keySerDef);
  @SuppressWarnings("unchecked")
  Serializer<String> valueSerializer = (Serializer<String>) new DefaultSerializerFactory().getSerializer(valueSerDef);
  Serializer<String> transSerializer = (Serializer<String>) new StringSerializer();
  Map<Integer, Store<String, String, String>> nodeStores = Maps.newHashMap();
  for (int i = 0 ; i < numNodes ; i++)
  {
    File currNode = new File(nodeDir, Integer.toString(i));
    currNode.mkdirs();
    currNode.deleteOnExit();
    Utils.move(new File(outputDir, ("node-" + Integer.toString(i))), new File(currNode, "version-0"));
    CompressionStrategyFactory comppressionStrategyFactory = new CompressionStrategyFactory();
    CompressionStrategy keyCompressionStrat = comppressionStrategyFactory.get(keySerDef.getCompression());
    CompressionStrategy valueCompressionStrat = comppressionStrategyFactory.get(valueSerDef.getCompression());
    Store<ByteArray, byte[], byte[]> innerStore = new CompressingStore(new ReadOnlyStorageEngine("test", strategy, currNode, 1), keyCompressionStrat, valueCompressionStrat);
    nodeStores.put(i, SerializingStore.wrap(innerStore, keySerializer, valueSerializer, transSerializer));
  }
  return new ReadOnlyStorageEngineTestInstance(data, baseDir, nodeStores, router, keySerializer);
}
}