{
  Map<String, String> data = createTestData(testSize);
  JsonReader reader = makeTestDataReader(data, baseDir);
  List<Node> nodes = new ArrayList<Node>();
  for (int i = 0 ; i < numNodes ; i++)
  {
    nodes.add(new Node(i, "localhost", (8080 + i), (6666 + i), Arrays.asList((4 * i), (4 * i + 1), (4 * i + 2), (4 * i + 3))));
  }
  Cluster cluster = new Cluster("test", nodes);
  SerializerDefinition serDef = new SerializerDefinition("json", "'string'");
  StoreDefinition storeDef = new StoreDefinition("test", ReadOnlyStorageConfiguration.TYPE_NAME, serDef, serDef, RoutingTier.CLIENT, RoutingStrategyType.CONSISTENT_STRATEGY, repFactor, 1, 1, 1, 1, 1);
  RoutingStrategy router = new RoutingStrategyFactory(cluster).getRoutingStrategy(storeDef);
  File outputDir = TestUtils.createTempDir(baseDir);
  JsonStoreBuilder storeBuilder = new JsonStoreBuilder(reader, cluster, storeDef, router, outputDir, null, testSize / 5, 1, 2, 10000);
  storeBuilder.build();
  File nodeDir = TestUtils.createTempDir(baseDir);
  @SuppressWarnings("unchecked")
  Serializer<String> serializer = (Serializer<String>) new DefaultSerializerFactory().getSerializer(serDef);
  Map<Integer, Store<String, String>> nodeStores = Maps.newHashMap();
  for (int i = 0 ; i < numNodes ; i++)
  {
    File currNode = new File(nodeDir, Integer.toString(i));
    currNode.mkdirs();
    currNode.deleteOnExit();
    Utils.move(new File(outputDir, ("node-" + Integer.toString(i))), new File(currNode, "version-0"));
    nodeStores.put(i, new SerializingStore<String, String>(new ReadOnlyStorageEngine("test", currNode, 1, 3, 1000), serializer, serializer));
  }
  return new ReadOnlyStorageEngineTestInstance(data, baseDir, nodeStores, router, serializer);
}