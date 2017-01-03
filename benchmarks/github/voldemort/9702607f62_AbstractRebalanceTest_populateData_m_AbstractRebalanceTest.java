{
  Map<Integer, Store<ByteArray, byte[], byte[]>> storeMap = new HashMap<Integer, Store<ByteArray, byte[], byte[]>>();
  for (int nodeId : nodeList) {
                                Node node = cluster.getNodeById(nodeId);
                                storeMap.put(nodeId, getSocketStore(testStoreNameRW, node.getHost(), node.getSocketPort()));
                              }
  RoutingStrategy routing = new ConsistentRoutingStrategy(cluster.getNodes(), 1);
  for (Entry<String, String> entry : testEntries.entrySet()) {
                                                               int masterNode = routing.routeRequest(ByteUtils.getBytes(entry.getKey(), "UTF-8")).get(0).getId();
                                                               if (nodeList.contains(masterNode))
                                                               {
                                                                 try
                                                                 {
                                                                   ByteArray keyBytes = new ByteArray(ByteUtils.getBytes(entry.getKey(), "UTF-8"));
                                                                   storeMap.get(masterNode).put(keyBytes, new Versioned<byte[]>(ByteUtils.getBytes(entry.getValue(), "UTF-8")), null);
                                                                 }
                                                                 catch (ObsoleteVersionException e)
                                                                 {
                                                                   System.out.println("Why are we seeing this at all here ?? ");
                                                                   e.printStackTrace();
                                                                 }
                                                               }
                                                             }
  for (Store<ByteArray, byte[], byte[]> store : storeMap.values()) {
                                                                     store.close();
                                                                   }
  File baseDir = TestUtils.createTempDir();
  JsonReader reader = ReadOnlyStorageEngineTestInstance.makeTestDataReader(testEntries, baseDir);
  StoreDefinition def = null;
  for (StoreDefinition storeDef : storeDefs) {
                                               if (storeDef.getName().compareTo(testStoreNameRO) == 0)
                                               {
                                                 def = storeDef;
                                                 break;
                                               }
                                             }
  Utils.notNull(def);
  RoutingStrategy router = new RoutingStrategyFactory().updateRoutingStrategy(def, cluster);
  File outputDir = TestUtils.createTempDir(baseDir);
  JsonStoreBuilder storeBuilder = new JsonStoreBuilder(reader, cluster, def, router, outputDir, null, testEntries.size() / 5, 1, 2, 10000, false);
  storeBuilder.build();
  AdminStoreSwapper swapper = new AdminStoreSwapper(cluster, Executors.newFixedThreadPool(nodeList.size()), adminClient, 100000);
  swapper.swapStoreData(testStoreNameRO, outputDir.getAbsolutePath(), 1L);
}