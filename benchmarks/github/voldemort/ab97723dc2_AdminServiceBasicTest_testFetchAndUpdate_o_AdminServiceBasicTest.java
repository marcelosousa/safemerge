class AdminServiceBasicTest{ 
 void testFetchAndUpdate() {
  Store<ByteArray, byte[]> store = server.getStoreRepository().getStorageEngine(storeName);
  assertNotSame(("Store '" + storeName + "' should not be null"), null, store);
  Set<Pair<ByteArray, Versioned<byte[]>>> entrySet = createEntries();
  for (Pair<ByteArray, Versioned<byte[]>> entry : entrySet) {
                                                              store.put(entry.getFirst(), entry.getSecond());
                                                            }
  VoldemortConfig config2 = ServerTestUtils.createServerConfig(1, TestUtils.createTempDir().getAbsolutePath(), null, storesXmlfile);
  VoldemortServer server2 = new VoldemortServer(config2, cluster);
  server2.start();
  for (Pair<ByteArray, Versioned<byte[]>> entry : entrySet) {
                                                              assertEquals("Server2 should return empty result List for all", 0, server2.getStoreRepository().getStorageEngine(storeName).get(entry.getFirst()).size());
                                                            }
  AdminClient client = ServerTestUtils.getAdminClient(server2.getIdentityNode(), server2.getMetadataStore());
  client.fetchAndUpdateStreams(0, 1, storeName, Arrays.asList(0, 1), null);
  Store<ByteArray, byte[]> store2 = server2.getStoreRepository().getStorageEngine(storeName);
  assertNotSame(("Store '" + storeName + "' should not be null"), null, store2);
  StoreDefinition storeDef = server.getMetadataStore().getStoreDef(storeName);
  assertNotSame("StoreDefinition for 'users' should not be null", null, storeDef);
  RoutingStrategy routingStrategy = new RoutingStrategyFactory().updateRoutingStrategy(storeDef, server.getMetadataStore().getCluster());
  int checked = 0;
  int matched = 0;
  for (int i = 100 ; i <= 1000 ; i++)
  {
    ByteArray key = new ByteArray(ByteUtils.getBytes(("" + i), "UTF-8"));
    byte[] value = ByteUtils.getBytes(("value-" + i), "UTF-8");
    if ((routingStrategy.getPartitionList(key.get()).get(0) == 0 || routingStrategy.getPartitionList(key.get()).get(0)) == 1)
    {
      checked++;
      if (store2.get(key).size() > 0 && new String(value).equals(new String(store2.get(key).get(0).getValue())))
      {
        matched++;
      }
    }
  }
  server2.stop();
  assertEquals("All Values should have matched", checked, matched);
}
}