class ClientRegistryTest{ 
 void testRepeatRegistrationSameFactory() {
  List<Integer> emptyPartitionList = Lists.newArrayList();
  ClientConfig clientConfig = new ClientConfig().setMaxThreads(4).setMaxTotalConnections(4).setMaxConnectionsPerNode(4).setBootstrapUrls((SERVER_LOCAL_URL + serverPorts[1])).setClientContextName(CLIENT_CONTEXT_NAME).setClientRegistryUpdateInSecs(CLIENT_REGISTRY_REFRSH_INTERVAL).setEnableLazy(false);
  SocketStoreClientFactory socketFactory1 = new SocketStoreClientFactory(clientConfig);
  ClientConfig clientConfig2 = new ClientConfig().setMaxThreads(4).setMaxTotalConnections(4).setMaxConnectionsPerNode(4).setBootstrapUrls((SERVER_LOCAL_URL + serverPorts[1])).setClientContextName(CLIENT_CONTEXT_NAME2).setClientRegistryUpdateInSecs(CLIENT_REGISTRY_REFRSH_INTERVAL).setEnableLazy(false);
  SocketStoreClientFactory socketFactory2 = new SocketStoreClientFactory(clientConfig2);
  for (int i = 0 ; i < 3 ; i++)
  {
    StoreClient<String, String> client1 = socketFactory1.getStoreClient(TEST_STORE_NAME);
    StoreClient<String, String> client2 = socketFactory2.getStoreClient(TEST_STORE_NAME2);
    client1.put("k1", "v1");
    client2.put("k2", "v2");
    ((ZenStoreClient<String, String>) client1).close();
    ((ZenStoreClient<String, String>) client2).close();
  }
  Iterator<Pair<ByteArray, Versioned<byte[]>>> it = adminClient.fetchEntries(1, SystemStoreConstants.SystemStoreName.voldsys$_client_registry.name(), emptyPartitionList, null, false);
  ArrayList<ClientInfo> infoList = getClientRegistryContent(it);
  assertEquals("Incrrect # of entries created in client registry", 6, infoList.size());
}
}