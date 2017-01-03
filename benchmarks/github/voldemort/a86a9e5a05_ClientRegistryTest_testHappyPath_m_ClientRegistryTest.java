{
  List<Integer> emptyPartitionList = Lists.newArrayList();
  ClientConfig clientConfig = new ClientConfig().setMaxThreads(4).setMaxTotalConnections(4).setMaxConnectionsPerNode(4).setBootstrapUrls((SERVER_LOCAL_URL + serverPorts[0])).setClientContextName(CLIENT_CONTEXT_NAME).setClientRegistryUpdateInSecs(CLIENT_REGISTRY_REFRSH_INTERVAL).setEnableLazy(false);
  SocketStoreClientFactory socketFactory = new SocketStoreClientFactory(clientConfig);
  StoreClient<String, String> client1 = socketFactory.getStoreClient(TEST_STORE_NAME);
  client1.put("k", "v");
  Iterator<Pair<ByteArray, Versioned<byte[]>>> it = adminClient.fetchEntries(0, SystemStoreConstants.SystemStoreName.voldsys$_client_registry.name(), emptyPartitionList, null, false);
  ArrayList<ClientInfo> infoList = getClientRegistryContent(it);
  assertEquals(TEST_STORE_NAME, infoList.get(0).getStoreName());
  assertEquals(CLIENT_CONTEXT_NAME, infoList.get(0).getContext());
  assertEquals(0, infoList.get(0).getClientSequence());
  assertTrue("Client registry bootstrap time incorrect", (startTime <= infoList.get(0).getBootstrapTime()));
  assertNotNull("Client version is null", infoList.get(0).getReleaseVersion());
  assertEquals(1, infoList.size());
  it = adminClient.fetchEntries(1, SystemStoreConstants.SystemStoreName.voldsys$_client_registry.name(), emptyPartitionList, null, false);
  infoList = getClientRegistryContent(it);
  assertEquals(TEST_STORE_NAME, infoList.get(0).getStoreName());
  assertEquals(CLIENT_CONTEXT_NAME, infoList.get(0).getContext());
  assertEquals(0, infoList.get(0).getClientSequence());
  assertTrue("Client registry bootstrap time incorrect", (startTime <= infoList.get(0).getBootstrapTime()));
  assertNotNull("Client version is null", infoList.get(0).getReleaseVersion());
  assertEquals(1, infoList.size());
  try
  {
    Thread.sleep((CLIENT_REGISTRY_REFRSH_INTERVAL * 1000 * 5));
  }
  catch (InterruptedException e)
  {
  }
  it = adminClient.fetchEntries(1, SystemStoreConstants.SystemStoreName.voldsys$_client_registry.name(), emptyPartitionList, null, false);
  infoList = getClientRegistryContent(it);
  assertTrue("Client registry not updated.", (infoList.get(0).getBootstrapTime() < infoList.get(0).getUpdateTime()));
  assertTrue("Client Config received from the Client registry system store is incorrect.", isConfigEqual(infoList.get(0).getClientConfig(), clientConfig));
  socketFactory.close();
}