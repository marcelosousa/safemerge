{
  List<Integer> emptyPartitionList = Lists.newArrayList();
  ClientConfig clientConfig = new ClientConfig().setMaxThreads(4).setMaxTotalConnections(4).setMaxConnectionsPerNode(4).setBootstrapUrls((SERVER_LOCAL_URL + serverPorts[0])).setClientContextName(CLIENT_CONTEXT_NAME).setClientRegistryUpdateInSecs(CLIENT_REGISTRY_REFRSH_INTERVAL).setEnableLazy(false);
  SocketStoreClientFactory socketFactory1 = new SocketStoreClientFactory(clientConfig);
  ClientConfig clientConfig2 = new ClientConfig().setMaxThreads(4).setMaxTotalConnections(4).setMaxConnectionsPerNode(4).setBootstrapUrls((SERVER_LOCAL_URL + serverPorts[0])).setClientContextName(CLIENT_CONTEXT_NAME2).setClientRegistryUpdateInSecs(CLIENT_REGISTRY_REFRSH_INTERVAL).setEnableLazy(false);
  SocketStoreClientFactory socketFactory2 = new SocketStoreClientFactory(clientConfig2);
  StoreClient<String, String> client1 = socketFactory1.getStoreClient(TEST_STORE_NAME);
  StoreClient<String, String> client2 = socketFactory2.getStoreClient(TEST_STORE_NAME2);
  client1.put("k1", "v1");
  client2.put("k2", "v2");
  Iterator<Pair<ByteArray, Versioned<byte[]>>> it = adminClient.fetchEntries(0, SystemStoreConstants.SystemStoreName.voldsys$_client_registry.name(), emptyPartitionList, null, false);
  ArrayList<ClientInfo> infoList = getClientRegistryContent(it);
  assertNotNull("Client version is null", infoList.get(0).getReleaseVersion());
  assertNotNull("Client version is null", infoList.get(1).getReleaseVersion());
  if (infoList.get(0).getStoreName().equals(TEST_STORE_NAME))
  {
    assertEquals(CLIENT_CONTEXT_NAME, infoList.get(0).getContext());
    assertEquals(0, infoList.get(0).getClientSequence());
    assertTrue("Client registry bootstrap time incorrect", (startTime <= infoList.get(0).getBootstrapTime()));
    assertEquals(TEST_STORE_NAME2, infoList.get(1).getStoreName());
    assertEquals(CLIENT_CONTEXT_NAME2, infoList.get(1).getContext());
    assertEquals(0, infoList.get(1).getClientSequence());
    assertTrue("Client registry bootstrap time incorrect", (startTime <= infoList.get(1).getBootstrapTime()));
    assertTrue("Client registry bootstrap time incorrect", (infoList.get(1).getBootstrapTime() >= infoList.get(0).getBootstrapTime()));
  }
  else
  {
    assertEquals(TEST_STORE_NAME2, infoList.get(0).getStoreName());
    assertEquals(CLIENT_CONTEXT_NAME2, infoList.get(0).getContext());
    assertEquals(0, infoList.get(0).getClientSequence());
    assertTrue("Client registry bootstrap time incorrect", (startTime <= infoList.get(0).getBootstrapTime()));
    assertEquals(TEST_STORE_NAME, infoList.get(1).getStoreName());
    assertEquals(CLIENT_CONTEXT_NAME, infoList.get(1).getContext());
    assertEquals(0, infoList.get(1).getClientSequence());
    assertTrue("Client registry bootstrap time incorrect", (startTime <= infoList.get(1).getBootstrapTime()));
    assertTrue("Client registry bootstrap time incorrect", (infoList.get(0).getBootstrapTime() >= infoList.get(1).getBootstrapTime()));
  }
  it = adminClient.fetchEntries(1, SystemStoreConstants.SystemStoreName.voldsys$_client_registry.name(), emptyPartitionList, null, false);
  infoList = getClientRegistryContent(it);
  assertNotNull("Client version is null", infoList.get(0).getReleaseVersion());
  assertNotNull("Client version is null", infoList.get(1).getReleaseVersion());
  if (infoList.get(0).getStoreName().equals(TEST_STORE_NAME))
  {
    assertEquals(CLIENT_CONTEXT_NAME, infoList.get(0).getContext());
    assertEquals(0, infoList.get(0).getClientSequence());
    assertTrue("Client registry bootstrap time incorrect", (startTime <= infoList.get(0).getBootstrapTime()));
    assertEquals(TEST_STORE_NAME2, infoList.get(1).getStoreName());
    assertEquals(CLIENT_CONTEXT_NAME2, infoList.get(1).getContext());
    assertEquals(0, infoList.get(1).getClientSequence());
    assertTrue("Client registry bootstrap time incorrect", (startTime <= infoList.get(1).getBootstrapTime()));
    assertTrue("Client registry bootstrap time incorrect", (infoList.get(1).getBootstrapTime() >= infoList.get(0).getBootstrapTime()));
  }
  else
  {
    assertEquals(TEST_STORE_NAME2, infoList.get(0).getStoreName());
    assertEquals(CLIENT_CONTEXT_NAME2, infoList.get(0).getContext());
    assertEquals(0, infoList.get(0).getClientSequence());
    assertTrue("Client registry bootstrap time incorrect", (startTime <= infoList.get(0).getBootstrapTime()));
    assertEquals(TEST_STORE_NAME, infoList.get(1).getStoreName());
    assertEquals(CLIENT_CONTEXT_NAME, infoList.get(1).getContext());
    assertEquals(0, infoList.get(1).getClientSequence());
    assertTrue("Client registry bootstrap time incorrect", (startTime <= infoList.get(1).getBootstrapTime()));
    assertTrue("Client registry bootstrap time incorrect", (infoList.get(0).getBootstrapTime() >= infoList.get(1).getBootstrapTime()));
  }
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
  assertTrue("Client registry not updated.", (infoList.get(1).getBootstrapTime() < infoList.get(1).getUpdateTime()));
  ((ZenStoreClient<String, String>) client1).close();
  ((ZenStoreClient<String, String>) client2).close();
}