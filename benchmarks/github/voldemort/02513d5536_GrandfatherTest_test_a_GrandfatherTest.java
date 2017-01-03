{
  int numStores = currentStoreDefs.size();
  RoutingStrategy beforeStrategy = new RoutingStrategy[numStores];
  RoutingStrategy afterStrategy = new RoutingStrategy[numStores];
  RoutingStrategyFactory routingFactory = new RoutingStrategyFactory();
  for (int i = 0 ; i < numStores ; i++)
  {
    beforeStrategy[i] = routingFactory.updateRoutingStrategy(currentStoreDefs.get(i), currentCluster);
    afterStrategy[i] = routingFactory.updateRoutingStrategy(targetStoreDefs.get(i), targetCluster);
  }
  VoldemortServer voldemortServer = new VoldemortServer[currentCluster.getNumberOfNodes()];
  for (int nodeId = 0 ; nodeId < currentCluster.getNumberOfNodes() ; nodeId++)
  {
    voldemortServer[nodeId] = startServer(nodeId, currentStoreDefs, currentCluster);
  }
  Thread.sleep((10 * 1000));
  try
  {
    ClientConfig config = new ClientConfig().setBootstrapUrls(("tcp://localhost:" + currentCluster.getNodeById(0).getSocketPort())).setFailureDetectorBannagePeriod(1).setMaxBootstrapRetries(10).setConnectionTimeout(3000, TimeUnit.MILLISECONDS).setMaxConnectionsPerNode(10).setSelectors(8);
    factory = new SocketStoreClientFactory(config);
    final StoreClient<String, String> storeClients = new StoreClient[numStores];
    for (int storeNo = 1 ; storeNo <= numStores ; storeNo++)
    {
      storeClients[storeNo - 1] = factory.getStoreClient(("test" + storeNo));
    }
    for (int i = 0 ; i < NUM_KEYS ; i++)
    {
      for (int storeNo = 1 ; storeNo <= numStores ; storeNo++)
      {
        storeClients[(storeNo - 1)].put(("key" + i), ("value" + i + "_" + 1));
      }
    }
    final AdminClient adminClient = ServerTestUtils.getAdminClient(currentCluster);
    ExecutorService service = Executors.newFixedThreadPool(2);
    Future<?> future = service.submit(new Runnable()
                                      {
                                        public void run ()
                                        {
                                          MigratePartitions migrate = new MigratePartitions(currentCluster, targetCluster, currentStoreDefs, targetStoreDefs, adminClient, createTempVoldemortConfig(), null, false);
                                          migrate.migrate();
                                        }
                                      });
    for (int i = 0 ; i < NUM_KEYS ; i++)
    {
      for (int storeNo = 1 ; storeNo <= numStores ; storeNo++)
      {
        try
        {
          storeClients[(storeNo - 1)].put(("key" + i), ("value" + i + "_" + 2));
        }
        catch (Exception e)
        {
          storeClients[(storeNo - 1)].put(("key" + i), ("value" + i + "_" + 2));
        }
      }
    }
    try
    {
      future.get();
    }
    catch (ExecutionException e)
    {
      e.printStackTrace();
    }
    Thread.sleep((120 * 1000));
    factory.close();
    for (int nodeId = 0 ; nodeId < currentCluster.getNumberOfNodes() ; nodeId++)
    {
      voldemortServer[nodeId].getMetadataStore().put(MetadataStore.CLUSTER_KEY, targetCluster);
      voldemortServer[nodeId].getMetadataStore().put(MetadataStore.STORES_KEY, targetStoreDefs);
    }
    for (int zoneId = 0 ; zoneId < targetCluster.getZones().size() ; zoneId++)
    {
      config = new ClientConfig().setBootstrapUrls(("tcp://localhost:" + currentCluster.getNodeById(0).getSocketPort())).setMaxBootstrapRetries(10).setMaxConnectionsPerNode(10).setSelectors(8).setConnectionTimeout(3000, TimeUnit.MILLISECONDS).setClientZoneId(zoneId);
      factory = new SocketStoreClientFactory(config);
      for (int storeNo = 1 ; storeNo <= numStores ; storeNo++)
      {
        storeClients[storeNo - 1] = factory.getStoreClient(("test" + storeNo));
      }
      for (int storeNo = 1 ; storeNo <= numStores ; storeNo++)
      {
        for (int i = 0 ; i < NUM_KEYS ; i++)
        {
          Versioned<String> value = null;
          try
          {
            value = storeClients[(storeNo - 1)].get(("key" + i), null);
          }
          catch (Exception e)
          {
            value = storeClients[(storeNo - 1)].get(("key" + i), null);
          }
          if (value == null)
          {
            Assert.fail(("Should not have happened for key" + i + " => " + beforeStrategy[(storeNo - 1)].getPartitionList(new String(("key" + i)).getBytes()) + " => " + afterStrategy[(storeNo - 1)].getPartitionList(new String(("key" + i)).getBytes())));
          }
          else
          {
            Assert.assertEquals(value.getValue(), ("value" + i + "_" + 2));
          }
        }
      }
      factory.close();
      factory = null;
    }
    SocketStoreFactory socketStoreFactory = null;
    try
    {
      socketStoreFactory = new ClientRequestExecutorPool(2, 10000, 100000, 32 * 1024);
      StringSerializer serializer = new StringSerializer();
      for (int storeNo = 1 ; storeNo <= numStores ; storeNo++)
      {
        StoreDefinition storeDefToTest = targetStoreDefs.get((storeNo - 1));
        RoutingStrategy strategy = new RoutingStrategyFactory().updateRoutingStrategy(storeDefToTest, targetCluster);
        HashMap<Integer, Store<ByteArray, byte[], byte[]>> socketStoresPerNode = Maps.newHashMap();
        for (Node node : targetCluster.getNodes()) {
                                                     socketStoresPerNode.put(node.getId(), socketStoreFactory.create(storeDefToTest.getName(), node.getHost(), node.getSocketPort(), RequestFormatType.PROTOCOL_BUFFERS, RequestRoutingType.IGNORE_CHECKS));
                                                   }
        for (int i = 0 ; i < NUM_KEYS ; i++)
        {
          byte[] keyBytes = serializer.toBytes(("key" + i));
          List<Node> responsibleNodes = strategy.routeRequest(keyBytes);
          for (Node node : responsibleNodes) {
                                               List<Versioned<byte[]>> value = socketStoresPerNode.get(node.getId()).get(new ByteArray(keyBytes), null);
                                               Assert.assertNotNull(value);
                                               Assert.assertNotSame(value.size(), 0);
                                               Assert.assertEquals(serializer.toObject(value.get(0).getValue()), ("value" + i + "_2"));
                                             }
        }
      }
    }
    finally {
              if (socketStoreFactory != null)
                socketStoreFactory.close();
            }
  }
  catch (Exception e)
  {
    e.printStackTrace();
  }
  finally {
            if (factory != null)
              factory.close();
            stopServer(Lists.newArrayList(targetCluster.getNodes()));
          }
}