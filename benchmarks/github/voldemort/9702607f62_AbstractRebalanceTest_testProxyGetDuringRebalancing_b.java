class AbstractRebalanceTest{ 
 void testProxyGetDuringRebalancing() {
  final Cluster currentCluster = ServerTestUtils.getLocalCluster(2, new int[][] {
                                                                                  { 0, 1, 2, 3 },
                                                                                  { },
                                                                                });
  final Cluster targetCluster = ServerTestUtils.getLocalCluster(2, new int[][] {
                                                                                 { },
                                                                                 { 0, 1, 2, 3 },
                                                                               });
  final List<Integer> serverList = Arrays.asList(0, 1);
  final Cluster updatedCluster = startServers(currentCluster, storeDefFile, serverList, null);
  ExecutorService executors = Executors.newFixedThreadPool(2);
  final AtomicBoolean rebalancingToken = new AtomicBoolean(false);
  final List<Exception> exceptions = Collections.synchronizedList(new ArrayList<Exception>());
  RebalanceClientConfig rebalanceClientConfig = new RebalanceClientConfig();
  rebalanceClientConfig.setMaxParallelDonors(2);
  rebalanceClientConfig.setMaxParallelRebalancing(2);
  final RebalanceController rebalanceClient = new RebalanceController(getBootstrapUrl(updatedCluster, 0), rebalanceClientConfig);
  populateData(updatedCluster, Arrays.asList(0), rebalanceClient.getAdminClient());
  final SocketStoreClientFactory factory = new SocketStoreClientFactory(new ClientConfig().setBootstrapUrls(getBootstrapUrl(updatedCluster, 0)).setSocketTimeout(120, TimeUnit.SECONDS));
  final StoreClient<String, String> storeClient = new DefaultStoreClient<String, String>(testStoreNameRW, null, factory, 3);
  final boolean[] masterNodeResponded = { false, false };
  executors.execute(new Runnable()
                    {
                      public void run ()
                      {
                        try
                        {
                          List<String> keys = new ArrayList<String>(testEntries.keySet());
                          int nRequests = 0;
                          while ((!rebalancingToken.get()))
                          {
                            int index = (int) (Math.random() * keys.size());
                            try
                            {
                              nRequests++;
                              Versioned<String> value = storeClient.get(keys.get(index));
                              assertNotSame("StoreClient get() should not return null.", null, value);
                              assertEquals("Value returned should be good", new Versioned<String>(testEntries.get(keys.get(index))), value);
                              int masterNode = storeClient.getResponsibleNodes(keys.get(index)).get(0).getId();
                              masterNodeResponded[masterNode] = true;
                            }
                            catch (Exception e)
                            {
                              System.out.println(e);
                              e.printStackTrace();
                              exceptions.add(e);
                            }
                          }
                        }
                        catch (Exception e)
                        {
                          exceptions.add(e);
                        }
                        finally {
                                  factory.close();
                                }
                      }
                    });
  executors.execute(new Runnable()
                    {
                      public void run ()
                      {
                        try
                        {
                          Thread.sleep(500);
                          rebalanceAndCheck(updatedCluster, updateCluster(targetCluster), rebalanceClient, Arrays.asList(1));
                          Thread.sleep(500);
                          rebalancingToken.set(true);
                        }
                        catch (Exception e)
                        {
                          exceptions.add(e);
                        }
                        finally {
                                  try
                                  {
                                    stopServer(serverList);
                                  }
                                  catch (Exception e)
                                  {
                                    throw new RuntimeException(e);
                                  }
                                }
                      }
                    });
  executors.shutdown();
  executors.awaitTermination(300, TimeUnit.SECONDS);
  assertEquals(("Client should see values returned master at both (0,1):(" + masterNodeResponded[0] + "," + masterNodeResponded[1] + ")"), true, (masterNodeResponded[0] && masterNodeResponded[1]));
  if (exceptions.size() > 0)
  {
    for (Exception e : exceptions) {
                                     e.printStackTrace();
                                   }
    fail("Should not see any exceptions.");
  }
}
}