{
  Cluster localCluster = ServerTestUtils.getLocalCluster(2, new int[][] {
                                                                          { 0, 1, 2, 3 },
                                                                          { },
                                                                        });
  Cluster localTargetCluster = ServerTestUtils.getLocalCluster(2, new int[][] {
                                                                                { },
                                                                                { 0, 1, 2, 3 },
                                                                              });
  final List<Integer> serverList = Arrays.asList(0, 1);
  final Cluster updatedCluster = startServers(localCluster, storeDefFile, serverList, null);
  final Cluster targetCluster = updateCluster(localTargetCluster);
  ExecutorService executors = Executors.newFixedThreadPool(2);
  final AtomicBoolean rebalancingToken = new AtomicBoolean(false);
  final List<Exception> exceptions = Collections.synchronizedList(new ArrayList<Exception>());
  RebalanceClientConfig rebalanceClientConfig = new RebalanceClientConfig();
  rebalanceClientConfig.setMaxParallelDonors(2);
  rebalanceClientConfig.setMaxParallelRebalancing(2);
  final RebalanceController rebalanceClient = new RebalanceController(getBootstrapUrl(updatedCluster, 0), rebalanceClientConfig);
  populateData(updatedCluster, Arrays.asList(0), rebalanceClient.getAdminClient());
  Node node = updatedCluster.getNodeById(0);
  final Store<ByteArray, byte[], byte[]> serverSideRoutingStore = getSocketStore(testStoreNameRW, node.getHost(), node.getSocketPort(), true);
  final CountDownLatch latch = new CountDownLatch(1);
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
                              List<Versioned<byte[]>> values = serverSideRoutingStore.get(new ByteArray(ByteUtils.getBytes(keys.get(index), "UTF-8")), null);
                              assertEquals("serverSideRoutingStore should return value.", 1, values.size());
                              assertEquals("Value returned should be good", new Versioned<String>(testEntries.get(keys.get(index))), new Versioned<String>(ByteUtils.getString(values.get(0).getValue(), "UTF-8"), values.get(0).getVersion()));
                            }
                            catch (UnreachableStoreException e)
                            {
                            }
                            catch (Exception e)
                            {
                              exceptions.add(e);
                            }
                          }
                          latch.countDown();
                        }
                        catch (Exception e)
                        {
                          exceptions.add(e);
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
                          rebalanceAndCheck(updatedCluster, targetCluster, rebalanceClient, Arrays.asList(1));
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
                                    latch.await(300, TimeUnit.SECONDS);
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
  if (exceptions.size() > 0)
  {
    for (Exception e : exceptions) {
                                     e.printStackTrace();
                                   }
    fail("Should not see any exceptions !!");
  }
}