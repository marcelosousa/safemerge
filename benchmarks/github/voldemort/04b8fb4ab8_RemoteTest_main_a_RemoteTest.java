{
  OptionParser parser = new OptionParser();
  parser.accepts("r", "execute read operations");
  parser.accepts("w", "execute write operations");
  parser.accepts("d", "execute delete operations");
  parser.accepts("m", "generate a mix of read and write requests");
  parser.accepts("v", "verbose");
  parser.accepts("ignore-nulls", "ignore null values");
  parser.accepts("node", "go to this node id").withRequiredArg().ofType(Integer.class);
  parser.accepts("interval", "print requests on this interval").withRequiredArg().ofType(Integer.class);
  parser.accepts("handshake", "perform a handshake");
  parser.accepts("verify", "verify values read");
  parser.accepts("request-file", "execute specific requests in order").withRequiredArg();
  parser.accepts("start-key-index", "starting point when using int keys. Default = 0").withRequiredArg().ofType(Integer.class);
  parser.accepts("value-size", "size in bytes for random value.  Default = 1024").withRequiredArg().ofType(Integer.class);
  parser.accepts("iterations", "number of times to repeat the test  Default = 1").withRequiredArg().ofType(Integer.class);
  parser.accepts("threads", ("max number concurrent worker threads  Default = " + MAX_WORKERS)).withRequiredArg().ofType(Integer.class);
  parser.accepts("percent-cached", "percentage of requests to come from previously requested keys; valid values are in range [0..100]; 0 means caching disabled  Default = 0").withRequiredArg().ofType(Integer.class);
  parser.accepts("help");
  OptionSet options = parser.parse(args);
  List<String> nonOptions = options.nonOptionArguments();
  if (options.has("help"))
  {
    printUsage(System.out, parser);
  }
  if (nonOptions.size() != 3)
  {
    printUsage(System.err, parser);
  }
  String url = nonOptions.get(0);
  String storeName = nonOptions.get(1);
  int numRequests = Integer.parseInt(nonOptions.get(2));
  String ops = "";
  List<Integer> keys = null;
  Integer startNum = CmdUtils.valueOf(options, "start-key-index", 0);
  Integer valueSize = CmdUtils.valueOf(options, "value-size", 1024);
  Integer numIterations = CmdUtils.valueOf(options, "iterations", 1);
  Integer numThreads = CmdUtils.valueOf(options, "threads", MAX_WORKERS);
  Integer percentCached = CmdUtils.valueOf(options, "percent-cached", 0);
  if ((percentCached < 0 || percentCached) > 100)
  {
    printUsage(System.err, parser);
  }
  Integer nodeId = CmdUtils.valueOf(options, "node", 0);
  final Integer interval = CmdUtils.valueOf(options, "interval", 100000);
  final boolean verifyValues = options.has("verify");
  final boolean verbose = options.has("v");
  if (options.has("request-file"))
  {
    keys = loadKeys(((String) options.valueOf("request-file")));
  }
  if (options.has("r"))
  {
    ops += "r";
  }
  if (options.has("w"))
  {
    ops += "w";
  }
  if (options.has("d"))
  {
    ops += "d";
  }
  if (options.has("m"))
  {
    ops += "m";
  }
  if (ops.length() == 0)
  {
    ops = "rwd";
  }
  System.out.println(("operations : " + ops));
  System.out.println(("value size : " + valueSize));
  System.out.println(("start index : " + startNum));
  System.out.println(("iterations : " + numIterations));
  System.out.println(("threads : " + numThreads));
  System.out.println(("cache percentage : " + percentCached + "%"));
  System.out.println("Bootstraping cluster data.");
  ClientConfig clientConfig = new ClientConfig().setMaxThreads(numThreads).setMaxTotalConnections(numThreads).setMaxConnectionsPerNode(numThreads).setBootstrapUrls(url).setConnectionTimeout(60, TimeUnit.SECONDS).setSocketTimeout(60, TimeUnit.SECONDS).setSocketBufferSize((4 * 1024));
  SocketStoreClientFactory factory = new SocketStoreClientFactory(clientConfig);
  final StoreClient<Object, Object, Object> store = factory.getStoreClient(storeName);
  StoreDefinition storeDef = getStoreDefinition(factory, storeName);
  Class<?> keyType = findKeyType(storeDef);
  final String value = TestUtils.randomLetters(valueSize);
  ExecutorService service = Executors.newFixedThreadPool(numThreads);
  if (options.has("handshake"))
  {
    final Object key = getKeyProvider(keyType, startNum, keys, 0).next();
    store.delete(key);
    store.put(key, new Versioned<String>(value));
    store.delete(key);
  }
  for (int loopCount = 0 ; loopCount < numIterations ; loopCount++)
  {
    System.out.println(("======================= iteration = " + loopCount + " ======================================"));
    if (ops.contains("d"))
    {
      System.out.println("Beginning delete test.");
      final AtomicInteger successes = new AtomicInteger(0);
      final KeyProvider<?> keyProvider0 = getKeyProvider(keyType, startNum, keys, percentCached);
      final CountDownLatch latch0 = new CountDownLatch(numRequests);
      final long[] requestTimes = new long[numRequests];
      final long start = System.nanoTime();
      for (int i = 0 ; i < numRequests ; i++)
      {
        final int j = i;
        service.execute(new Runnable()
                        {
                          public void run ()
                          {
                            try
                            {
                              long startNs = System.nanoTime();
                              store.delete(keyProvider0.next());
                              requestTimes[j] = ((System.nanoTime() - startNs) / Time.NS_PER_MS);
                              successes.getAndIncrement();
                            }
                            catch (Exception e)
                            {
                              e.printStackTrace();
                            }
                            finally {
                                      latch0.countDown();
                                      if (j % interval == 0)
                                      {
                                        printStatistics("deletes", successes.get(), start);
                                      }
                                    }
                          }
                        });
      }
      latch0.await();
      printStatistics("deletes", successes.get(), start);
      System.out.println(("95th percentile delete latency: " + TestUtils.quantile(requestTimes, 0.95) + " ms."));
      System.out.println(("99th percentile delete latency: " + TestUtils.quantile(requestTimes, 0.99) + " ms."));
    }
    if (ops.contains("w"))
    {
      final AtomicInteger numWrites = new AtomicInteger(0);
      System.out.println("Beginning write test.");
      final KeyProvider<?> keyProvider1 = getKeyProvider(keyType, startNum, keys, percentCached);
      final CountDownLatch latch1 = new CountDownLatch(numRequests);
      final long[] requestTimes = new long[numRequests];
      final long start = System.nanoTime();
      for (int i = 0 ; i < numRequests ; i++)
      {
        final int j = i;
        service.execute(new Runnable()
                        {
                          public void run ()
                          {
                            try
                            {
                              final Object key = keyProvider1.next();
                              store.applyUpdate(new UpdateAction<Object, Object, Object>()
                                                {
                                                  public void update (StoreClient<Object, Object, Object> storeClient)
                                                  {
                                                    long startNs = System.nanoTime();
                                                    storeClient.put(key, value);
                                                    requestTimes[j] = ((System.nanoTime() - startNs) / Time.NS_PER_MS);
                                                    numWrites.incrementAndGet();
                                                  }
                                                }, 64);
                            }
                            catch (Exception e)
                            {
                              if (verbose)
                              {
                                e.printStackTrace();
                              }
                            }
                            finally {
                                      latch1.countDown();
                                      if (j % interval == 0)
                                      {
                                        printStatistics("writes", numWrites.get(), start);
                                      }
                                    }
                          }
                        });
      }
      latch1.await();
      printStatistics("writes", numWrites.get(), start);
      System.out.println(("95th percentile write latency: " + TestUtils.quantile(requestTimes, 0.95) + " ms."));
      System.out.println(("99th percentile write latency: " + TestUtils.quantile(requestTimes, 0.99) + " ms."));
    }
    if (ops.contains("r"))
    {
      final boolean ignoreNulls = options.has("ignore-nulls");
      final AtomicInteger numReads = new AtomicInteger(0);
      final AtomicInteger numNulls = new AtomicInteger(0);
      System.out.println("Beginning read test.");
      final KeyProvider<?> keyProvider = getKeyProvider(keyType, startNum, keys, percentCached);
      final CountDownLatch latch = new CountDownLatch(numRequests);
      final long[] requestTimes = new long[numRequests];
      final long start = System.nanoTime();
      keyProvider.next();
      for (int i = 0 ; i < numRequests ; i++)
      {
        final int j = i;
        service.execute(new Runnable()
                        {
                          public void run ()
                          {
                            try
                            {
                              Object key = keyProvider.next();
                              long startNs = System.nanoTime();
                              Versioned<Object> v = store.get(key);
                              requestTimes[j] = ((System.nanoTime() - startNs) / Time.NS_PER_MS);
                              numReads.incrementAndGet();
                              if (v == null)
                              {
                                numNulls.incrementAndGet();
                                if (!ignoreNulls)
                                {
                                  throw new Exception("value returned is null for key " + key);
                                }
                              }
                              if (verifyValues && !value.equals(v.getValue()))
                              {
                                throw new Exception("value returned isn't same as set value for key " + key);
                              }
                            }
                            catch (Exception e)
                            {
                              if (verbose)
                              {
                                e.printStackTrace();
                              }
                            }
                            finally {
                                      latch.countDown();
                                      if (j % interval == 0)
                                      {
                                        printStatistics("reads", numReads.get(), start);
                                        printNulls(numNulls.get(), start);
                                      }
                                    }
                          }
                        });
      }
      latch.await();
      printStatistics("reads", numReads.get(), start);
      System.out.println(("95th percentile read latency: " + TestUtils.quantile(requestTimes, 0.95) + " ms."));
      System.out.println(("99th percentile read latency: " + TestUtils.quantile(requestTimes, 0.99) + " ms."));
    }
  }
  if (ops.contains("m"))
  {
    final AtomicInteger numNulls = new AtomicInteger(0);
    final AtomicInteger numReads = new AtomicInteger(0);
    final AtomicInteger numWrites = new AtomicInteger(0);
    System.out.println("Beginning mixed test.");
    final KeyProvider<?> keyProvider = getKeyProvider(keyType, startNum, keys, percentCached);
    final CountDownLatch latch = new CountDownLatch(numRequests);
    final long start = System.nanoTime();
    keyProvider.next();
    for (int i = 0 ; i < numRequests ; i++)
    {
      final int j = i;
      service.execute(new Runnable()
                      {
                        public void run ()
                        {
                          try
                          {
                            final Object key = keyProvider.next();
                            store.applyUpdate(new UpdateAction<Object, Object, Object>()
                                              {
                                                public void update (StoreClient<Object, Object, Object> storeClient)
                                                {
                                                  Versioned<Object> v = store.get(key);
                                                  numReads.incrementAndGet();
                                                  if ((v != null))
                                                  {
                                                    storeClient.put(key, v);
                                                  }
                                                  else
                                                  {
                                                    numNulls.incrementAndGet();
                                                  }
                                                  numWrites.incrementAndGet();
                                                }
                                              }, 64);
                          }
                          catch (Exception e)
                          {
                            if (verbose)
                            {
                              e.printStackTrace();
                            }
                          }
                          finally {
                                    if (j % interval == 0)
                                    {
                                      printStatistics("reads", numReads.get(), start);
                                      printStatistics("writes", numWrites.get(), start);
                                      printNulls(numNulls.get(), start);
                                      printStatistics("transactions", j, start);
                                    }
                                    latch.countDown();
                                  }
                        }
                      });
    }
    latch.await();
    printStatistics("reads", numReads.get(), start);
    printStatistics("writes", numWrites.get(), start);
  }
  System.exit(0);
}