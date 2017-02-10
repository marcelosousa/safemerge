class Benchmark{ 
 void initializeStore() {
  this.numThreads = benchmarkProps.getInt(THREADS, MAX_WORKERS);
  this.numIterations = benchmarkProps.getInt(ITERATIONS, 1);
  this.statusIntervalSec = benchmarkProps.getInt(INTERVAL, 0);
  this.verbose = benchmarkProps.getBoolean(VERBOSE, false);
  this.verifyRead = benchmarkProps.getBoolean(VERIFY, false);
  this.ignoreNulls = benchmarkProps.getBoolean(IGNORE_NULLS, false);
  boolean enablePipelineRouted = benchmarkProps.getBoolean(PIPELINE_ROUTED_STORE, false);
  boolean enableHintedHandoff = benchmarkProps.getBoolean(HINTED_HANDOFF, false);
  int clientZoneId = benchmarkProps.getInt(CLIENT_ZONE_ID, (-1));
  int numSelectors = benchmarkProps.getInt(SELECTORS, 4);
  int socketBufferSize = benchmarkProps.getInt(SOCKET_BUFFER_SIZE, (4 * 1024));
  if (benchmarkProps.containsKey(URL))
  {
    if (!benchmarkProps.containsKey(STORE_NAME))
    {
      throw new VoldemortException("Missing storename");
    }
    String socketUrl = benchmarkProps.getString(URL);
    String storeName = benchmarkProps.getString(STORE_NAME);
    ClientConfig clientConfig = new ClientConfig().setMaxThreads(numThreads).setMaxTotalConnections(numThreads).setMaxConnectionsPerNode(numThreads).setBootstrapUrls(socketUrl).setConnectionTimeout(60, TimeUnit.SECONDS).setSocketTimeout(60, TimeUnit.SECONDS).setFailureDetectorRequestLengthThreshold(TimeUnit.SECONDS.toMillis(60)).setSocketBufferSize(socketBufferSize).setEnableHintedHandoff(enableHintedHandoff).setSelectors(numSelectors).setEnablePipelineRoutedStore(enablePipelineRouted);
    if (clientZoneId >= 0)
    {
      clientConfig.setClientZoneId(clientZoneId);
    }
    SocketStoreClientFactory socketFactory = new SocketStoreClientFactory(clientConfig);
    this.storeClient = socketFactory.getStoreClient(storeName);
    StoreDefinition storeDef = getStoreDefinition(socketFactory, storeName);
    this.keyType = findKeyType(storeDef);
    benchmarkProps.put(Benchmark.KEY_TYPE, this.keyType);
    this.factory = socketFactory;
    if (benchmarkProps.getBoolean(HANDSHAKE, false))
    {
      final Object key = getTempKey(this.keyType);
      this.storeClient.delete(key);
      this.storeClient.put(key, "123");
      this.storeClient.delete(key);
    }
  }
  else
  {
    String storageEngineClass = benchmarkProps.getString(STORAGE_CONFIGURATION_CLASS);
    this.keyType = benchmarkProps.getString(KEY_TYPE, STRING_KEY_TYPE);
    Serializer serializer = findKeyType(this.keyType);
    Store<Object, Object> store = null;
    StorageConfiguration conf = (StorageConfiguration) ReflectUtils.callConstructor(ReflectUtils.loadClass(storageEngineClass), new Object[] {
                                                                                                                                               ServerTestUtils.getVoldemortConfig(),
                                                                                                                                             });
    store = SerializingStore.wrap(conf.getStore(DUMMY_DB), serializer, new StringSerializer());
    this.factory = new StaticStoreClientFactory(store);
    this.storeClient = factory.getStoreClient(store.getName());
  }
  this.storeInitialized = true;
}
}