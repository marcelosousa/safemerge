class AbstractStoreClientFactory{ 
 void getRawStore() {
  String clusterXml = bootstrapMetadataWithRetries(MetadataStore.CLUSTER_KEY, bootstrapUrls);
  Cluster cluster = clusterMapper.readCluster(new StringReader(clusterXml), false);
  String storesXml = bootstrapMetadataWithRetries(MetadataStore.STORES_KEY, bootstrapUrls);
  List<StoreDefinition> storeDefs = storeMapper.readStoreList(new StringReader(storesXml), false);
  StoreDefinition storeDef = null;
  for (StoreDefinition d : storeDefs) if (d.getName().equals(storeName))
                                        storeDef = d;
  if (storeDef == null)
    throw new BootstrapFailureException("Unknown store '" + storeName + "'.");
  Map<Integer, Store<ByteArray, byte[]>> clientMapping = Maps.newHashMap();
  for (Node node : cluster.getNodes()) {
                                         Store<ByteArray, byte[]> store = getStore(storeDef.getName(), node.getHost(), getPort(node), this.requestFormatType);
                                         store = new LoggingStore(store);
                                         clientMapping.put(node.getId(), store);
                                       }
  boolean repairReads = !storeDef.isView();
  Store<ByteArray, byte[]> store = new RoutedStore(storeName, clientMapping, cluster, storeDef, repairReads, threadPool, routingTimeoutMs, getFailureDetector(), SystemTime.INSTANCE);
  if (isJmxEnabled)
  {
    StatTrackingStore statStore = new StatTrackingStore(store, this.stats);
    store = statStore;
    JmxUtils.registerMbean(new StoreStatsJmx(statStore.getStats()), JmxUtils.createObjectName(JmxUtils.getPackageName(store.getClass()), (store.getName() + jmxId())));
  }
  if (storeDef.getKeySerializer().hasCompression() || storeDef.getValueSerializer().hasCompression())
  {
    store = new CompressingStore(store, getCompressionStrategy(storeDef.getKeySerializer()), getCompressionStrategy(storeDef.getValueSerializer()));
  }
  Serializer<K> keySerializer = (Serializer<K>) serializerFactory.getSerializer(storeDef.getKeySerializer());
  Serializer<V> valueSerializer = (Serializer<V>) serializerFactory.getSerializer(storeDef.getValueSerializer());
  Store<K, V> serializedStore = SerializingStore.wrap(store, keySerializer, valueSerializer);
  InconsistencyResolver<Versioned<V>> secondaryResolver = resolver == null ? new TimeBasedInconsistencyResolver() : resolver;
  serializedStore = new InconsistencyResolvingStore<K, V>(serializedStore, new ChainedResolver<Versioned<V>>(new VectorClockInconsistencyResolver(), secondaryResolver));
  return serializedStore;
}
}