{
  String clusterXml = bootstrapMetadataWithRetries(MetadataStore.CLUSTER_KEY, bootstrapUrls);
  Cluster cluster = clusterMapper.readCluster(new StringReader(clusterXml), false);
  String storesXml = bootstrapMetadataWithRetries(MetadataStore.STORES_KEY, bootstrapUrls);
  List<StoreDefinition> storeDefs = storeMapper.readStoreList(new StringReader(storesXml), false);
  StoreDefinition storeDef = null;
  for (StoreDefinition d : storeDefs) if (d.getName().equals(storeName))
                                        storeDef = d;
  if (storeDef == null)
    throw new BootstrapFailureException("Unknown store '" + storeName + "'.");
  boolean repairReads = !storeDef.isView();
  Map<Integer, Store<ByteArray, byte[], byte[]>> clientMapping = Maps.newHashMap();
  Map<Integer, NonblockingStore> nonblockingStores = Maps.newHashMap();
  Map<Integer, Store<ByteArray, Slop, byte[]>> slopStores = null;
  if (config.isHintedHandoffEnabled() && storeDef.isHintedHandoffEnabled())
    slopStores = Maps.newHashMap();
  for (Node node : cluster.getNodes()) {
                                         Store<ByteArray, byte[], byte[]> store = getStore(storeDef.getName(), node.getHost(), getPort(node), this.requestFormatType);
                                         Store<ByteArray, byte[], byte[]> loggingStore = new LoggingStore(store);
                                         clientMapping.put(node.getId(), loggingStore);
                                         NonblockingStore nonblockingStore = routedStoreFactory.toNonblockingStore(store);
                                         nonblockingStores.put(node.getId(), nonblockingStore);
                                         if (slopStores != null)
                                         {
                                           Store<ByteArray, Slop, byte[]> slopStore = SerializingStore.wrap(getStore("slop", node.getHost(), getPort(node), this.requestFormatType), slopKeySerializer, slopValueSerializer, new IdentitySerializer());
                                           slopStores.put(node.getId(), slopStore);
                                         }
                                       }
  Store<ByteArray, byte[], byte[]> store = routedStoreFactory.create(cluster, storeDef, clientMapping, nonblockingStores, slopStores, repairReads, clientZoneId, getFailureDetector());
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
  if (storeDef.isView() && storeDef.getTransformsSerializer() == null)
    throw new SerializationException("Transforms serializer must be specified with a view ");
  Serializer<T> transformsSerializer = (Serializer<T>) serializerFactory.getSerializer((storeDef.getTransformsSerializer() != null ? storeDef.getTransformsSerializer() : new SerializerDefinition("identity")));
  Store<K, V, T> serializedStore = SerializingStore.wrap(store, keySerializer, valueSerializer, transformsSerializer);
  InconsistencyResolver<Versioned<V>> secondaryResolver = resolver == null ? new TimeBasedInconsistencyResolver() : resolver;
  serializedStore = new InconsistencyResolvingStore<K, V, T>(serializedStore, new ChainedResolver<Versioned<V>>(new VectorClockInconsistencyResolver(), secondaryResolver));
  return serializedStore;
}