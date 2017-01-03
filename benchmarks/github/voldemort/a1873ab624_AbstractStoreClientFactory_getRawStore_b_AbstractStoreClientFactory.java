{
  logger.info(("Client zone-id [" + clientZoneId + "] Attempting to obtain metadata for store [" + storeName + "] "));
  if (logger.isDebugEnabled())
  {
    for (URI uri : bootstrapUrls) {
                                    logger.debug(("Client Bootstrap url [" + uri + "]"));
                                  }
  }
  String clusterXml = bootstrapMetadataWithRetries(MetadataStore.CLUSTER_KEY, bootstrapUrls);
  Cluster cluster = clusterMapper.readCluster(new StringReader(clusterXml), false);
  String storesXml = bootstrapMetadataWithRetries(MetadataStore.STORES_KEY, bootstrapUrls);
  if (logger.isDebugEnabled())
  {
    logger.debug(("Obtained cluster metadata xml" + clusterXml));
    logger.debug(("Obtained stores  metadata xml" + storesXml));
  }
  List<StoreDefinition> storeDefs = storeMapper.readStoreList(new StringReader(storesXml), false);
  StoreDefinition storeDef = null;
  for (StoreDefinition d : storeDefs) if (d.getName().equals(storeName))
                                        storeDef = d;
  if (storeDef == null)
    throw new BootstrapFailureException("Unknown store '" + storeName + "'.");
  if (logger.isDebugEnabled())
  {
    logger.debug(cluster.toString(true));
    logger.debug(storeDef.toString());
  }
  boolean repairReads = !storeDef.isView();
  Map<Integer, Store<ByteArray, byte[], byte[]>> clientMapping = Maps.newHashMap();
  Map<Integer, NonblockingStore> nonblockingStores = Maps.newHashMap();
  Map<Integer, NonblockingStore> nonblockingSlopStores = Maps.newHashMap();
  Map<Integer, Store<ByteArray, Slop, byte[]>> slopStores = null;
  if (storeDef.hasHintedHandoffStrategyType())
    slopStores = Maps.newHashMap();
  for (Node node : cluster.getNodes()) {
                                         Store<ByteArray, byte[], byte[]> store = getStore(storeDef.getName(), node.getHost(), getPort(node), this.requestFormatType);
                                         clientMapping.put(node.getId(), store);
                                         NonblockingStore nonblockingStore = routedStoreFactory.toNonblockingStore(store);
                                         nonblockingStores.put(node.getId(), nonblockingStore);
                                         if (slopStores != null)
                                         {
                                           Store<ByteArray, byte[], byte[]> rawSlopStore = getStore("slop", node.getHost(), getPort(node), this.requestFormatType);
                                           Store<ByteArray, Slop, byte[]> slopStore = SerializingStore.wrap(rawSlopStore, slopKeySerializer, slopValueSerializer, new IdentitySerializer());
                                           slopStores.put(node.getId(), slopStore);
                                           nonblockingSlopStores.put(node.getId(), routedStoreFactory.toNonblockingStore(rawSlopStore));
                                         }
                                       }
  Store<ByteArray, byte[], byte[]> store = routedStoreFactory.create(cluster, storeDef, clientMapping, nonblockingStores, slopStores, nonblockingSlopStores, repairReads, clientZoneId, getFailureDetector());
  store = new LoggingStore(store);
  if (isJmxEnabled)
  {
    StatTrackingStore statStore = new StatTrackingStore(store, this.stats);
    store = statStore;
    JmxUtils.registerMbean(new StoreStatsJmx(statStore.getStats()), JmxUtils.createObjectName(JmxUtils.getPackageName(store.getClass()), (clientContextName + "." + store.getName() + jmxId() + (null == clientId ? "" : "." + clientId.toString()))));
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