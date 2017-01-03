{
  registerEngine(metadata);
  for (String configClassName : voldemortConfig.getStorageConfigurations()) initStorageConfig(configClassName);
  storageConfigs.put(ViewStorageConfiguration.TYPE_NAME, new ViewStorageConfiguration(voldemortConfig, metadata.getStoreDefList(), storeRepository));
  if (voldemortConfig.isSlopEnabled())
  {
    StorageConfiguration config = storageConfigs.get(voldemortConfig.getSlopStoreType());
    if (config == null)
      throw new ConfigurationException("Attempt to slop store failed");
    StorageEngine<ByteArray, byte[]> slopEngine = config.getStore("slop");
    registerEngine(slopEngine);
    storeRepository.setSlopStore(SerializingStorageEngine.wrap(slopEngine, new ByteArraySerializer(), new SlopSerializer()));
  }
  List<StoreDefinition> storeDefs = new ArrayList<StoreDefinition>(this.metadata.getStoreDefList());
  logger.info("Initializing stores:");
  for (StoreDefinition def : storeDefs) if (!def.isView())
                                          openStore(def);
  for (StoreDefinition def : storeDefs) if (def.isView())
                                          openStore(def);
  if (voldemortConfig.isStatTrackingEnabled())
    JmxUtils.registerMbean(new StoreStatsJmx(this.storeStats), JmxUtils.createObjectName("voldemort.store.stats.aggregate", "aggregate-perf"));
  logger.info("All stores initialized.");
}