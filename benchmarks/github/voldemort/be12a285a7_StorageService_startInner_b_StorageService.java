{
  registerEngine(metadata, false, "metadata");
  for (String configClassName : voldemortConfig.getStorageConfigurations()) initStorageConfig(configClassName);
  storageConfigs.put(ViewStorageConfiguration.TYPE_NAME, new ViewStorageConfiguration(voldemortConfig, metadata.getStoreDefList(), storeRepository));
  if (voldemortConfig.isSlopEnabled())
  {
    logger.info(("Initializing the slop store using " + voldemortConfig.getSlopStoreType()));
    StorageConfiguration config = storageConfigs.get(voldemortConfig.getSlopStoreType());
    if (config == null)
      throw new ConfigurationException("Attempt to open store " + SlopStorageEngine.SLOP_STORE_NAME + " but " + voldemortConfig.getSlopStoreType() + " storage engine has not been enabled.");
    StoreDefinition slopStoreDefinition = new StoreDefinition(SlopStorageEngine.SLOP_STORE_NAME, null, null, null, null, null, null, null, 0, null, 0, null, 0, null, null, null, null, null, null, null, null, null, null, null, null, 0);
    SlopStorageEngine slopEngine = new SlopStorageEngine(config.getStore(slopStoreDefinition), metadata.getCluster());
    registerEngine(slopEngine, false, "slop");
    storeRepository.setSlopStore(slopEngine);
    if (voldemortConfig.isSlopPusherJobEnabled())
    {
      GregorianCalendar cal = new GregorianCalendar();
      cal.add(Calendar.SECOND, ((int) (voldemortConfig.getSlopFrequencyMs() / Time.MS_PER_SECOND)));
      Date nextRun = cal.getTime();
      logger.info(("Initializing slop pusher job type " + voldemortConfig.getPusherType() + " at " + nextRun));
      scheduler.schedule("slop", (voldemortConfig.getPusherType().compareTo(BlockingSlopPusherJob.TYPE_NAME) == 0 ? new BlockingSlopPusherJob(storeRepository, metadata, failureDetector, voldemortConfig, scanPermitWrapper) : new StreamingSlopPusherJob(storeRepository, metadata, failureDetector, voldemortConfig, scanPermitWrapper)), nextRun, voldemortConfig.getSlopFrequencyMs());
    }
    if (voldemortConfig.isRepairEnabled())
    {
      logger.info("Initializing repair job.");
      RepairJob job = new RepairJob(storeRepository, metadata, scanPermitWrapper);
      JmxUtils.registerMbean(job, JmxUtils.createObjectName(job.getClass()));
      storeRepository.registerRepairJob(job);
    }
  }
  List<StoreDefinition> storeDefs = new ArrayList<StoreDefinition>(this.metadata.getStoreDefList());
  logger.info("Initializing stores:");
  for (StoreDefinition def : storeDefs) if (!def.isView())
                                          openStore(def);
  for (StoreDefinition def : storeDefs) if (def.isView())
                                          openStore(def);
  if (voldemortConfig.isStatTrackingEnabled())
    if (this.voldemortConfig.isEnableJmxClusterName())
      JmxUtils.registerMbean(new StoreStatsJmx(this.storeStats), JmxUtils.createObjectName((metadata.getCluster().getName() + ".voldemort.store.stats.aggregate"), "aggregate-perf"));
    else
      JmxUtils.registerMbean(new StoreStatsJmx(this.storeStats), JmxUtils.createObjectName("voldemort.store.stats.aggregate", "aggregate-perf"));
  logger.info("All stores initialized.");
}