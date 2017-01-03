{
  OptionParser parser = new OptionParser();
  parser.accepts("help", "print help information");
  parser.accepts("url", "[REQUIRED] bootstrap URL").withRequiredArg().describedAs("bootstrap-url").ofType(String.class);
  parser.accepts("node", "node id").withRequiredArg().describedAs("node-id").ofType(Integer.class);
  parser.accepts("delete-partitions", "Delete partitions").withRequiredArg().describedAs("partition-ids").withValuesSeparatedBy(',').ofType(Integer.class);
  parser.accepts("restore", "Restore from replication [ Optional parallelism param - Default - 5 ]").withOptionalArg().describedAs("parallelism").ofType(Integer.class);
  parser.accepts("ascii", "Fetch keys as ASCII");
  parser.accepts("fetch-keys", "Fetch keys").withOptionalArg().describedAs("partition-ids").withValuesSeparatedBy(',').ofType(Integer.class);
  parser.accepts("fetch-entries", "Fetch full entries").withOptionalArg().describedAs("partition-ids").withValuesSeparatedBy(',').ofType(Integer.class);
  parser.accepts("outdir", "Output directory").withRequiredArg().describedAs("output-directory").ofType(String.class);
  parser.accepts("stores", "Store names").withRequiredArg().describedAs("store-names").withValuesSeparatedBy(',').ofType(String.class);
  parser.accepts("store", "Store name for querying keys").withRequiredArg().describedAs("store-name").ofType(String.class);
  parser.accepts("add-stores", "Add stores in this stores.xml").withRequiredArg().describedAs("stores.xml containing just the new stores").ofType(String.class);
  parser.accepts("delete-store", "Delete store").withRequiredArg().describedAs("store-name").ofType(String.class);
  parser.accepts("update-entries", "Insert or update entries").withRequiredArg().describedAs("input-directory").ofType(String.class);
  parser.accepts("get-metadata", ("retreive metadata information " + MetadataStore.METADATA_KEYS)).withOptionalArg().describedAs("metadata-key").ofType(String.class);
  parser.accepts("check-metadata", ("retreive metadata information from all nodes and checks if they are consistent across [ " + MetadataStore.CLUSTER_KEY + " | " + MetadataStore.STORES_KEY + " | " + MetadataStore.SERVER_STATE_KEY + " ]")).withRequiredArg().describedAs("metadata-key").ofType(String.class);
  parser.accepts("ro-metadata", "retrieve version information [current | max | storage-format]").withRequiredArg().describedAs("type").ofType(String.class);
  parser.accepts("truncate", "truncate a store").withRequiredArg().describedAs("store-name").ofType(String.class);
  parser.accepts("set-metadata", ("Forceful setting of metadata [ " + MetadataStore.CLUSTER_KEY + " | " + MetadataStore.STORES_KEY + " | " + MetadataStore.SERVER_STATE_KEY + " | " + MetadataStore.REBALANCING_STEAL_INFO + " ]")).withRequiredArg().describedAs("metadata-key").ofType(String.class);
  parser.accepts("set-metadata-value", ("The value for the set-metadata [ " + MetadataStore.CLUSTER_KEY + " | " + MetadataStore.STORES_KEY + ", " + MetadataStore.REBALANCING_STEAL_INFO + " ] - xml file location, [ " + MetadataStore.SERVER_STATE_KEY + " ] - " + MetadataStore.VoldemortState.NORMAL_SERVER + "," + MetadataStore.VoldemortState.REBALANCING_MASTER_SERVER)).withRequiredArg().describedAs("metadata-value").ofType(String.class);
  parser.accepts("key-distribution", "Prints the current key distribution of the cluster");
  parser.accepts("clear-rebalancing-metadata", "Remove the metadata related to rebalancing");
  parser.accepts("async", "a) Get a list of async job ids [get] b) Stop async job ids [stop] ").withRequiredArg().describedAs("op-type").ofType(String.class);
  parser.accepts("async-id", "Comma separated list of async ids to stop").withOptionalArg().describedAs("job-ids").withValuesSeparatedBy(',').ofType(Integer.class);
  parser.accepts("repair-job", "Clean after rebalancing is done");
  parser.accepts("native-backup", "Perform a native backup").withRequiredArg().describedAs("store-name").ofType(String.class);
  parser.accepts("backup-dir").withRequiredArg().describedAs("backup-directory").ofType(String.class);
  parser.accepts("backup-timeout").withRequiredArg().describedAs("minutes to wait for backup completion, default 30 mins").ofType(Integer.class);
  parser.accepts("backup-verify", "If provided, backup will also verify checksum (with extra overhead)");
  parser.accepts("backup-incremental", ("Perform an incremental backup for point-in-time recovery." + " By default backup has latest consistent snapshot."));
  parser.accepts("zone", "zone id").withRequiredArg().describedAs("zone-id").ofType(Integer.class);
  parser.accepts("rollback", "rollback a store").withRequiredArg().describedAs("store-name").ofType(String.class);
  parser.accepts("version", "Push version of store to rollback to").withRequiredArg().describedAs("version").ofType(Long.class);
  parser.accepts("reserve-memory", "Memory in MB to reserve for the store").withRequiredArg().describedAs("size-in-mb").ofType(Long.class);
  parser.accepts("query-keys", "Get values of keys on specific nodes").withRequiredArg().describedAs("query-keys").withValuesSeparatedBy(',').ofType(String.class);
  OptionSet options = parser.parse(args);
  if (options.has("help"))
  {
    printHelp(System.out, parser);
    System.exit(0);
  }
  Set<String> missing = CmdUtils.missing(options, "url", "node");
  if (missing.size() > 0)
  {
    if (!(missing.equals(ImmutableSet.of("node")) && (options.has("add-stores") || options.has("delete-store") || options.has("ro-metadata") || options.has("set-metadata") || options.has("get-metadata") || options.has("check-metadata") || options.has("key-distribution")) || options.has("truncate") || options.has("clear-rebalancing-metadata") || options.has("async") || options.has("native-backup") || options.has("rollback") || options.has("reserve-memory")))
    {
      System.err.println(("Missing required arguments: " + Joiner.on(", ").join(missing)));
      printHelp(System.err, parser);
      System.exit(1);
    }
  }
  String url = (String) options.valueOf("url");
  Integer nodeId = CmdUtils.valueOf(options, "node", (-1));
  int parallelism = CmdUtils.valueOf(options, "restore", 5);
  Integer zoneId = CmdUtils.valueOf(options, "zone", (-1));
  AdminClient adminClient = new AdminClient(url, new AdminClientConfig());
  String ops = "";
  if (options.has("delete-partitions"))
  {
    ops += "d";
  }
  if (options.has("fetch-keys"))
  {
    ops += "k";
  }
  if (options.has("fetch-entries"))
  {
    ops += "v";
  }
  if (options.has("restore"))
  {
    ops += "r";
  }
  if (options.has("add-stores"))
  {
    ops += "a";
  }
  if (options.has("update-entries"))
  {
    ops += "u";
  }
  if (options.has("delete-store"))
  {
    ops += "s";
  }
  if (options.has("get-metadata"))
  {
    ops += "g";
  }
  if (options.has("ro-metadata"))
  {
    ops += "e";
  }
  if (options.has("truncate"))
  {
    ops += "t";
  }
  if (options.has("set-metadata"))
  {
    ops += "m";
  }
  if (options.has("check-metadata"))
  {
    ops += "c";
  }
  if (options.has("key-distribution"))
  {
    ops += "y";
  }
  if (options.has("clear-rebalancing-metadata"))
  {
    ops += "i";
  }
  if (options.has("async"))
  {
    ops += "b";
  }
  if (options.has("repair-job"))
  {
    ops += "l";
  }
  if (options.has("native-backup"))
  {
    if (!options.has("backup-dir"))
    {
      Utils.croak("A backup directory must be specified with backup-dir option");
    }
    ops += "n";
  }
  if (options.has("rollback"))
  {
    if (!options.has("version"))
    {
      Utils.croak("A read-only push version must be specified with rollback option");
    }
    ops += "o";
  }
  if (options.has("reserve-memory"))
  {
    if (!options.has("stores"))
    {
      Utils.croak("Specify the list of stores to reserve memory");
    }
    ops += "f";
  }
  if (options.has("query-keys"))
  {
    ops += "q";
  }
  if (ops.length() < 1)
  {
    Utils.croak(("At least one of (delete-partitions, restore, add-node, fetch-entries, " + "fetch-keys, add-stores, delete-store, update-entries, get-metadata, ro-metadata, " + "set-metadata, check-metadata, key-distribution, clear-rebalancing-metadata, async, " + "repair-job, native-backup, rollback, reserve-memory) must be specified"));
  }
  List<String> storeNames = null;
  if (options.has("stores"))
  {
    List<String> temp = (List<String>) options.valuesOf("stores");
    storeNames = temp;
  }
  String outputDir = null;
  if (options.has("outdir"))
  {
    outputDir = (String) options.valueOf("outdir");
  }
  try
  {
    if (ops.contains("d"))
    {
      System.out.println("Starting delete-partitions");
      List<Integer> partitionIdList = (List<Integer>) options.valuesOf("delete-partitions");
      executeDeletePartitions(nodeId, adminClient, partitionIdList, storeNames);
      System.out.println("Finished delete-partitions");
    }
    if (ops.contains("r"))
    {
      if (nodeId == -1)
      {
        System.err.println("Cannot run restore without node id");
        System.exit(1);
      }
      System.out.println("Starting restore");
      adminClient.restoreDataFromReplications(nodeId, parallelism, zoneId);
      System.out.println("Finished restore");
    }
    if (ops.contains("k"))
    {
      boolean useAscii = options.has("ascii");
      System.out.println("Starting fetch keys");
      List<Integer> partitionIdList = null;
      if (options.hasArgument("fetch-keys"))
        partitionIdList = (List<Integer>) options.valuesOf("fetch-keys");
      executeFetchKeys(nodeId, adminClient, partitionIdList, outputDir, storeNames, useAscii);
    }
    if (ops.contains("v"))
    {
      boolean useAscii = options.has("ascii");
      System.out.println("Starting fetch entries");
      List<Integer> partitionIdList = null;
      if (options.hasArgument("fetch-entries"))
        partitionIdList = (List<Integer>) options.valuesOf("fetch-entries");
      executeFetchEntries(nodeId, adminClient, partitionIdList, outputDir, storeNames, useAscii);
    }
    if (ops.contains("a"))
    {
      String storesXml = (String) options.valueOf("add-stores");
      executeAddStores(adminClient, storesXml, nodeId);
    }
    if (ops.contains("u"))
    {
      String inputDir = (String) options.valueOf("update-entries");
      executeUpdateEntries(nodeId, adminClient, storeNames, inputDir);
    }
    if (ops.contains("s"))
    {
      String storeName = (String) options.valueOf("delete-store");
      executeDeleteStore(adminClient, storeName, nodeId);
    }
    if (ops.contains("g"))
    {
      String metadataKey = ALL_METADATA;
      if (options.hasArgument("get-metadata"))
      {
        metadataKey = (String) options.valueOf("get-metadata");
      }
      executeGetMetadata(nodeId, adminClient, metadataKey, outputDir);
    }
    if (ops.contains("e"))
    {
      String type = (String) options.valueOf("ro-metadata");
      executeROMetadata(nodeId, adminClient, storeNames, type);
    }
    if (ops.contains("t"))
    {
      String storeName = (String) options.valueOf("truncate");
      executeTruncateStore(nodeId, adminClient, storeName);
    }
    if (ops.contains("c"))
    {
      String metadataKey = (String) options.valueOf("check-metadata");
      executeCheckMetadata(adminClient, metadataKey);
    }
    if (ops.contains("m"))
    {
      String metadataKey = (String) options.valueOf("set-metadata");
      if (!options.has("set-metadata-value"))
      {
        throw new VoldemortException("Missing set-metadata-value");
      }
      else
      {
        String metadataValue = (String) options.valueOf("set-metadata-value");
        if (metadataKey.compareTo(MetadataStore.CLUSTER_KEY) == 0)
        {
          if (!Utils.isReadableFile(metadataValue))
            throw new VoldemortException("Cluster xml file path incorrect");
          ClusterMapper mapper = new ClusterMapper();
          Cluster newCluster = mapper.readCluster(new File(metadataValue));
          executeSetMetadata(nodeId, adminClient, MetadataStore.CLUSTER_KEY, mapper.writeCluster(newCluster));
        }
        else
          if (metadataKey.compareTo(MetadataStore.SERVER_STATE_KEY) == 0)
          {
            VoldemortState newState = VoldemortState.valueOf(metadataValue);
            executeSetMetadata(nodeId, adminClient, MetadataStore.SERVER_STATE_KEY, newState.toString());
          }
          else
            if (metadataKey.compareTo(MetadataStore.STORES_KEY) == 0)
            {
              if (!Utils.isReadableFile(metadataValue))
                throw new VoldemortException("Stores definition xml file path incorrect");
              StoreDefinitionsMapper mapper = new StoreDefinitionsMapper();
              List<StoreDefinition> storeDefs = mapper.readStoreList(new File(metadataValue));
              executeSetMetadata(nodeId, adminClient, MetadataStore.STORES_KEY, mapper.writeStoreList(storeDefs));
            }
            else
              if (metadataKey.compareTo(MetadataStore.REBALANCING_STEAL_INFO) == 0)
              {
                if (!Utils.isReadableFile(metadataValue))
                  throw new VoldemortException("Rebalancing steal info file path incorrect");
                String rebalancingStealInfoJsonString = FileUtils.readFileToString(new File(metadataValue));
                RebalancerState state = RebalancerState.create(rebalancingStealInfoJsonString);
                executeSetMetadata(nodeId, adminClient, MetadataStore.REBALANCING_STEAL_INFO, state.toJsonString());
              }
              else
              {
                throw new VoldemortException("Incorrect metadata key");
              }
      }
    }
    if (ops.contains("y"))
    {
      executeKeyDistribution(adminClient);
    }
    if (ops.contains("i"))
    {
      executeClearRebalancing(nodeId, adminClient);
    }
    if (ops.contains("b"))
    {
      String asyncKey = (String) options.valueOf("async");
      List<Integer> asyncIds = null;
      if (options.hasArgument("async-id"))
        asyncIds = (List<Integer>) options.valuesOf("async-id");
      executeAsync(nodeId, adminClient, asyncKey, asyncIds);
    }
    if (ops.contains("l"))
    {
      executeRepairJob(nodeId, adminClient);
    }
    if (ops.contains("n"))
    {
      String backupDir = (String) options.valueOf("backup-dir");
      String storeName = (String) options.valueOf("native-backup");
      int timeout = CmdUtils.valueOf(options, "backup-timeout", 30);
      adminClient.nativeBackup(nodeId, storeName, backupDir, timeout, options.has("backup-verify"), options.has("backup-incremental"));
    }
    if (ops.contains("o"))
    {
      String storeName = (String) options.valueOf("rollback");
      long pushVersion = (Long) options.valueOf("version");
      executeRollback(nodeId, storeName, pushVersion, adminClient);
    }
    if (ops.contains("f"))
    {
      long reserveMB = (Long) options.valueOf("reserve-memory");
      adminClient.reserveMemory(nodeId, storeNames, reserveMB);
    }
    if (ops.contains("q"))
    {
      List<String> keyList = (List<String>) options.valuesOf("query-keys");
      if ((storeNames == null || storeNames.size()) == 0)
      {
        throw new VoldemortException("Must specify store name using --stores option");
      }
      executeQueryKeys(nodeId, adminClient, storeNames, keyList);
    }
  }
  catch (Exception e)
  {
    e.printStackTrace();
    Utils.croak(e.getMessage());
  }
}