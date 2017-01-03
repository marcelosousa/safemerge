{
  OptionParser parser = new OptionParser();
  parser.accepts("help", "print help information");
  parser.accepts("url", "[REQUIRED] bootstrap URL").withRequiredArg().describedAs("bootstrap-url").ofType(String.class);
  parser.accepts("node", "node id").withRequiredArg().describedAs("node-id").ofType(Integer.class);
  parser.accepts("delete-partitions", "Delete partitions").withRequiredArg().describedAs("partition-ids").withValuesSeparatedBy(',').ofType(Integer.class);
  parser.accepts("restore", "Restore from replication");
  parser.accepts("ascii", "Fetch keys as ASCII");
  parser.accepts("parallelism", "Parallelism").withRequiredArg().describedAs("parallelism").ofType(Integer.class);
  parser.accepts("fetch-keys", "Fetch keys").withRequiredArg().describedAs("partition-ids").withValuesSeparatedBy(',').ofType(Integer.class);
  parser.accepts("fetch-entries", "Fetch full entries").withRequiredArg().describedAs("partition-ids").withValuesSeparatedBy(',').ofType(Integer.class);
  parser.accepts("outdir", "Output directory").withRequiredArg().describedAs("output-directory").ofType(String.class);
  parser.accepts("stores", "Store names").withRequiredArg().describedAs("store-names").withValuesSeparatedBy(',').ofType(String.class);
  parser.accepts("add-stores", "Add stores in this stores.xml").withRequiredArg().describedAs("stores.xml").ofType(String.class);
  parser.accepts("delete-store", "Delete store").withRequiredArg().describedAs("store-name").ofType(String.class);
  parser.accepts("update-entries", "[EXPERIMENTAL] Insert or update entries").withRequiredArg().describedAs("input-directory").ofType(String.class);
  parser.accepts("get-metadata", ("retreive metadata information [ " + MetadataStore.CLUSTER_KEY + " | " + MetadataStore.STORES_KEY + " | " + MetadataStore.SERVER_STATE_KEY + " ]")).withRequiredArg().describedAs("metadata-key").ofType(String.class);
  parser.accepts("ro-version", "retrieve version information [current | max]").withRequiredArg().describedAs("version-type").ofType(String.class);
  parser.accepts("truncate", "truncate a store").withRequiredArg().describedAs("store-name").ofType(String.class);
  parser.accepts("set-metadata", ("Forceful setting of metadata [ " + MetadataStore.CLUSTER_KEY + " | " + MetadataStore.STORES_KEY + " | " + MetadataStore.SERVER_STATE_KEY + " ], possibly after grandfathering or partial rebalancing")).withRequiredArg().describedAs("metadata-key").ofType(String.class);
  parser.accepts("set-metadata-value", ("The value for the set-metadata [ " + MetadataStore.CLUSTER_KEY + " | " + MetadataStore.STORES_KEY + " ] - xml file location, [ " + MetadataStore.SERVER_STATE_KEY + " ] - " + MetadataStore.VoldemortState.NORMAL_SERVER + "," + MetadataStore.VoldemortState.REBALANCING_MASTER_SERVER + "," + MetadataStore.VoldemortState.GRANDFATHERING_SERVER)).withRequiredArg().describedAs("metadata-value").ofType(String.class);
  OptionSet options = parser.parse(args);
  if (options.has("help"))
  {
    parser.printHelpOn(System.out);
    System.exit(0);
  }
  Set<String> missing = CmdUtils.missing(options, "url", "node");
  if (missing.size() > 0)
  {
    if (!(missing.equals(ImmutableSet.of("node")) && (options.has("add-stores") || options.has("delete-store") || options.has("ro-version") || options.has("set-metadata") || options.has("get-metadata"))))
    {
      System.err.println(("Missing required arguments: " + Joiner.on(", ").join(missing)));
      parser.printHelpOn(System.err);
      System.exit(1);
    }
  }
  String url = (String) options.valueOf("url");
  Integer nodeId = CmdUtils.valueOf(options, "node", (-1));
  Integer parallelism = CmdUtils.valueOf(options, "parallelism", 5);
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
  if (options.has("ro-version"))
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
  if (ops.length() < 1)
  {
    Utils.croak("At least one of (delete-partitions, restore, add-node, fetch-entries, fetch-keys, add-stores, delete-store, update-entries, get-metadata, ro-version, set-metadata) must be specified");
  }
  List<String> storeNames = null;
  if (options.has("stores"))
  {
    @SuppressWarnings("unchecked")
    List<String> temp = (List<String>) options.valuesOf("stores");
    storeNames = temp;
  }
  try
  {
    if (ops.contains("d"))
    {
      System.out.println("Starting delete-partitions");
      @SuppressWarnings("unchecked")
      List<Integer> partitionIdList = (List<Integer>) options.valuesOf("delete-partitions");
      executeDeletePartitions(nodeId, adminClient, partitionIdList, storeNames);
      System.out.println("Finished delete-partitions");
    }
    if (ops.contains("r"))
    {
      System.out.println("Starting restore");
      adminClient.restoreDataFromReplications(nodeId, parallelism);
      System.err.println("Finished restore");
    }
    if (ops.contains("k"))
    {
      String outputDir = null;
      if (options.has("outdir"))
      {
        outputDir = (String) options.valueOf("outdir");
      }
      boolean useAscii = options.has("ascii");
      System.out.println("Starting fetch keys");
      @SuppressWarnings("unchecked")
      List<Integer> partitionIdList = (List<Integer>) options.valuesOf("fetch-keys");
      executeFetchKeys(nodeId, adminClient, partitionIdList, outputDir, storeNames, useAscii);
    }
    if (ops.contains("v"))
    {
      String outputDir = null;
      if (options.has("outdir"))
      {
        outputDir = (String) options.valueOf("outdir");
      }
      boolean useAscii = options.has("ascii");
      @SuppressWarnings("unchecked")
      List<Integer> partitionIdList = (List<Integer>) options.valuesOf("fetch-entries");
      executeFetchEntries(nodeId, adminClient, partitionIdList, outputDir, storeNames, useAscii);
    }
    if (ops.contains("a"))
    {
      String storesXml = (String) options.valueOf("add-stores");
      executeAddStores(adminClient, storesXml, storeNames, nodeId);
    }
    if (ops.contains("u"))
    {
      String inputDir = (String) options.valueOf("update-entries");
      boolean useAscii = options.has("ascii");
      executeUpdateEntries(nodeId, adminClient, storeNames, inputDir, useAscii);
    }
    if (ops.contains("s"))
    {
      String storeName = (String) options.valueOf("delete-store");
      executeDeleteStore(adminClient, storeName);
    }
    if (ops.contains("g"))
    {
      String metadataKey = (String) options.valueOf("get-metadata");
      executeGetMetadata(nodeId, adminClient, metadataKey);
    }
    if (ops.contains("e"))
    {
      String versionType = (String) options.valueOf("ro-version");
      executeROVersion(nodeId, adminClient, storeNames, versionType);
    }
    if (ops.contains("t"))
    {
      String storeName = (String) options.valueOf("truncate");
      executeTruncateStore(nodeId, adminClient, storeName);
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
            {
              throw new VoldemortException("Incorrect metadata key");
            }
      }
    }
  }
  catch (Exception e)
  {
    e.printStackTrace();
    Utils.croak(e.getMessage());
  }
}