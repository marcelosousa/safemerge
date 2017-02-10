class VoldemortAdminTool{ 
 void main() {
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
  parser.accepts("nodes", "list of nodes").withRequiredArg().describedAs("nodes").withValuesSeparatedBy(',').ofType(Integer.class);
  parser.accepts("stores", "Store names").withRequiredArg().describedAs("store-names").withValuesSeparatedBy(',').ofType(String.class);
  parser.accepts("store", "Store name for querying keys").withRequiredArg().describedAs("store-name").ofType(String.class);
  parser.accepts("add-stores", "Add stores in this stores.xml").withRequiredArg().describedAs("stores.xml containing just the new stores").ofType(String.class);
  parser.accepts("delete-store", "Delete store").withRequiredArg().describedAs("store-name").ofType(String.class);
  parser.accepts("update-entries", "Insert or update entries").withRequiredArg().describedAs("input-directory").ofType(String.class);
  parser.accepts("get-metadata", ("retreive metadata information " + MetadataStore.METADATA_KEYS)).withOptionalArg().describedAs("metadata-key").ofType(String.class);
  parser.accepts("check-metadata", ("retreive metadata information from all nodes and checks if they are consistent across [ " + MetadataStore.CLUSTER_KEY + " | " + MetadataStore.STORES_KEY + " | " + MetadataStore.REBALANCING_SOURCE_CLUSTER_XML + " | " + MetadataStore.SERVER_STATE_KEY + " ]")).withRequiredArg().describedAs("metadata-key").ofType(String.class);
  parser.accepts("ro-metadata", "retrieve version information [current | max | storage-format]").withRequiredArg().describedAs("type").ofType(String.class);
  parser.accepts("truncate", "truncate a store").withRequiredArg().describedAs("store-name").ofType(String.class);
  parser.accepts("set-metadata", ("Forceful setting of metadata [ " + MetadataStore.CLUSTER_KEY + " | " + MetadataStore.STORES_KEY + " | " + MetadataStore.SERVER_STATE_KEY + " | " + MetadataStore.REBALANCING_SOURCE_CLUSTER_XML + " | " + MetadataStore.REBALANCING_STEAL_INFO + " ]")).withRequiredArg().describedAs("metadata-key").ofType(String.class);
  parser.accepts("set-metadata-value", ("The value for the set-metadata [ " + MetadataStore.CLUSTER_KEY + " | " + MetadataStore.STORES_KEY + ", " + MetadataStore.REBALANCING_SOURCE_CLUSTER_XML + ", " + MetadataStore.REBALANCING_STEAL_INFO + " ] - xml file location, [ " + MetadataStore.SERVER_STATE_KEY + " ] - " + MetadataStore.VoldemortState.NORMAL_SERVER + "," + MetadataStore.VoldemortState.REBALANCING_MASTER_SERVER)).withRequiredArg().describedAs("metadata-value").ofType(String.class);
  parser.accepts("set-metadata-pair", ("Atomic setting of metadata pair [ " + MetadataStore.CLUSTER_KEY + " & " + MetadataStore.STORES_KEY + " ]")).withRequiredArg().describedAs("metadata-keys-pair").withValuesSeparatedBy(',').ofType(String.class);
  parser.accepts("set-metadata-value-pair", ("The value for the set-metadata pair [ " + MetadataStore.CLUSTER_KEY + " & " + MetadataStore.STORES_KEY + " ]")).withRequiredArg().describedAs("metadata-value-pair").withValuesSeparatedBy(',').ofType(String.class);
  parser.accepts("clear-rebalancing-metadata", "Remove the metadata related to rebalancing");
  parser.accepts("async", "a) Get a list of async job ids [get] b) Stop async job ids [stop] ").withRequiredArg().describedAs("op-type").ofType(String.class);
  parser.accepts("async-id", "Comma separated list of async ids to stop").withOptionalArg().describedAs("job-ids").withValuesSeparatedBy(',').ofType(Integer.class);
  parser.accepts("repair-job", "Clean after rebalancing is done");
  parser.accepts("prune-job", "Prune versioned put data, after rebalancing");
  parser.accepts("purge-slops", "Purge the slop stores selectively, based on nodeId or zoneId");
  parser.accepts("native-backup", "Perform a native backup").withRequiredArg().describedAs("store-name").ofType(String.class);
  parser.accepts("backup-dir").withRequiredArg().describedAs("backup-directory").ofType(String.class);
  parser.accepts("backup-timeout").withRequiredArg().describedAs("minutes to wait for backup completion, default 30 mins").ofType(Integer.class);
  parser.accepts("backup-verify", "If provided, backup will also verify checksum (with extra overhead)");
  parser.accepts("backup-incremental", ("Perform an incremental backup for point-in-time recovery." + " By default backup has latest consistent snapshot."));
  parser.accepts("zone", "zone id").withRequiredArg().describedAs("zone-id").ofType(Integer.class);
  parser.accepts("rollback", "rollback a store").withRequiredArg().describedAs("store-name").ofType(String.class);
  parser.accepts("version", "Push version of store to rollback to").withRequiredArg().describedAs("version").ofType(Long.class);
  parser.accepts("verify-metadata-version", "Verify the version of Metadata on all the cluster nodes");
  parser.accepts("synchronize-metadata-version", "Synchronize the metadata versions across all the nodes.");
  parser.accepts("reserve-memory", "Memory in MB to reserve for the store").withRequiredArg().describedAs("size-in-mb").ofType(Long.class);
  parser.accepts("query-key", "Get values of a key on specific node").withRequiredArg().describedAs("query-key").ofType(String.class);
  parser.accepts("query-key-format", "Format of the query key. Can be one of [hex|readable]").withRequiredArg().describedAs("key-format").ofType(String.class);
  parser.accepts("show-routing-plan", "Routing plan of the specified keys").withRequiredArg().describedAs("keys-to-be-routed").withValuesSeparatedBy(',').ofType(String.class);
  parser.accepts("mirror-from-url", "Cluster url to mirror data from").withRequiredArg().describedAs("mirror-cluster-bootstrap-url").ofType(String.class);
  parser.accepts("mirror-node", "Node id in the mirror cluster to mirror from").withRequiredArg().describedAs("id-of-mirror-node").ofType(Integer.class);
  parser.accepts("fetch-orphaned", "Fetch any orphaned keys/entries in the node");
  parser.accepts("set-quota", "Enforce some quota on the servers").withRequiredArg().describedAs("quota-type").ofType(String.class);
  parser.accepts("quota-value", "Value of the quota enforced on the servers").withRequiredArg().describedAs("quota-value").ofType(String.class);
  parser.accepts("unset-quota", "Remove some quota already enforced on the servers").withRequiredArg().describedAs("quota-type").ofType(String.class);
  parser.accepts("get-quota", "Retrieve some quota already enforced on the servers").withRequiredArg().describedAs("quota-type").ofType(String.class);
  OptionSet options = parser.parse(args);
  if (options.has("help"))
  {
    printHelp(System.out, parser);
    System.exit(0);
  }
  Set<String> missing = CmdUtils.missing(options, "url", "node");
  if (missing.size() > 0)
  {
    if (!(missing.equals(ImmutableSet.of("node")) && (options.has("add-stores") || options.has("delete-store") || options.has("ro-metadata") || options.has("set-metadata") || options.has("set-metadata-pair") || options.has("get-metadata") || options.has("check-metadata")) || options.has("truncate") || options.has("clear-rebalancing-metadata") || options.has("async") || options.has("native-backup") || options.has("rollback") || options.has("verify-metadata-version") || options.has("reserve-memory") || options.has("purge-slops") || options.has("show-routing-plan") || options.has("query-key") || options.has("set-quota") || options.has("unset-quota") || options.has("get-quota")))
    {
      System.err.println(("Missing required arguments: " + Joiner.on(", ").join(missing)));
      printHelp(System.err, parser);
      System.exit(1);
    }
  }
  try
  {
    String url = (String) options.valueOf("url");
    Integer nodeId = CmdUtils.valueOf(options, "node", (-1));
    int parallelism = CmdUtils.valueOf(options, "restore", 5);
    Integer zoneId = CmdUtils.valueOf(options, "zone", (-1));
    AdminClient adminClient = new AdminClient(url, new AdminClientConfig(), new ClientConfig());
    List<String> storeNames = null;
    if (options.has("store") && options.has("stores"))
    {
      throw new VoldemortException("Must not specify both --stores and --store options");
    }
    else
      if (options.has("stores"))
      {
        storeNames = (List<String>) options.valuesOf("stores");
      }
      else
        if (options.has("store"))
        {
          storeNames = Arrays.asList(((String) options.valueOf("store")));
        }
    String outputDir = null;
    if (options.has("outdir"))
    {
      outputDir = (String) options.valueOf("outdir");
    }
    if (options.has("add-stores"))
    {
      String storesXml = (String) options.valueOf("add-stores");
      executeAddStores(adminClient, storesXml, nodeId);
    }
    else
      if (options.has("async"))
      {
        String asyncKey = (String) options.valueOf("async");
        List<Integer> asyncIds = null;
        if (options.hasArgument("async-id"))
          asyncIds = (List<Integer>) options.valuesOf("async-id");
        executeAsync(nodeId, adminClient, asyncKey, asyncIds);
      }
      else
        if (options.has("check-metadata"))
        {
          String metadataKey = (String) options.valueOf("check-metadata");
          executeCheckMetadata(adminClient, metadataKey);
        }
        else
          if (options.has("delete-partitions"))
          {
            System.out.println("Starting delete-partitions");
            List<Integer> partitionIdList = (List<Integer>) options.valuesOf("delete-partitions");
            executeDeletePartitions(nodeId, adminClient, partitionIdList, storeNames);
            System.out.println("Finished delete-partitions");
          }
          else
            if (options.has("ro-metadata"))
            {
              String type = (String) options.valueOf("ro-metadata");
              executeROMetadata(nodeId, adminClient, storeNames, type);
            }
            else
              if (options.has("reserve-memory"))
              {
                if (!options.has("stores"))
                {
                  Utils.croak("Specify the list of stores to reserve memory");
                }
                long reserveMB = (Long) options.valueOf("reserve-memory");
                adminClient.quotaMgmtOps.reserveMemory(nodeId, storeNames, reserveMB);
              }
              else
                if (options.has("get-metadata"))
                {
                  String metadataKey = ALL_METADATA;
                  if (options.hasArgument("get-metadata"))
                  {
                    metadataKey = (String) options.valueOf("get-metadata");
                  }
                  executeGetMetadata(nodeId, adminClient, metadataKey, outputDir);
                }
                else
                  if (options.has("mirror-from-url"))
                  {
                    if (!options.has("mirror-node"))
                    {
                      Utils.croak("Specify the mirror node to fetch from");
                    }
                    if (nodeId == -1)
                    {
                      System.err.println("Cannot run mirroring without node id");
                      System.exit(1);
                    }
                    Integer mirrorNodeId = CmdUtils.valueOf(options, "mirror-node", (-1));
                    if (mirrorNodeId == -1)
                    {
                      System.err.println("Cannot run mirroring without mirror node id");
                      System.exit(1);
                    }
                    adminClient.restoreOps.mirrorData(nodeId, mirrorNodeId, ((String) options.valueOf("mirror-from-url")), storeNames);
                  }
                  else
                    if (options.has("clear-rebalancing-metadata"))
                    {
                      executeClearRebalancing(nodeId, adminClient);
                    }
                    else
                      if (options.has("prune-job"))
                      {
                        if (storeNames == null)
                        {
                          Utils.croak("Must specify --stores to run the prune job");
                        }
                        executePruneJob(nodeId, adminClient, storeNames);
                      }
                      else
                        if (options.has("fetch-keys"))
                        {
                          boolean useAscii = options.has("ascii");
                          System.out.println("Starting fetch keys");
                          List<Integer> partitionIdList = null;
                          if (options.hasArgument("fetch-keys"))
                            partitionIdList = (List<Integer>) options.valuesOf("fetch-keys");
                          executeFetchKeys(nodeId, adminClient, partitionIdList, outputDir, storeNames, useAscii, options.has("fetch-orphaned"));
                        }
                        else
                          if (options.has("repair-job"))
                          {
                            executeRepairJob(nodeId, adminClient);
                          }
                          else
                            if (options.has("set-metadata-pair"))
                            {
                              List<String> metadataKeyPair = (List<String>) options.valuesOf("set-metadata-pair");
                              if (metadataKeyPair.size() != 2)
                              {
                                throw new VoldemortException("Missing set-metadata-pair keys (only two keys are needed and allowed)");
                              }
                              if (!options.has("set-metadata-value-pair"))
                              {
                                throw new VoldemortException("Missing set-metadata-value-pair");
                              }
                              else
                              {
                                List<String> metadataValuePair = (List<String>) options.valuesOf("set-metadata-value-pair");
                                if (metadataValuePair.size() != 2)
                                {
                                  throw new VoldemortException("Missing set-metadata--value-pair values (only two values are needed and allowed)");
                                }
                                if (metadataKeyPair.contains(MetadataStore.CLUSTER_KEY) && metadataKeyPair.contains(MetadataStore.STORES_KEY))
                                {
                                  ClusterMapper clusterMapper = new ClusterMapper();
                                  StoreDefinitionsMapper storeDefsMapper = new StoreDefinitionsMapper();
                                  Integer nodeIdToGetStoreXMLFrom = nodeId;
                                  if (nodeId < 0)
                                  {
                                    Collection<Node> nodes = adminClient.getAdminClientCluster().getNodes();
                                    if (nodes.isEmpty())
                                    {
                                      throw new VoldemortException("No nodes in this cluster");
                                    }
                                    else
                                    {
                                      nodeIdToGetStoreXMLFrom = nodes.iterator().next().getId();
                                    }
                                  }
                                  Versioned<String> storesXML = adminClient.metadataMgmtOps.getRemoteMetadata(nodeIdToGetStoreXMLFrom, MetadataStore.STORES_KEY);
                                  List<StoreDefinition> oldStoreDefs = storeDefsMapper.readStoreList(new StringReader(storesXML.getValue()));
                                  String clusterXMLPath = metadataValuePair.get(metadataKeyPair.indexOf(MetadataStore.CLUSTER_KEY));
                                  clusterXMLPath = clusterXMLPath.replace("~", System.getProperty("user.home"));
                                  if (!Utils.isReadableFile(clusterXMLPath))
                                    throw new VoldemortException("Cluster xml file path incorrect");
                                  Cluster cluster = clusterMapper.readCluster(new File(clusterXMLPath));
                                  String storesXMLPath = metadataValuePair.get(metadataKeyPair.indexOf(MetadataStore.STORES_KEY));
                                  storesXMLPath = storesXMLPath.replace("~", System.getProperty("user.home"));
                                  if (!Utils.isReadableFile(storesXMLPath))
                                    throw new VoldemortException("Stores definition xml file path incorrect");
                                  List<StoreDefinition> newStoreDefs = storeDefsMapper.readStoreList(new File(storesXMLPath));
                                  checkSchemaCompatibility(newStoreDefs);
                                  executeSetMetadataPair(nodeId, adminClient, MetadataStore.CLUSTER_KEY, clusterMapper.writeCluster(cluster), MetadataStore.STORES_KEY, storeDefsMapper.writeStoreList(newStoreDefs));
                                  executeUpdateMetadataVersionsOnStores(adminClient, oldStoreDefs, newStoreDefs);
                                }
                                else
                                {
                                  throw new VoldemortException("set-metadata-pair keys should be <cluster.xml, stores.xml>");
                                }
                              }
                            }
                            else
                              if (options.has("set-metadata"))
                              {
                                String metadataKey = (String) options.valueOf("set-metadata");
                                if (!options.has("set-metadata-value"))
                                {
                                  throw new VoldemortException("Missing set-metadata-value");
                                }
                                else
                                {
                                  String metadataValue = (String) options.valueOf("set-metadata-value");
                                  if ((metadataKey.compareTo(MetadataStore.CLUSTER_KEY) == 0 || metadataKey.compareTo(MetadataStore.REBALANCING_SOURCE_CLUSTER_XML)) == 0)
                                  {
                                    if (!Utils.isReadableFile(metadataValue))
                                      throw new VoldemortException("Cluster xml file path incorrect");
                                    ClusterMapper mapper = new ClusterMapper();
                                    Cluster newCluster = mapper.readCluster(new File(metadataValue));
                                    executeSetMetadata(nodeId, adminClient, metadataKey, mapper.writeCluster(newCluster));
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
                                        List<StoreDefinition> newStoreDefs = mapper.readStoreList(new File(metadataValue));
                                        checkSchemaCompatibility(newStoreDefs);
                                        Integer nodeIdToGetStoreXMLFrom = nodeId;
                                        if (nodeId < 0)
                                        {
                                          Collection<Node> nodes = adminClient.getAdminClientCluster().getNodes();
                                          if (nodes.isEmpty())
                                          {
                                            throw new VoldemortException("No nodes in this cluster");
                                          }
                                          else
                                          {
                                            nodeIdToGetStoreXMLFrom = nodes.iterator().next().getId();
                                          }
                                        }
                                        Versioned<String> storesXML = adminClient.metadataMgmtOps.getRemoteMetadata(nodeIdToGetStoreXMLFrom, MetadataStore.STORES_KEY);
                                        List<StoreDefinition> oldStoreDefs = mapper.readStoreList(new StringReader(storesXML.getValue()));
                                        executeSetMetadata(nodeId, adminClient, MetadataStore.STORES_KEY, mapper.writeStoreList(newStoreDefs));
                                        if (nodeId >= 0)
                                        {
                                          System.err.println(("WARNING: Metadata version update of stores goes to all servers, " + "although this set-metadata oprations only goes to node " + nodeId));
                                        }
                                        executeUpdateMetadataVersionsOnStores(adminClient, oldStoreDefs, newStoreDefs);
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
                              else
                                if (options.has("native-backup"))
                                {
                                  if (!options.has("backup-dir"))
                                  {
                                    Utils.croak("A backup directory must be specified with backup-dir option");
                                  }
                                  String backupDir = (String) options.valueOf("backup-dir");
                                  String storeName = (String) options.valueOf("native-backup");
                                  int timeout = CmdUtils.valueOf(options, "backup-timeout", 30);
                                  adminClient.storeMntOps.nativeBackup(nodeId, storeName, backupDir, timeout, options.has("backup-verify"), options.has("backup-incremental"));
                                }
                                else
                                  if (options.has("rollback"))
                                  {
                                    if (!options.has("version"))
                                    {
                                      Utils.croak("A read-only push version must be specified with rollback option");
                                    }
                                    String storeName = (String) options.valueOf("rollback");
                                    long pushVersion = (Long) options.valueOf("version");
                                    executeRollback(nodeId, storeName, pushVersion, adminClient);
                                  }
                                  else
                                    if (options.has("query-key"))
                                    {
                                      String key = (String) options.valueOf("query-key");
                                      String keyFormat = (String) options.valueOf("query-key-format");
                                      if (keyFormat == null)
                                      {
                                        keyFormat = "hex";
                                      }
                                      if (!keyFormat.equals("hex") && !keyFormat.equals("readable"))
                                      {
                                        throw new VoldemortException("--query-key-format must be hex or readable");
                                      }
                                      executeQueryKey(nodeId, adminClient, storeNames, key, keyFormat);
                                    }
                                    else
                                      if (options.has("restore"))
                                      {
                                        if (nodeId == -1)
                                        {
                                          System.err.println("Cannot run restore without node id");
                                          System.exit(1);
                                        }
                                        System.out.println("Starting restore");
                                        adminClient.restoreOps.restoreDataFromReplications(nodeId, parallelism, zoneId);
                                        System.out.println("Finished restore");
                                      }
                                      else
                                        if (options.has("delete-store"))
                                        {
                                          String storeName = (String) options.valueOf("delete-store");
                                          executeDeleteStore(adminClient, storeName, nodeId);
                                        }
                                        else
                                          if (options.has("truncate"))
                                          {
                                            String storeName = (String) options.valueOf("truncate");
                                            executeTruncateStore(nodeId, adminClient, storeName);
                                          }
                                          else
                                            if (options.has("update-entries"))
                                            {
                                              String inputDir = (String) options.valueOf("update-entries");
                                              executeUpdateEntries(nodeId, adminClient, storeNames, inputDir);
                                            }
                                            else
                                              if (options.has("fetch-entries"))
                                              {
                                                boolean useAscii = options.has("ascii");
                                                System.out.println("Starting fetch entries");
                                                List<Integer> partitionIdList = null;
                                                if (options.hasArgument("fetch-entries"))
                                                  partitionIdList = (List<Integer>) options.valuesOf("fetch-entries");
                                                executeFetchEntries(nodeId, adminClient, partitionIdList, outputDir, storeNames, useAscii, options.has("fetch-orphaned"));
                                              }
                                              else
                                                if (options.has("purge-slops"))
                                                {
                                                  List<Integer> nodesToPurge = null;
                                                  if (options.has("nodes"))
                                                  {
                                                    nodesToPurge = (List<Integer>) options.valuesOf("nodes");
                                                  }
                                                  if (((nodesToPurge == null && zoneId) == -1 && storeNames) == null)
                                                  {
                                                    Utils.croak("Must specify atleast one of --nodes, --zone-id or --stores with --purge-slops");
                                                  }
                                                  executePurgeSlops(adminClient, nodesToPurge, zoneId, storeNames);
                                                }
                                                else
                                                  if (options.has("synchronize-metadata-version"))
                                                  {
                                                    synchronizeMetadataVersion(adminClient, nodeId);
                                                  }
                                                  else
                                                    if (options.has("verify-metadata-version"))
                                                    {
                                                      checkMetadataVersion(adminClient);
                                                    }
                                                    else
                                                      if (options.has("show-routing-plan"))
                                                      {
                                                        if (!options.has("store"))
                                                        {
                                                          Utils.croak("Must specify the store the keys belong to using --store ");
                                                        }
                                                        String storeName = (String) options.valueOf("store");
                                                        List<String> keysToRoute = (List<String>) options.valuesOf("show-routing-plan");
                                                        if ((keysToRoute == null || keysToRoute.size()) == 0)
                                                        {
                                                          Utils.croak("Must specify comma separated keys list in hex format");
                                                        }
                                                        executeShowRoutingPlan(adminClient, storeName, keysToRoute);
                                                      }
                                                      else
                                                        if (options.has("set-quota"))
                                                        {
                                                          String quotaType = (String) options.valueOf("set-quota");
                                                          Set<String> validQuotaTypes = QuotaUtils.validQuotaTypes();
                                                          if (!validQuotaTypes.contains(quotaType))
                                                          {
                                                            Utils.croak(("Specify a valid quota type from :" + validQuotaTypes));
                                                          }
                                                          if (!options.has("store"))
                                                          {
                                                            Utils.croak("Must specify the store to enforce the quota on. ");
                                                          }
                                                          if (!options.has("quota-value"))
                                                          {
                                                            Utils.croak("Must specify the value of the quota being set");
                                                          }
                                                          String storeName = (String) options.valueOf("store");
                                                          String quotaValue = (String) options.valueOf("quota-value");
                                                          executeSetQuota(adminClient, storeName, quotaType, quotaValue);
                                                        }
                                                        else
                                                          if (options.has("unset-quota"))
                                                          {
                                                            String quotaType = (String) options.valueOf("unset-quota");
                                                            Set<String> validQuotaTypes = QuotaUtils.validQuotaTypes();
                                                            if (!validQuotaTypes.contains(quotaType))
                                                            {
                                                              Utils.croak(("Specify a valid quota type from :" + validQuotaTypes));
                                                            }
                                                            if (!options.has("store"))
                                                            {
                                                              Utils.croak("Must specify the store to enforce the quota on. ");
                                                            }
                                                            String storeName = (String) options.valueOf("store");
                                                            executeUnsetQuota(adminClient, storeName, quotaType);
                                                          }
                                                          else
                                                            if (options.has("get-quota"))
                                                            {
                                                              String quotaType = (String) options.valueOf("get-quota");
                                                              Set<String> validQuotaTypes = QuotaUtils.validQuotaTypes();
                                                              if (!validQuotaTypes.contains(quotaType))
                                                              {
                                                                Utils.croak(("Specify a valid quota type from :" + validQuotaTypes));
                                                              }
                                                              if (!options.has("store"))
                                                              {
                                                                Utils.croak("Must specify the store to enforce the quota on. ");
                                                              }
                                                              String storeName = (String) options.valueOf("store");
                                                              executeGetQuota(adminClient, storeName, quotaType);
                                                            }
                                                            else
                                                            {
                                                              Utils.croak(("At least one of (delete-partitions, restore, add-node, fetch-entries, " + "fetch-keys, add-stores, delete-store, update-entries, get-metadata, ro-metadata, " + "set-metadata, check-metadata, clear-rebalancing-metadata, async, " + "repair-job, native-backup, rollback, reserve-memory, mirror-url," + " verify-metadata-version, prune-job, purge-slops) must be specified"));
                                                            }
  }
  catch (Exception e)
  {
    e.printStackTrace();
    Utils.croak(e.getMessage());
  }
}
}