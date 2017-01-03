{
  int exitCode = ERROR_EXIT_CODE;
  RebalanceController rebalanceController = null;
  try
  {
    OptionParser parser = new OptionParser();
    parser.accepts("help", "Print usage information");
    parser.accepts("current-cluster", "Path to current cluster xml").withRequiredArg().describedAs("cluster.xml");
    parser.accepts("target-cluster", "Path to target cluster xml").withRequiredArg().describedAs("cluster.xml");
    parser.accepts("current-stores", "Path to store definition xml").withRequiredArg().describedAs("stores.xml");
    parser.accepts("url", "Url to bootstrap from ").withRequiredArg().describedAs("url");
    parser.accepts("parallelism", ("Number of rebalances to run in parallel (Default:" + RebalanceClientConfig.MAX_PARALLEL_REBALANCING + ")")).withRequiredArg().ofType(Integer.class).describedAs("parallelism");
    parser.accepts("tries", ("(1) Tries during rebalance (Default:" + RebalanceClientConfig.MAX_TRIES + ")(2) Number of tries while generating new metadata")).withRequiredArg().ofType(Integer.class).describedAs("num-tries");
    parser.accepts("generate", "Optimize the target cluster which has new nodes with empty partitions");
    parser.accepts("entropy", "True - if we want to run the entropy calculator. False - if we want to store keys").withRequiredArg().ofType(Boolean.class);
    parser.accepts("output-dir", ("Specify the output directory for (1) dumping metadata" + "(b) dumping entropy keys")).withRequiredArg().ofType(String.class).describedAs("path");
    parser.accepts("no-delete", "Do not delete after rebalancing (Valid only for RW Stores) ");
    parser.accepts("show-plan", "Shows the rebalancing plan only without executing the rebalance");
    parser.accepts("keys", ("The number of keys to use for entropy calculation [ Default : " + Entropy.DEFAULT_NUM_KEYS + " ]")).withRequiredArg().ofType(Long.class).describedAs("num-keys");
    OptionSet options = parser.parse(args);
    if (options.has("help"))
    {
      printHelp(System.out, parser);
      System.exit(HELP_EXIT_CODE);
    }
    boolean deleteAfterRebalancing = !options.has("no-delete");
    int parallelism = CmdUtils.valueOf(options, "parallelism", RebalanceClientConfig.MAX_PARALLEL_REBALANCING);
    int maxTriesRebalancing = CmdUtils.valueOf(options, "tries", RebalanceClientConfig.MAX_TRIES);
    boolean enabledShowPlan = options.has("show-plan");
    RebalanceClientConfig config = new RebalanceClientConfig();
    config.setMaxParallelRebalancing(parallelism);
    config.setDeleteAfterRebalancingEnabled(deleteAfterRebalancing);
    config.setEnableShowPlan(enabledShowPlan);
    config.setMaxTriesRebalancing(maxTriesRebalancing);
    if (options.has("output-dir"))
    {
      config.setOutputDirectory(((String) options.valueOf("output-dir")));
    }
    if (options.has("url"))
    {
      if (!options.has("target-cluster"))
      {
        System.err.println("Missing required arguments: target-cluster");
        printHelp(System.err, parser);
        System.exit(ERROR_EXIT_CODE);
      }
      String targetClusterXML = (String) options.valueOf("target-cluster");
      Cluster targetCluster = new ClusterMapper().readCluster(new File(targetClusterXML));
      String bootstrapURL = (String) options.valueOf("url");
      rebalanceController = new RebalanceController(bootstrapURL, config);
      rebalanceController.rebalance(targetCluster);
    }
    else
    {
      Set<String> missing = CmdUtils.missing(options, "current-cluster", "current-stores");
      if (missing.size() > 0)
      {
        System.err.println(("Missing required arguments: " + Joiner.on(", ").join(missing)));
        printHelp(System.err, parser);
        System.exit(ERROR_EXIT_CODE);
      }
      String currentClusterXML = (String) options.valueOf("current-cluster");
      String currentStoresXML = (String) options.valueOf("current-stores");
      Cluster currentCluster = new ClusterMapper().readCluster(new File(currentClusterXML));
      List<StoreDefinition> storeDefs = new StoreDefinitionsMapper().readStoreList(new File(currentStoresXML));
      if (options.has("entropy"))
      {
        if (!config.hasOutputDirectory())
        {
          System.err.println("Missing arguments output-dir");
          printHelp(System.err, parser);
          System.exit(ERROR_EXIT_CODE);
        }
        boolean entropy = (Boolean) options.valueOf("entropy");
        long numKeys = CmdUtils.valueOf(options, "keys", Entropy.DEFAULT_NUM_KEYS);
        Entropy generator = new Entropy(-1, parallelism, numKeys);
        generator.generateEntropy(currentCluster, storeDefs, new File(config.getOutputDirectory()), entropy);
        return;
      }
      if (!options.has("target-cluster"))
      {
        System.err.println("Missing required arguments: target-cluster");
        printHelp(System.err, parser);
        System.exit(ERROR_EXIT_CODE);
      }
      String targetClusterXML = (String) options.valueOf("target-cluster");
      Cluster targetCluster = new ClusterMapper().readCluster(new File(targetClusterXML));
      if (options.has("generate"))
      {
        RebalanceUtils.generateMinCluster(currentCluster, targetCluster, storeDefs, config.getOutputDirectory(), config.getMaxTriesRebalancing());
        return;
      }
      rebalanceController = new RebalanceController(currentCluster, config);
      rebalanceController.rebalance(currentCluster, targetCluster, storeDefs);
    }
    exitCode = SUCCESS_EXIT_CODE;
    if (logger.isInfoEnabled())
    {
      logger.info("Successfully terminated rebalance all tasks");
    }
  }
  catch (VoldemortException e)
  {
    logger.error(("Unsuccessfully terminated rebalance operation - " + e.getMessage()), e);
  }
  catch (Throwable e)
  {
    logger.error(e.getMessage(), e);
  }
  finally {
            if (rebalanceController != null)
            {
              try
              {
                rebalanceController.stop();
              }
              catch (Exception e)
              {
              }
            }
          }
  System.exit(exitCode);
}