{
  if (args.length != 4)
    Utils.croak("USAGE: java RebalanceCommandShell bootstrapURL currentCluster.xml targetCluster.xml maxParallelRebalancing");
  String bootstrapURL = args[0];
  Cluster currentCluster = clusterMapper.readCluster(new File(args[1]));
  Cluster targetCluster = clusterMapper.readCluster(new File(args[2]));
  int maxParallelRebalancing = Integer.parseInt(args[3]);
  RebalanceClientConfig config = new RebalanceClientConfig();
  config.setMaxParallelRebalancing(maxParallelRebalancing);
  rebalanceClient = new RebalanceController(bootstrapURL, config);
  rebalanceClient.rebalance(currentCluster, targetCluster);
}