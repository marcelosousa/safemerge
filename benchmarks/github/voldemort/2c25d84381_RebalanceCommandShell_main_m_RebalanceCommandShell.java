{
  if (args.length != 3)
    Utils.croak("USAGE: java RebalanceCommandShell bootstrapURL targetCluster.xml maxParallelRebalancing");
  String bootstrapURL = args[0];
  Cluster targetCluster = clusterMapper.readCluster(new File(args[1]));
  int maxParallelRebalancing = Integer.parseInt(args[2]);
  RebalanceClientConfig config = new RebalanceClientConfig();
  config.setMaxParallelRebalancing(maxParallelRebalancing);
  rebalanceClient = new RebalanceController(bootstrapURL, config);
  rebalanceClient.rebalance(targetCluster);
}