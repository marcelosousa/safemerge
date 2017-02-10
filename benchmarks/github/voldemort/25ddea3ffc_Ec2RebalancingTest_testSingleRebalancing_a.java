class Ec2RebalancingTest{ 
 void testSingleRebalancing() {
  int clusterSize = ec2RebalancingTestConfig.getInstanceCount();
  int[][] targetLayout;
  if (spareNode)
    targetLayout = splitLastPartition(partitionMap, (partitionMap[(clusterSize - 2)].length - 2));
  else
    targetLayout = insertNode(partitionMap, (partitionMap[(clusterSize - 1)].length - 2));
  if (logger.isInfoEnabled())
    logPartitionMap(targetLayout, "Target");
  Cluster targetCluster = ServerTestUtils.getLocalCluster(targetLayout.length, getPorts(targetLayout.length), targetLayout);
  List<Integer> originalNodes = new ArrayList<Integer>();
  for (Node node : originalCluster.getNodes()) {
                                                 originalNodes.add(node.getId());
                                               }
  targetCluster = expandCluster((targetCluster.getNumberOfNodes() - clusterSize), targetCluster);
  try
  {
    RebalanceController rebalanceClient = new RebalanceController(getBootstrapUrl(Arrays.asList(originalCluster.getNodeById(0).getHost())), new RebalanceClientConfig());
    populateData(originalCluster, originalNodes);
    rebalanceAndCheck(originalCluster, targetCluster, rebalanceClient, (spareNode ? Arrays.asList((clusterSize - 1)) : originalNodes));
  }
  finally {
            stopCluster(hostNames, ec2RebalancingTestConfig);
          }
}
}