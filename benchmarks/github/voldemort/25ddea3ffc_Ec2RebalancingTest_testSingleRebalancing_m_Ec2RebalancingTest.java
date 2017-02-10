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
                                                 if (node.getId() == clusterSize - 1 && spareNode)
                                                   break;
                                                 originalNodes.add(node.getId());
                                               }
  try
  {
    targetCluster = expandCluster((targetCluster.getNumberOfNodes() - clusterSize), targetCluster);
    RebalanceController RebalanceController = new RebalanceController(getBootstrapUrl(originalCluster, 0), new RebalanceClientConfig());
    populateData(originalCluster, originalNodes);
    rebalanceAndCheck(originalCluster, targetCluster, RebalanceController, originalNodes);
  }
  finally {
            stopCluster(hostNames, ec2RebalancingTestConfig);
          }
}
}