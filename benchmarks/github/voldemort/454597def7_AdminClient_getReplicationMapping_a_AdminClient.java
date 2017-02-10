class AdminClient{ 
 void getReplicationMapping() {
  Node node = cluster.getNodeById(nodeId);
  Map<Integer, Integer> partitionsToNodeMapping = RebalanceUtils.getCurrentPartitionMapping(cluster);
  HashMap<Integer, List<Integer>> restoreMapping = new HashMap<Integer, List<Integer>>();
  for (int partition : node.getPartitionIds()) {
                                                 List<Integer> replicationPartitionsList = strategy.getReplicatingPartitionList(partition);
                                                 if (replicationPartitionsList.size() > 1)
                                                 {
                                                   int index = 0;
                                                   int replicatingPartition = replicationPartitionsList.get(index++);
                                                   while (partition == replicatingPartition)
                                                   {
                                                     replicatingPartition = replicationPartitionsList.get(index++);
                                                   }
                                                   int replicatingNode = partitionsToNodeMapping.get(replicatingPartition);
                                                   if (!restoreMapping.containsKey(replicatingNode))
                                                   {
                                                     restoreMapping.put(replicatingNode, new ArrayList<Integer>());
                                                   }
                                                   restoreMapping.get(replicatingNode).add(partition);
                                                 }
                                               }
  return restoreMapping;
}
}