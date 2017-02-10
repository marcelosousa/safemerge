class AbstractRebalanceTest{ 
 void populateData() {
  Map<Integer, Store<ByteArray, byte[]>> storeMap = new HashMap<Integer, Store<ByteArray, byte[]>>();
  for (int nodeId : nodeList) {
                                Node node = cluster.getNodeById(nodeId);
                                storeMap.put(nodeId, getSocketStore(testStoreName, node.getHost(), node.getSocketPort()));
                              }
  RoutingStrategy routing = new ConsistentRoutingStrategy(cluster.getNodes(), 1);
  for (Entry<String, String> entry : testEntries.entrySet()) {
                                                               int masterNode = routing.routeRequest(ByteUtils.getBytes(entry.getKey(), "UTF-8")).get(0).getId();
                                                               if (nodeList.contains(masterNode))
                                                               {
                                                                 try
                                                                 {
                                                                   ByteArray keyBytes = new ByteArray(ByteUtils.getBytes(entry.getKey(), "UTF-8"));
                                                                   storeMap.get(masterNode).put(keyBytes, new Versioned<byte[]>(ByteUtils.getBytes(entry.getValue(), "UTF-8")));
                                                                 }
                                                                 catch (ObsoleteVersionException e)
                                                                 {
                                                                   System.out.println("Why are we seeing this at all here ?? ");
                                                                   e.printStackTrace();
                                                                 }
                                                               }
                                                             }
  for (Store<ByteArray, byte[]> store : storeMap.values()) {
                                                             store.close();
                                                           }
}
}