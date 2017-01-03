{
  Map<Integer, Store<ByteArray, byte[]>> nodeStores = new HashMap<Integer, Store<ByteArray, byte[]>>(cluster.getNumberOfNodes());
  for (Node node : cluster.getNodes()) {
                                         Store<ByteArray, byte[]> store = getNodeStore(def.getName(), node, localNode);
                                         this.storeRepository.addNodeStore(node.getId(), store);
                                         nodeStores.put(node.getId(), store);
                                       }
  Store<ByteArray, byte[]> routedStore = new RoutedStore(def.getName(), nodeStores, metadata.getCluster(), def, true, this.clientThreadPool, voldemortConfig.getRoutingTimeoutMs(), failureDetector, SystemTime.INSTANCE);
  routedStore = new RebootstrappingStore(metadata, storeRepository, voldemortConfig, storeFactory, (RoutedStore) routedStore);
  routedStore = new InconsistencyResolvingStore<ByteArray, byte[]>(routedStore, new VectorClockInconsistencyResolver<byte[]>());
  this.storeRepository.addRoutedStore(routedStore);
}