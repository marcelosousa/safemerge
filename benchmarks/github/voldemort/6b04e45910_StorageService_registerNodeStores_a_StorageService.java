class StorageService{ 
 void registerNodeStores() {
  Map<Integer, Store<ByteArray, byte[]>> nodeStores = new HashMap<Integer, Store<ByteArray, byte[]>>(cluster.getNumberOfNodes());
  for (Node node : cluster.getNodes()) {
                                         Store<ByteArray, byte[]> store = getNodeStore(def.getName(), node, localNode);
                                         this.storeRepository.addNodeStore(node.getId(), store);
                                         nodeStores.put(node.getId(), store);
                                       }
  Store<ByteArray, byte[]> store = new NewRoutedStore(def.getName(), nodeStores, metadata.getCluster(), def, true, this.clientThreadPool, voldemortConfig.getRoutingTimeoutMs(), failureDetector, SystemTime.INSTANCE);
  store = new RebootstrappingStore(metadata, storeRepository, voldemortConfig, socketPool, (RoutableStore) store);
  store = new InconsistencyResolvingStore<ByteArray, byte[]>(store, new VectorClockInconsistencyResolver<byte[]>());
  this.storeRepository.addRoutedStore(store);
}
}