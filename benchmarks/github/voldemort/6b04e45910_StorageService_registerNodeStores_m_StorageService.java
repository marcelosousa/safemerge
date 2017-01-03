{
  Map<Integer, Store<ByteArray, byte[]>> nodeStores = new HashMap<Integer, Store<ByteArray, byte[]>>(cluster.getNumberOfNodes());
  Map<Integer, NonblockingStore> nonblockingStores = new HashMap<Integer, NonblockingStore>();
  for (Node node : cluster.getNodes()) {
                                         Store<ByteArray, byte[]> store = getNodeStore(def.getName(), node, localNode);
                                         this.storeRepository.addNodeStore(node.getId(), store);
                                         nodeStores.put(node.getId(), store);
                                         nonblockingStores.put(node.getId(), new ThreadPoolBasedNonblockingStoreImpl(this.clientThreadPool, store));
                                       }
  Store<ByteArray, byte[]> store = new NewRoutedStore(def.getName(), nodeStores, nonblockingStores, metadata.getCluster(), def, true, this.clientThreadPool, voldemortConfig.getRoutingTimeoutMs(), failureDetector);
  store = new RebootstrappingStore(metadata, storeRepository, voldemortConfig, (RoutableStore) store, storeFactory);
  store = new InconsistencyResolvingStore<ByteArray, byte[]>(store, new VectorClockInconsistencyResolver<byte[]>());
  this.storeRepository.addRoutedStore(store);
}