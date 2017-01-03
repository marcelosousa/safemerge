{
  Map<Integer, Store<ByteArray, byte[]>> nodeStores = new HashMap<Integer, Store<ByteArray, byte[]>>(cluster.getNumberOfNodes());
  Map<Integer, NonblockingStore> nonblockingStores = new HashMap<Integer, NonblockingStore>(cluster.getNumberOfNodes());
  try
  {
    for (Node node : cluster.getNodes()) {
                                           Store<ByteArray, byte[]> store = getNodeStore(def.getName(), node, localNode);
                                           this.storeRepository.addNodeStore(node.getId(), store);
                                           nodeStores.put(node.getId(), store);
                                           NonblockingStore nonblockingStore = routedStoreFactory.toNonblockingStore(store);
                                           nonblockingStores.put(node.getId(), nonblockingStore);
                                         }
    Store<ByteArray, byte[]> store = routedStoreFactory.create(cluster, def, nodeStores, nonblockingStores, true, cluster.getNodeById(localNode).getZoneId(), failureDetector);
    store = new RebootstrappingStore(metadata, storeRepository, voldemortConfig, (RoutedStore) store, storeFactory);
    store = new InconsistencyResolvingStore<ByteArray, byte[]>(store, new VectorClockInconsistencyResolver<byte[]>());
    this.storeRepository.addRoutedStore(store);
  }
  catch (Exception e)
  {
    for (Node node : cluster.getNodes()) this.storeRepository.removeNodeStore(def.getName(), node.getId());
    throw new VoldemortException(e);
  }
}