class StorageService{ 
 void registerNodeStores() {
  Map<Integer, Store<ByteArray, byte[]>> nodeStores = new HashMap<Integer, Store<ByteArray, byte[]>>(cluster.getNumberOfNodes());
  for (Node node : cluster.getNodes()) {
                                         Store<ByteArray, byte[]> store;
                                         if (node.getId() == localNode)
                                         {
                                           store = this.storeRepository.getLocalStore(def.getName());
                                         }
                                         else
                                         {
                                           store = new SocketStore(def.getName(), new SocketDestination(node.getHost(), node.getSocketPort(), voldemortConfig.getRequestFormatType()), socketPool, false);
                                         }
                                         this.storeRepository.addNodeStore(node.getId(), store);
                                         nodeStores.put(node.getId(), store);
                                       }
  Store<ByteArray, byte[]> routedStore = new RoutedStore(def.getName(), nodeStores, cluster, def, true, this.clientThreadPool, voldemortConfig.getRoutingTimeoutMs(), failureDetector, SystemTime.INSTANCE);
  routedStore = new InconsistencyResolvingStore<ByteArray, byte[]>(routedStore, new VectorClockInconsistencyResolver<byte[]>());
  this.storeRepository.addRoutedStore(routedStore);
}
}