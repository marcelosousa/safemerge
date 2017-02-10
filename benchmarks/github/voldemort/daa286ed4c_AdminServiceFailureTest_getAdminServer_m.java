class AdminServiceFailureTest{ 
 void getAdminServer() {
  StoreRepository storeRepository = new StoreRepository();
  storeRepository.addStorageEngine(storageEngine);
  storeRepository.addLocalStore(storageEngine);
  SocketRequestHandlerFactory requestHandlerFactory = new SocketRequestHandlerFactory(null, storeRepository, ServerTestUtils.createMetadataStore(cluster, storeDefs), ServerTestUtils.createServerConfig(useNio, 0, TestUtils.createTempDir().getAbsolutePath(), null, null, new Properties()), null, null);
  return ServerTestUtils.getSocketService(useNio, requestHandlerFactory, node.getAdminPort(), 2, 2, 10000);
}
}