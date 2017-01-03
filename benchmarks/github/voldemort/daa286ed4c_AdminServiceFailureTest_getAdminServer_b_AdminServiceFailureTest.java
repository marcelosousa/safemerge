{
  StoreRepository storeRepository = new StoreRepository();
  storeRepository.addStorageEngine(storageEngine);
  storeRepository.addLocalStore(storageEngine);
  return new SocketService(new SocketRequestHandlerFactory(null, storeRepository, ServerTestUtils.createMetadataStore(cluster, storeDefs), ServerTestUtils.createServerConfig(0, TestUtils.createTempDir().getAbsolutePath(), null, null, new Properties()), null, null), node.getAdminPort(), 2, 2, 10000, "test-admin-service", false);
}