{
  failureDetectorListener = new FailureDetectorListener()
                            {
                              public void nodeAvailable (Node node)
                              {
                              }
                              public void nodeUnavailable (Node node)
                              {
                                if (logger.isInfoEnabled())
                                  logger.info((node + " has been marked as unavailable, destroying socket pool"));
                                SocketDestination destination = new SocketDestination(node.getHost(), node.getSocketPort(), config.getRequestFormatType());
                                storeFactory.close(destination);
                              }
                            };
  ClientStoreVerifier storeVerifier = new ClientStoreVerifier()
                                      {
                                        @Override
                                        protected Store<ByteArray, byte[], byte[]> getStoreInternal (Node node)
                                        {
                                          return SocketStoreClientFactory.this.getStore(MetadataStore.METADATA_STORE_NAME, node.getHost(), node.getSocketPort(), config.getRequestFormatType());
                                        }
                                      };
  FailureDetectorConfig failureDetectorConfig = new FailureDetectorConfig(config).setNodes(nodes).setStoreVerifier(storeVerifier);
  return create(failureDetectorConfig, false, failureDetectorListener);
}