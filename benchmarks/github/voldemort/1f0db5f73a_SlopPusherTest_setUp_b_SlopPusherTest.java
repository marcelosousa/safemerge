{
  repo = new StoreRepository();
  repo.setSlopStore(new InMemoryStorageEngine<ByteArray, Slop>("slop"));
  repo.addNodeStore(0, new InMemoryStorageEngine<ByteArray, byte[]>(STORE_NAME));
  repo.addNodeStore(1, new InMemoryStorageEngine<ByteArray, byte[]>(STORE_NAME));
  this.failingNodeId = 2;
  repo.addNodeStore(failingNodeId, new FailingStore<ByteArray, byte[]>(STORE_NAME));
  pusher = new SlopPusherJob(repo, makeCluster(3), new NoopFailureDetector(), 10 * 1000 * 1000);
}