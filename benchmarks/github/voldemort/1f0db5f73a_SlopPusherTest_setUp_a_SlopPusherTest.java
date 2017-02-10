class SlopPusherTest{ 
 void setUp() {
  repo = new StoreRepository();
  repo.setSlopStore(new InMemoryStorageEngine<ByteArray, Slop, byte[]>("slop"));
  repo.addNodeStore(0, new InMemoryStorageEngine<ByteArray, byte[], byte[]>(STORE_NAME));
  repo.addNodeStore(1, new InMemoryStorageEngine<ByteArray, byte[], byte[]>(STORE_NAME));
  this.failingNodeId = 2;
  repo.addNodeStore(failingNodeId, new FailingStore<ByteArray, byte[], byte[]>(STORE_NAME));
  pusher = new SlopPusherJob(repo);
}
}