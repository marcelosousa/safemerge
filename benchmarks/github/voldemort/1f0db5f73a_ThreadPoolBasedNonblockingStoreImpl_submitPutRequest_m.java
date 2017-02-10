class ThreadPoolBasedNonblockingStoreImpl{ 
 void submitPutRequest() {
  submit(new StoreRequest<Void>()
         {
           public Void request (Store<ByteArray, byte[], byte[]> store)
           {
             innerStore.put(key, value, transforms);
             return null;
           }
         }, callback, timeoutMs, "put");
}
}