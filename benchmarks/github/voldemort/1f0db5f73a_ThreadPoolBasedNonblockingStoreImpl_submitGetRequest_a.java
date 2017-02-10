class ThreadPoolBasedNonblockingStoreImpl{ 
 void submitGetRequest() {
  submit(new StoreRequest<List<Versioned<byte[]>>>()
         {
           public List<Versioned<byte[]>> request (Store<ByteArray, byte[], byte[]> store)
           {
             return innerStore.get(key, transforms);
           }
         }, callback);
}
}