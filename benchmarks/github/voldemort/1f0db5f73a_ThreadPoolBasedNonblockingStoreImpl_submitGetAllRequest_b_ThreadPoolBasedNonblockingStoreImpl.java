class ThreadPoolBasedNonblockingStoreImpl{ 
 void submitGetAllRequest() {
  submit(new StoreRequest<Map<ByteArray, List<Versioned<byte[]>>>>()
         {
           public Map<ByteArray, List<Versioned<byte[]>>> request (Store<ByteArray, byte[]> store)
           {
             return innerStore.getAll(keys);
           }
         }, callback, timeoutMs, "get all");
}
}