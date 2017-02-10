class ThreadPoolBasedNonblockingStoreImpl{ 
 void submitDeleteRequest() {
  submit(new StoreRequest<Boolean>()
         {
           public Boolean request (Store<ByteArray, byte[]> store)
           {
             return innerStore.delete(key, version);
           }
         }, callback);
}
}