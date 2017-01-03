{
  submit(new StoreRequest<Boolean>()
         {
           public Boolean request (Store<ByteArray, byte[], byte[]> store)
           {
             return innerStore.delete(key, version);
           }
         }, callback);
}