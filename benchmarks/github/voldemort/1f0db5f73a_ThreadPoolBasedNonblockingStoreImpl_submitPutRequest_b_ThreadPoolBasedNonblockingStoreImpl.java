{
  submit(new StoreRequest<Void>()
         {
           public Void request (Store<ByteArray, byte[]> store)
           {
             innerStore.put(key, value);
             return null;
           }
         }, callback, timeoutMs, "put");
}