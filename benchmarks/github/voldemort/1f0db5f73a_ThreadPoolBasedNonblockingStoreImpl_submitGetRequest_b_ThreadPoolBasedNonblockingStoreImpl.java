{
  submit(new StoreRequest<List<Versioned<byte[]>>>()
         {
           public List<Versioned<byte[]>> request (Store<ByteArray, byte[]> store)
           {
             return innerStore.get(key);
           }
         }, callback, timeoutMs, "get");
}