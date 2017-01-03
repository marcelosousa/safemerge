{
  submit(new StoreRequest<Map<ByteArray, List<Versioned<byte[]>>>>()
         {
           public Map<ByteArray, List<Versioned<byte[]>>> request (Store<ByteArray, byte[], byte[]> store)
           {
             return innerStore.getAll(keys, transforms);
           }
         }, callback);
}