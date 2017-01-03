{
  boolean written = voldemortStore.applyUpdate(new UpdateAction<Object, Object>()
                                               {
                                                 @Override
                                                 public void update (StoreClient<Object, Object> storeClient)
                                                 {
                                                   long startNs = System.nanoTime();
                                                   storeClient.put(key, value);
                                                   long endNs = System.nanoTime();
                                                   measurement.recordLatency(Operations.Write.getOpString(), ((int) ((endNs - startNs) / Time.NS_PER_MS)));
                                                 }
                                               });
  ReturnCode res = ReturnCode.Error;
  if (written)
  {
    res = ReturnCode.Ok;
  }
  measurement.recordReturnCode(Operations.Write.getOpString(), res.ordinal());
}