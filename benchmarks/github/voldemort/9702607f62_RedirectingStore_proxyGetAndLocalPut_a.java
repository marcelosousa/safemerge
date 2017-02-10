class RedirectingStore{ 
 void proxyGetAndLocalPut() {
  List<Versioned<byte[]>> proxyValues = proxyGet(key, donorId, transforms);
  for (Versioned<byte[]> proxyValue : proxyValues) {
                                                     try
                                                     {
                                                       getInnerStore().put(key, proxyValue, null);
                                                     }
                                                     catch (ObsoleteVersionException e)
                                                     {
                                                     }
                                                   }
}
}