class RedirectingStore{ 
 void proxyGetAndLocalPut() {
  List<Versioned<byte[]>> proxyValues = proxyGet(key, donorId);
  if (!isReadOnly)
  {
    for (Versioned<byte[]> proxyValue : proxyValues) {
                                                       try
                                                       {
                                                         getInnerStore().put(key, proxyValue);
                                                       }
                                                       catch (ObsoleteVersionException e)
                                                       {
                                                       }
                                                     }
  }
  return proxyValues;
}
}