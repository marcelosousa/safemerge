class RedirectingStore{ 
 void get() {
  RebalancePartitionsInfo stealInfo = redirectingKey(key);
  if (stealInfo != null)
  {
    List<Versioned<byte[]>> proxyValues = proxyGetAndLocalPut(key, stealInfo.getDonorId());
    if (isReadOnly)
      return proxyValues;
  }
  return getInnerStore().get(key);
}
}