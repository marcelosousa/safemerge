{
  RebalancePartitionsInfo stealInfo = redirectingKey(key);
  if (stealInfo != null)
  {
    List<Versioned<byte[]>> proxyValues = proxyGetAndLocalPut(key, stealInfo.getDonorId(), transforms);
    if (isReadOnly)
      return proxyValues;
  }
  return getInnerStore().get(key, transforms);
}