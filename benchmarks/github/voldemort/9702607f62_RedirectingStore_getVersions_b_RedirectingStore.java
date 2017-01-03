{
  RebalancePartitionsInfo stealInfo = redirectingKey(key);
  if (stealInfo != null)
  {
    List<Versioned<byte[]>> proxyValues = proxyGetAndLocalPut(key, stealInfo.getDonorId());
    if (isReadOnly)
      return StoreUtils.getVersions(proxyValues);
  }
  return getInnerStore().getVersions(key);
}