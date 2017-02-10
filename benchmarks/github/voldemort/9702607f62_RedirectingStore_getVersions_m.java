class RedirectingStore{ 
 void getVersions() {
  RebalancePartitionsInfo stealInfo = redirectingKey(key);
  if (stealInfo != null)
  {
    List<Versioned<byte[]>> proxyValues = proxyGetAndLocalPut(key, stealInfo.getDonorId(), null);
    if (isReadOnly)
      return StoreUtils.getVersions(proxyValues);
  }
  return getInnerStore().getVersions(key);
}
}