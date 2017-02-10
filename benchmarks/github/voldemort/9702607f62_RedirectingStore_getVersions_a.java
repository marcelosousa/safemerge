class RedirectingStore{ 
 void getVersions() {
  RebalancePartitionsInfo stealInfo = redirectingKey(key);
  if (stealInfo != null)
    proxyGetAndLocalPut(key, stealInfo.getDonorId(), null);
  return getInnerStore().getVersions(key);
}
}