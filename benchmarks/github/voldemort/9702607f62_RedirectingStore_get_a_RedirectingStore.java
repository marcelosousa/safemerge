class RedirectingStore{ 
 void get() {
  RebalancePartitionsInfo stealInfo = redirectingKey(key);
  if (stealInfo != null)
    proxyGetAndLocalPut(key, stealInfo.getDonorId(), transforms);
  return getInnerStore().get(key, transforms);
}
}