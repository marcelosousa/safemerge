class RedirectingStore{ 
 void get() {
  RebalancePartitionsInfo stealInfo = redirectingKey(key);
  if (stealInfo != null)
    proxyGetAndLocalPut(key, stealInfo.getDonorId());
  return getInnerStore().get(key);
}
}
