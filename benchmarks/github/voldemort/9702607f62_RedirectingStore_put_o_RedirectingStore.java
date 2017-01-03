{
  RebalancePartitionsInfo stealInfo = redirectingKey(key);
  if (stealInfo != null)
    proxyGetAndLocalPut(key, stealInfo.getDonorId());
  getInnerStore().put(key, value);
}