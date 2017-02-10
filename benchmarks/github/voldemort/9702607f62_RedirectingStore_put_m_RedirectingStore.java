class RedirectingStore{ 
 void put() {
  if (this.isReadOnly)
    throw new UnsupportedOperationException("Put is not supported on this store, it is read-only.");
  RebalancePartitionsInfo stealInfo = redirectingKey(key);
  if (stealInfo != null)
    proxyGetAndLocalPut(key, stealInfo.getDonorId(), transforms);
  getInnerStore().put(key, value, transforms);
}
}