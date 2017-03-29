class InvalidMetadataCheckingStore{ 
 void get() {
  StoreUtils.assertValidKey(key);
  StoreUtils.assertValidMetadata(key, metadata.getRoutingStrategy(getName()), metadata.getCluster().getNodeById(nodeId));
  return getInnerStore().get(key);
}
}