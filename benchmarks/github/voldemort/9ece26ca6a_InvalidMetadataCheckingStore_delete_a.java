class InvalidMetadataCheckingStore{ 
 void delete() {
  StoreUtils.assertValidKey(key);
  StoreUtils.assertValidMetadata(key, metadata.getRoutingStrategy(getName()), metadata.getCluster().getNodeById(nodeId));
  return getInnerStore().delete(key, version);
}
}