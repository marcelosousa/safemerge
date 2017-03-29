class InvalidMetadataCheckingStore{ 
 void get() {
  StoreUtils.assertValidKey(key);
  StoreUtils.assertValidMetadata(key, metadata.getRoutingStrategy(getName()), nodeId);
  return getInnerStore().get(key);
}
}