class InvalidMetadataCheckingStore{ 
 void delete() {
  StoreUtils.assertValidKey(key);
  StoreUtils.assertValidMetadata(key, metadata.getRoutingStrategy(getName()), nodeId);
  return getInnerStore().delete(key, version);
}
}