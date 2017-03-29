class InvalidMetadataCheckingStore{ 
 void delete() {
  StoreUtils.assertValidKey(key);
  StoreUtils.assertValidMetadata(key, metadata.getRoutingStrategy(getName()), node);
  return getInnerStore().delete(key, version);
}
}