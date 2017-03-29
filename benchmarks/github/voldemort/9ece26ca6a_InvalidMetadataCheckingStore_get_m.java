class InvalidMetadataCheckingStore{ 
 void get() {
  StoreUtils.assertValidKey(key);
  StoreUtils.assertValidMetadata(key, metadata.getRoutingStrategy(getName()), node);
  return getInnerStore().get(key);
}
}