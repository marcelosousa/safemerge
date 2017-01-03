{
  StoreUtils.assertValidKeys(keys);
  GetAllClientRequest clientRequest = new GetAllClientRequest(storeName, requestFormat, requestRoutingType, keys, transforms);
  requestAsync(clientRequest, callback);
}