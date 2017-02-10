class SocketStore{ 
 void submitPutRequest() {
  StoreUtils.assertValidKey(key);
  PutClientRequest clientRequest = new PutClientRequest(storeName, requestFormat, requestRoutingType, key, value, transforms);
  requestAsync(clientRequest, callback);
}
}