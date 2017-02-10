class SocketStore{ 
 void submitPutRequest() {
  StoreUtils.assertValidKey(key);
  PutClientRequest clientRequest = new PutClientRequest(storeName, requestFormat, requestRoutingType, key, value);
  requestAsync(clientRequest, callback, timeoutMs, "put");
}
}