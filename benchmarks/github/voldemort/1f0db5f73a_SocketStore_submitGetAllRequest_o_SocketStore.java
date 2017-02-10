class SocketStore{ 
 void submitGetAllRequest() {
  StoreUtils.assertValidKeys(keys);
  GetAllClientRequest clientRequest = new GetAllClientRequest(storeName, requestFormat, requestRoutingType, keys);
  requestAsync(clientRequest, callback);
}
}