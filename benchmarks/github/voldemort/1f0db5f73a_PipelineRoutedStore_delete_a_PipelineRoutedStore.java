class PipelineRoutedStore{ 
 void delete() {
  StoreUtils.assertValidKey(key);
  BasicPipelineData<Boolean> pipelineData = new BasicPipelineData<Boolean>();
  if (zoneRoutingEnabled)
    pipelineData.setZonesRequired(storeDef.getZoneCountWrites());
  else
    pipelineData.setZonesRequired(null);
  final Pipeline pipeline = new Pipeline(Operation.DELETE, timeoutMs, TimeUnit.MILLISECONDS);
  NonblockingStoreRequest nonblockingDelete = new NonblockingStoreRequest()
                                              {
                                                public void submit (Node node, NonblockingStore store, NonblockingStoreCallback callback)
                                                {
                                                  store.submitDeleteRequest(key, version, callback);
                                                }
                                              };
  StoreRequest<Boolean> blockingDelete = new StoreRequest<Boolean>()
                                         {
                                           public Boolean request (Store<ByteArray, byte[], byte[]> store)
                                           {
                                             return store.delete(key, version);
                                           }
                                         };
  pipeline.addEventAction(Event.STARTED, new ConfigureNodes<Boolean, BasicPipelineData<Boolean>>(pipelineData, Event.CONFIGURED, failureDetector, storeDef.getRequiredWrites(), routingStrategy, key, clientZone));
  pipeline.addEventAction(Event.CONFIGURED, new PerformParallelRequests<Boolean, BasicPipelineData<Boolean>>(pipelineData, Event.COMPLETED, key, failureDetector, storeDef.getPreferredWrites(), storeDef.getRequiredWrites(), timeoutMs, nonblockingStores, nonblockingDelete, Event.INSUFFICIENT_SUCCESSES, Event.INSUFFICIENT_ZONES));
  pipeline.addEventAction(Event.INSUFFICIENT_SUCCESSES, new PerformSerialRequests<Boolean, BasicPipelineData<Boolean>>(pipelineData, Event.COMPLETED, key, failureDetector, innerStores, storeDef.getPreferredWrites(), storeDef.getRequiredWrites(), blockingDelete, null));
  if (zoneRoutingEnabled)
    pipeline.addEventAction(Event.INSUFFICIENT_ZONES, new PerformZoneSerialRequests<Boolean, BasicPipelineData<Boolean>>(pipelineData, Event.COMPLETED, key, failureDetector, innerStores, blockingDelete));
  pipeline.addEvent(Event.STARTED);
  pipeline.execute();
  if (pipelineData.getFatalError() != null)
    throw pipelineData.getFatalError();
  for (Response<ByteArray, Boolean> response : pipelineData.getResponses()) {
                                                                              if (response.getValue().booleanValue())
                                                                                return true;
                                                                            }
  return false;
}
}