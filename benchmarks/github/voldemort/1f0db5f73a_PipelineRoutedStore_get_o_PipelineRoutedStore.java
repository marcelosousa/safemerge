{
  StoreUtils.assertValidKey(key);
  BasicPipelineData<List<Versioned<byte[]>>> pipelineData = new BasicPipelineData<List<Versioned<byte[]>>>();
  if (zoneRoutingEnabled)
    pipelineData.setZonesRequired(storeDef.getZoneCountReads());
  else
    pipelineData.setZonesRequired(null);
  final Pipeline pipeline = new Pipeline(Operation.GET, timeoutMs, TimeUnit.MILLISECONDS);
  NonblockingStoreRequest nonblockingStoreRequest = new NonblockingStoreRequest()
                                                    {
                                                      public void submit (Node node, NonblockingStore store, NonblockingStoreCallback callback)
                                                      {
                                                        store.submitGetRequest(key, callback);
                                                      }
                                                    };
  StoreRequest<List<Versioned<byte[]>>> blockingStoreRequest = new StoreRequest<List<Versioned<byte[]>>>()
                                                               {
                                                                 public List<Versioned<byte[]>> request (Store<ByteArray, byte[]> store)
                                                                 {
                                                                   return store.get(key);
                                                                 }
                                                               };
  pipeline.addEventAction(Event.STARTED, new ConfigureNodes<List<Versioned<byte[]>>, BasicPipelineData<List<Versioned<byte[]>>>>(pipelineData, Event.CONFIGURED, failureDetector, storeDef.getRequiredReads(), routingStrategy, key, clientZone));
  pipeline.addEventAction(Event.CONFIGURED, new PerformParallelRequests<List<Versioned<byte[]>>, BasicPipelineData<List<Versioned<byte[]>>>>(pipelineData, (repairReads ? Event.RESPONSES_RECEIVED : Event.COMPLETED), key, failureDetector, storeDef.getPreferredReads(), storeDef.getRequiredReads(), timeoutMs, nonblockingStores, nonblockingStoreRequest, Event.INSUFFICIENT_SUCCESSES, Event.INSUFFICIENT_ZONES));
  pipeline.addEventAction(Event.INSUFFICIENT_SUCCESSES, new PerformSerialRequests<List<Versioned<byte[]>>, BasicPipelineData<List<Versioned<byte[]>>>>(pipelineData, (repairReads ? Event.RESPONSES_RECEIVED : Event.COMPLETED), key, failureDetector, innerStores, storeDef.getPreferredReads(), storeDef.getRequiredReads(), blockingStoreRequest, null));
  if (repairReads)
    pipeline.addEventAction(Event.RESPONSES_RECEIVED, new ReadRepair<BasicPipelineData<List<Versioned<byte[]>>>>(pipelineData, Event.COMPLETED, storeDef.getPreferredReads(), nonblockingStores, readRepairer));
  if (zoneRoutingEnabled)
    pipeline.addEventAction(Event.INSUFFICIENT_ZONES, new PerformZoneSerialRequests<List<Versioned<byte[]>>, BasicPipelineData<List<Versioned<byte[]>>>>(pipelineData, (repairReads ? Event.RESPONSES_RECEIVED : Event.COMPLETED), key, failureDetector, innerStores, blockingStoreRequest));
  pipeline.addEvent(Event.STARTED);
  pipeline.execute();
  if (pipelineData.getFatalError() != null)
    throw pipelineData.getFatalError();
  List<Versioned<byte[]>> results = new ArrayList<Versioned<byte[]>>();
  for (Response<ByteArray, List<Versioned<byte[]>>> response : pipelineData.getResponses()) {
                                                                                              List<Versioned<byte[]>> value = response.getValue();
                                                                                              if (value != null)
                                                                                                results.addAll(value);
                                                                                            }
  return results;
}