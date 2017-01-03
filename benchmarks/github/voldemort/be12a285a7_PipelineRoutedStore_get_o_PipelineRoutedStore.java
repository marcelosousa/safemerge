{
  StoreUtils.assertValidKey(key);
  BasicPipelineData<List<Versioned<byte[]>>> pipelineData = new BasicPipelineData<List<Versioned<byte[]>>>();
  if (zoneRoutingEnabled)
    pipelineData.setZonesRequired(storeDef.getZoneCountReads());
  else
    pipelineData.setZonesRequired(null);
  pipelineData.setStats(stats);
  final Pipeline pipeline = new Pipeline(Operation.GET, timeoutConfig.getOperationTimeout(VoldemortOpCode.GET_OP_CODE), TimeUnit.MILLISECONDS);
  boolean allowReadRepair = (repairReads && transforms) == null;
  StoreRequest<List<Versioned<byte[]>>> blockingStoreRequest = new StoreRequest<List<Versioned<byte[]>>>()
                                                               {
                                                                 public List<Versioned<byte[]>> request (Store<ByteArray, byte[], byte[]> store)
                                                                 {
                                                                   return store.get(key, transforms);
                                                                 }
                                                               };
  pipeline.addEventAction(Event.STARTED, new ConfigureNodes<List<Versioned<byte[]>>, BasicPipelineData<List<Versioned<byte[]>>>>(pipelineData, Event.CONFIGURED, failureDetector, storeDef.getRequiredReads(), routingStrategy, key, clientZone));
  pipeline.addEventAction(Event.CONFIGURED, new PerformParallelRequests<List<Versioned<byte[]>>, BasicPipelineData<List<Versioned<byte[]>>>>(pipelineData, (allowReadRepair ? Event.RESPONSES_RECEIVED : Event.COMPLETED), key, transforms, failureDetector, storeDef.getPreferredReads(), storeDef.getRequiredReads(), timeoutConfig.getOperationTimeout(VoldemortOpCode.GET_OP_CODE), nonblockingStores, Event.INSUFFICIENT_SUCCESSES, Event.INSUFFICIENT_ZONES));
  pipeline.addEventAction(Event.INSUFFICIENT_SUCCESSES, new PerformSerialRequests<List<Versioned<byte[]>>, BasicPipelineData<List<Versioned<byte[]>>>>(pipelineData, (allowReadRepair ? Event.RESPONSES_RECEIVED : Event.COMPLETED), key, failureDetector, innerStores, storeDef.getPreferredReads(), storeDef.getRequiredReads(), blockingStoreRequest, null));
  if (allowReadRepair)
    pipeline.addEventAction(Event.RESPONSES_RECEIVED, new ReadRepair<BasicPipelineData<List<Versioned<byte[]>>>>(pipelineData, Event.COMPLETED, storeDef.getPreferredReads(), timeoutConfig.getOperationTimeout(VoldemortOpCode.GET_OP_CODE), nonblockingStores, readRepairer));
  if (zoneRoutingEnabled)
    pipeline.addEventAction(Event.INSUFFICIENT_ZONES, new PerformZoneSerialRequests<List<Versioned<byte[]>>, BasicPipelineData<List<Versioned<byte[]>>>>(pipelineData, (allowReadRepair ? Event.RESPONSES_RECEIVED : Event.COMPLETED), key, failureDetector, innerStores, blockingStoreRequest));
  pipeline.addEvent(Event.STARTED);
  if (logger.isDebugEnabled())
  {
    logger.debug(("Operation " + pipeline.getOperation().getSimpleName() + " Key " + ByteUtils.toHexString(key.get())));
  }
  try
  {
    pipeline.execute();
  }
  catch (VoldemortException e)
  {
    stats.reportException(e);
    throw e;
  }
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