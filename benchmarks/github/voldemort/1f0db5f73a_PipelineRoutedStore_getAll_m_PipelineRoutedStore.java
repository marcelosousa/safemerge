{
  StoreUtils.assertValidKeys(keys);
  boolean allowReadRepair = repairReads && (transforms == null || transforms.size()) == 0;
  GetAllPipelineData pipelineData = new GetAllPipelineData();
  if (zoneRoutingEnabled)
    pipelineData.setZonesRequired(storeDef.getZoneCountReads());
  else
    pipelineData.setZonesRequired(null);
  Pipeline pipeline = new Pipeline(Operation.GET_ALL, timeoutMs, TimeUnit.MILLISECONDS);
  pipeline.addEventAction(Event.STARTED, new GetAllConfigureNodes(pipelineData, Event.CONFIGURED, failureDetector, storeDef.getPreferredReads(), storeDef.getRequiredReads(), routingStrategy, keys, transforms, clientZone));
  pipeline.addEventAction(Event.CONFIGURED, new PerformParallelGetAllRequests(pipelineData, Event.INSUFFICIENT_SUCCESSES, failureDetector, timeoutMs, nonblockingStores));
  pipeline.addEventAction(Event.INSUFFICIENT_SUCCESSES, new PerformSerialGetAllRequests(pipelineData, (allowReadRepair ? Event.RESPONSES_RECEIVED : Event.COMPLETED), keys, failureDetector, innerStores, storeDef.getPreferredReads(), storeDef.getRequiredReads()));
  if (allowReadRepair)
    pipeline.addEventAction(Event.RESPONSES_RECEIVED, new GetAllReadRepair(pipelineData, Event.COMPLETED, storeDef.getPreferredReads(), timeoutMs, nonblockingStores, readRepairer));
  pipeline.addEvent(Event.STARTED);
  pipeline.execute();
  if (pipelineData.getFatalError() != null)
    throw pipelineData.getFatalError();
  return pipelineData.getResult();
}