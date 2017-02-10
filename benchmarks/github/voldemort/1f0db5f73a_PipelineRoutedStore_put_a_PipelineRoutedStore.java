class PipelineRoutedStore{ 
 void put() {
  StoreUtils.assertValidKey(key);
  PutPipelineData pipelineData = new PutPipelineData();
  if (zoneRoutingEnabled)
    pipelineData.setZonesRequired(storeDef.getZoneCountWrites());
  else
    pipelineData.setZonesRequired(null);
  pipelineData.setStartTimeNs(System.nanoTime());
  Pipeline pipeline = new Pipeline(Operation.PUT, timeoutMs, TimeUnit.MILLISECONDS);
  pipeline.addEventAction(Event.STARTED, new ConfigureNodes<Void, PutPipelineData>(pipelineData, Event.CONFIGURED, failureDetector, storeDef.getRequiredWrites(), routingStrategy, key, clientZone));
  pipeline.addEventAction(Event.CONFIGURED, new PerformSerialPutRequests(pipelineData, Event.COMPLETED, key, transforms, failureDetector, innerStores, storeDef.getRequiredWrites(), versioned, time, Event.MASTER_DETERMINED));
  pipeline.addEventAction(Event.MASTER_DETERMINED, new PerformParallelPutRequests(pipelineData, Event.RESPONSES_RECEIVED, key, transforms, failureDetector, storeDef.getPreferredWrites(), storeDef.getRequiredWrites(), timeoutMs, nonblockingStores));
  pipeline.addEventAction(Event.RESPONSES_RECEIVED, new IncrementClock(pipelineData, Event.COMPLETED, versioned, time));
  pipeline.addEvent(Event.STARTED);
  pipeline.execute();
  if (pipelineData.getFatalError() != null)
    throw pipelineData.getFatalError();
}
}