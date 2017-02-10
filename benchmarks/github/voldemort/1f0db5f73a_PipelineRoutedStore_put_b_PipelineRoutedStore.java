class PipelineRoutedStore{ 
 void put() {
  StoreUtils.assertValidKey(key);
  PutPipelineData pipelineData = new PutPipelineData();
  if (zoneRoutingEnabled)
    pipelineData.setZonesRequired(storeDef.getZoneCountWrites());
  else
    pipelineData.setZonesRequired(null);
  pipelineData.setStartTimeNs(System.nanoTime());
  pipelineData.setStoreName(name);
  Pipeline pipeline = new Pipeline(Operation.PUT, timeoutMs, TimeUnit.MILLISECONDS);
  pipeline.setEnableHintedHandoff(isHintedHandoffEnabled());
  HintedHandoff hintedHandoff = null;
  if (isHintedHandoffEnabled())
    hintedHandoff = new HintedHandoff(failureDetector, slopStores, handoffStrategy, pipelineData.getFailedNodes());
  pipeline.addEventAction(Event.STARTED, new ConfigureNodes<Void, PutPipelineData>(pipelineData, Event.CONFIGURED, failureDetector, storeDef.getRequiredWrites(), routingStrategy, key, clientZone));
  pipeline.addEventAction(Event.CONFIGURED, new PerformSerialPutRequests(pipelineData, (isHintedHandoffEnabled() ? Event.RESPONSES_RECEIVED : Event.COMPLETED), key, failureDetector, innerStores, storeDef.getRequiredWrites(), versioned, time, Event.MASTER_DETERMINED));
  pipeline.addEventAction(Event.MASTER_DETERMINED, new PerformParallelPutRequests(pipelineData, Event.RESPONSES_RECEIVED, key, failureDetector, storeDef.getPreferredWrites(), storeDef.getRequiredWrites(), timeoutMs, nonblockingStores, hintedHandoff));
  if (isHintedHandoffEnabled())
  {
    pipeline.addEventAction(Event.ABORTED, new PerformPutHintedHandoff(pipelineData, Event.ERROR, key, versioned, hintedHandoff, time));
    pipeline.addEventAction(Event.RESPONSES_RECEIVED, new PerformPutHintedHandoff(pipelineData, Event.HANDOFF_FINISHED, key, versioned, hintedHandoff, time));
    pipeline.addEventAction(Event.HANDOFF_FINISHED, new IncrementClock(pipelineData, Event.COMPLETED, versioned, time));
  }
  else
    pipeline.addEventAction(Event.RESPONSES_RECEIVED, new IncrementClock(pipelineData, Event.COMPLETED, versioned, time));
  pipeline.addEvent(Event.STARTED);
  pipeline.execute();
  if (pipelineData.getFatalError() != null)
    throw pipelineData.getFatalError();
}
}