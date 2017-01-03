{
  StoreUtils.assertValidKey(key);
  BasicPipelineData<Boolean> pipelineData = new BasicPipelineData<Boolean>();
  if (zoneRoutingEnabled)
    pipelineData.setZonesRequired(storeDef.getZoneCountWrites());
  else
    pipelineData.setZonesRequired(null);
  pipelineData.setStoreName(name);
  Pipeline pipeline = new Pipeline(Operation.DELETE, timeoutMs, TimeUnit.MILLISECONDS);
  pipeline.setEnableHintedHandoff(isHintedHandoffEnabled());
  HintedHandoff hintedHandoff = null;
  if (isHintedHandoffEnabled())
    hintedHandoff = new HintedHandoff(failureDetector, slopStores, handoffStrategy, pipelineData.getFailedNodes());
  StoreRequest<Boolean> blockingDelete = new StoreRequest<Boolean>()
                                         {
                                           public Boolean request (Store<ByteArray, byte[]> store)
                                           {
                                             return store.delete(key, version);
                                           }
                                         };
  pipeline.addEventAction(Event.STARTED, new ConfigureNodes<Boolean, BasicPipelineData<Boolean>>(pipelineData, Event.CONFIGURED, failureDetector, storeDef.getRequiredWrites(), routingStrategy, key, clientZone));
  pipeline.addEventAction(Event.CONFIGURED, new PerformParallelRequests<Boolean, BasicPipelineData<Boolean>>(pipelineData, (isHintedHandoffEnabled() ? Event.RESPONSES_RECEIVED : Event.COMPLETED), key, failureDetector, storeDef.getPreferredWrites(), storeDef.getRequiredWrites(), timeoutMs, nonblockingStores, hintedHandoff, version, Event.INSUFFICIENT_SUCCESSES, Event.INSUFFICIENT_ZONES));
  pipeline.addEventAction(Event.INSUFFICIENT_SUCCESSES, new PerformSerialRequests<Boolean, BasicPipelineData<Boolean>>(pipelineData, (isHintedHandoffEnabled() ? Event.RESPONSES_RECEIVED : Event.COMPLETED), key, failureDetector, innerStores, storeDef.getPreferredWrites(), storeDef.getRequiredWrites(), blockingDelete, null));
  if (zoneRoutingEnabled)
    pipeline.addEventAction(Event.INSUFFICIENT_ZONES, new PerformZoneSerialRequests<Boolean, BasicPipelineData<Boolean>>(pipelineData, (isHintedHandoffEnabled() ? Event.RESPONSES_RECEIVED : Event.COMPLETED), key, failureDetector, innerStores, blockingDelete));
  if (isHintedHandoffEnabled())
  {
    pipeline.addEventAction(Event.RESPONSES_RECEIVED, new PerformDeleteHintedHandoff(pipelineData, Event.COMPLETED, key, version, hintedHandoff));
    pipeline.addEventAction(Event.ABORTED, new PerformDeleteHintedHandoff(pipelineData, Event.ERROR, key, version, hintedHandoff));
  }
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