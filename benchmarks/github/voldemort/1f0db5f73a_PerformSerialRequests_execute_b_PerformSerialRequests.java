class PerformSerialRequests{ 
 void execute() {
  List<Node> nodes = pipelineData.getNodes();
  while ((pipelineData.getSuccesses() < preferred && pipelineData.getNodeIndex()) < nodes.size())
  {
    Node node = nodes.get(pipelineData.getNodeIndex());
    long start = System.nanoTime();
    try
    {
      Store<ByteArray, byte[]> store = stores.get(node.getId());
      V result = storeRequest.request(store);
      Response<ByteArray, V> response = new Response<ByteArray, V>(node, key, result, (System.nanoTime() - start) / Time.NS_PER_MS);
      pipelineData.incrementSuccesses();
      pipelineData.getResponses().add(response);
      failureDetector.recordSuccess(response.getNode(), response.getRequestTime());
      pipelineData.getZoneResponses().add(node.getZoneId());
    }
    catch (Exception e)
    {
      long requestTime = (System.nanoTime() - start) / Time.NS_PER_MS;
      if (handleResponseError(e, node, requestTime, pipeline, failureDetector))
        return;
    }
    pipelineData.incrementNodeIndex();
  }
  if (pipelineData.getSuccesses() < required)
  {
    if (insufficientSuccessesEvent != null)
    {
      pipeline.addEvent(insufficientSuccessesEvent);
    }
    else
    {
      pipelineData.setFatalError(new InsufficientOperationalNodesException((required + " " + pipeline.getOperation().getSimpleName() + "s required, but only " + pipelineData.getSuccesses() + " succeeded"), pipelineData.getFailures()));
      pipeline.abort();
    }
  }
  else
  {
    if (pipelineData.getZonesRequired() != null)
    {
      int zonesSatisfied = pipelineData.getZoneResponses().size();
      if (zonesSatisfied >= pipelineData.getZonesRequired() + 1)
      {
        pipeline.addEvent(completeEvent);
      }
      else
      {
        pipelineData.setFatalError(new InsufficientZoneResponsesException((pipelineData.getZonesRequired() + 1 + " " + pipeline.getOperation().getSimpleName() + "s required zone, but only " + zonesSatisfied + " succeeded")));
        pipeline.abort();
      }
    }
    else
    {
      pipeline.addEvent(completeEvent);
    }
  }
}
}