{
  List<Node> nodes = pipelineData.getNodes();
  while ((pipelineData.getNodeIndex() < nodes.size() && pipelineData.getZoneResponses().size() + 1) < pipelineData.getZonesRequired())
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
  int zonesSatisfied = pipelineData.getZoneResponses().size();
  if (zonesSatisfied >= pipelineData.getZonesRequired() + 1)
  {
    pipeline.addEvent(completeEvent);
  }
  else
  {
    pipelineData.setFatalError(new InsufficientZoneResponsesException((pipelineData.getZonesRequired() + 1 + " " + pipeline.getOperation().getSimpleName() + "s required zone, but only " + zonesSatisfied + " succeeded")));
    pipeline.addEvent(Event.ERROR);
  }
}