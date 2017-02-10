class PerformSerialPutRequests{ 
 void execute() {
  int currentNode = 0;
  List<Node> nodes = pipelineData.getNodes();
  if (logger.isDebugEnabled())
    logger.debug("Performing serial put requests to determine master");
  for (; currentNode < nodes.size() ; currentNode++)
  {
    Node node = nodes.get(currentNode);
    pipelineData.incrementNodeIndex();
    VectorClock versionedClock = (VectorClock) versioned.getVersion();
    final Versioned<byte[]> versionedCopy = new Versioned<byte[]>(versioned.getValue(), versionedClock.incremented(node.getId(), time.getMilliseconds()));
    if (logger.isTraceEnabled())
      logger.trace(("Attempt #" + currentNode + 1 + " to perform put (node " + node.getId() + ")"));
    long start = System.nanoTime();
    try
    {
      stores.get(node.getId()).put(key, versionedCopy, transforms);
      long requestTime = (System.nanoTime() - start) / Time.NS_PER_MS;
      pipelineData.incrementSuccesses();
      failureDetector.recordSuccess(node, requestTime);
      if (logger.isTraceEnabled())
        logger.trace(("Put on node " + node.getId() + " succeeded, using as master"));
      pipelineData.setMaster(node);
      pipelineData.setVersionedCopy(versionedCopy);
      pipelineData.getZoneResponses().add(node.getZoneId());
      break;
    }
    catch (Exception e)
    {
      long requestTime = (System.nanoTime() - start) / Time.NS_PER_MS;
      if (handleResponseError(e, node, requestTime, pipeline, failureDetector))
        return;
    }
  }
  if (pipelineData.getSuccesses() < 1)
  {
    List<Exception> failures = pipelineData.getFailures();
    pipelineData.setFatalError(new InsufficientOperationalNodesException("No master node succeeded!", (failures.size() > 0 ? failures.get(0) : null)));
    pipeline.addEvent(Event.ERROR);
    return;
  }
  currentNode++;
  if (currentNode == nodes.size())
  {
    if (pipelineData.getSuccesses() < required)
    {
      pipelineData.setFatalError(new InsufficientOperationalNodesException((required + " " + pipeline.getOperation().getSimpleName() + "s required, but only " + pipelineData.getSuccesses() + " succeeded"), pipelineData.getFailures()));
      pipeline.addEvent(Event.ERROR);
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
          pipeline.addEvent(Event.ERROR);
        }
      }
      else
      {
        pipeline.addEvent(completeEvent);
      }
    }
  }
  else
  {
    pipeline.addEvent(masterDeterminedEvent);
  }
}
}