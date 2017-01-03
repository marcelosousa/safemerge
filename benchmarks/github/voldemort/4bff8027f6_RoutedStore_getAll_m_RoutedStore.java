{
  StoreUtils.assertValidKeys(keys);
  Map<ByteArray, List<Versioned<byte[]>>> result = StoreUtils.newEmptyHashMap(keys);
  Map<Node, List<ByteArray>> nodeToKeysMap = Maps.newHashMap();
  Map<ByteArray, List<Node>> keyToExtraNodesMap = Maps.newHashMap();
  for (ByteArray key : keys) {
                               List<Node> availableNodes = availableNodes(routingStrategy.routeRequest(key.get()));
                               checkRequiredReads(availableNodes);
                               int preferredReads = storeDef.getPreferredReads();
                               List<Node> preferredNodes = Lists.newArrayListWithCapacity(preferredReads);
                               List<Node> extraNodes = Lists.newArrayListWithCapacity(3);
                               for (Node node : availableNodes) {
                                                                  if (preferredNodes.size() < preferredReads)
                                                                    preferredNodes.add(node);
                                                                  else
                                                                    extraNodes.add(node);
                                                                }
                               for (Node node : preferredNodes) {
                                                                  List<ByteArray> nodeKeys = nodeToKeysMap.get(node);
                                                                  if (nodeKeys == null)
                                                                  {
                                                                    nodeKeys = Lists.newArrayList();
                                                                    nodeToKeysMap.put(node, nodeKeys);
                                                                  }
                                                                  nodeKeys.add(key);
                                                                }
                               if (!extraNodes.isEmpty())
                               {
                                 List<Node> nodes = keyToExtraNodesMap.get(key);
                                 if (nodes == null)
                                   keyToExtraNodesMap.put(key, extraNodes);
                                 else
                                   nodes.addAll(extraNodes);
                               }
                             }
  List<Callable<GetAllResult>> callables = Lists.newArrayList();
  for (Map.Entry<Node, List<ByteArray>> entry : nodeToKeysMap.entrySet()) {
                                                                            final Node node = entry.getKey();
                                                                            final Collection<ByteArray> nodeKeys = entry.getValue();
                                                                            if (failureDetector.isAvailable(node))
                                                                              callables.add(new GetAllCallable(node, nodeKeys));
                                                                          }
  List<Throwable> failures = Lists.newArrayList();
  List<NodeValue<ByteArray, byte[]>> nodeValues = Lists.newArrayList();
  Map<ByteArray, MutableInt> keyToSuccessCount = Maps.newHashMap();
  for (ByteArray key : keys) keyToSuccessCount.put(key, new MutableInt(0));
  List<Future<GetAllResult>> futures;
  try
  {
    futures = executor.invokeAll(callables, (timeoutMs * 3), TimeUnit.MILLISECONDS);
  }
  catch (InterruptedException e)
  {
    throw new InsufficientOperationalNodesException("getAll operation interrupted.", e);
  }
  for (Future<GetAllResult> f : futures) {
                                           if (f.isCancelled())
                                           {
                                             logger.warn(("Get operation timed out after " + timeoutMs + " ms."));
                                             continue;
                                           }
                                           try
                                           {
                                             GetAllResult getResult = f.get();
                                             if (getResult.exception != null)
                                             {
                                               failures.add(getResult.exception);
                                               continue;
                                             }
                                             for (ByteArray key : getResult.callable.nodeKeys) {
                                                                                                 List<Versioned<byte[]>> retrieved = getResult.retrieved.get(key);
                                                                                                 MutableInt successCount = keyToSuccessCount.get(key);
                                                                                                 successCount.increment();
                                                                                                 if (retrieved != null)
                                                                                                 {
                                                                                                   List<Versioned<byte[]>> existing = result.get(key);
                                                                                                   if (existing == null)
                                                                                                     result.put(key, Lists.newArrayList(retrieved));
                                                                                                   else
                                                                                                     existing.addAll(retrieved);
                                                                                                 }
                                                                                               }
                                             nodeValues.addAll(getResult.nodeValues);
                                           }
                                           catch (InterruptedException e)
                                           {
                                             throw new InsufficientOperationalNodesException("getAll operation interrupted.", e);
                                           }
                                           catch (ExecutionException e)
                                           {
                                             if (e.getCause() instanceof Error)
                                               throw (Error) e.getCause();
                                             else
                                               logger.error(e.getMessage(), e);
                                           }
                                         }
  for (ByteArray key : keys) {
                               MutableInt successCountWrapper = keyToSuccessCount.get(key);
                               int successCount = successCountWrapper.intValue();
                               if (successCount < storeDef.getPreferredReads())
                               {
                                 List<Node> extraNodes = keyToExtraNodesMap.get(key);
                                 if (extraNodes != null)
                                 {
                                   for (Node node : extraNodes) {
                                                                  try
                                                                  {
                                                                    List<Versioned<byte[]>> values = innerStores.get(node.getId()).get(key);
                                                                    fillRepairReadsValues(nodeValues, key, node, values);
                                                                    List<Versioned<byte[]>> versioneds = result.get(key);
                                                                    if (versioneds == null)
                                                                      result.put(key, Lists.newArrayList(values));
                                                                    else
                                                                      versioneds.addAll(values);
                                                                    failureDetector.recordSuccess(node);
                                                                    if (++successCount >= storeDef.getPreferredReads())
                                                                      break;
                                                                  }
                                                                  catch (UnreachableStoreException e)
                                                                  {
                                                                    failures.add(e);
                                                                    failureDetector.recordException(node, e);
                                                                  }
                                                                  catch (VoldemortApplicationException e)
                                                                  {
                                                                    throw e;
                                                                  }
                                                                  catch (Exception e)
                                                                  {
                                                                    logger.warn(("Error in GET_ALL on node " + node.getId() + "(" + node.getHost() + ")"), e);
                                                                    failures.add(e);
                                                                  }
                                                                }
                                 }
                               }
                               successCountWrapper.setValue(successCount);
                             }
  repairReads(nodeValues);
  for (Map.Entry<ByteArray, MutableInt> mapEntry : keyToSuccessCount.entrySet()) {
                                                                                   int successCount = mapEntry.getValue().intValue();
                                                                                   if (successCount < storeDef.getRequiredReads())
                                                                                     throw new InsufficientOperationalNodesException(this.storeDef.getRequiredReads() + " reads required, but " + successCount + " succeeded.", failures);
                                                                                 }
  return result;
}