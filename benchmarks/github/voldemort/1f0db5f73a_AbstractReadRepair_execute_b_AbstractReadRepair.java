{
  insertNodeValues();
  if ((nodeValues.size() > 1 && preferred) > 1)
  {
    List<NodeValue<ByteArray, byte[]>> toReadRepair = Lists.newArrayList();
    for (NodeValue<ByteArray, byte[]> v : readRepairer.getRepairs(nodeValues)) {
                                                                                 Versioned<byte[]> versioned = Versioned.value(v.getVersioned().getValue(), ((VectorClock) v.getVersion()).clone());
                                                                                 toReadRepair.add(new NodeValue<ByteArray, byte[]>(v.getNodeId(), v.getKey(), versioned));
                                                                               }
    for (NodeValue<ByteArray, byte[]> v : toReadRepair) {
                                                          try
                                                          {
                                                            if (logger.isDebugEnabled())
                                                              logger.debug(("Doing read repair on node " + v.getNodeId() + " for key '" + v.getKey() + "' with version " + v.getVersion() + "."));
                                                            NonblockingStore store = nonblockingStores.get(v.getNodeId());
                                                            store.submitPutRequest(v.getKey(), v.getVersioned(), null, timeoutMs);
                                                          }
                                                          catch (VoldemortApplicationException e)
                                                          {
                                                            if (logger.isDebugEnabled())
                                                              logger.debug(("Read repair cancelled due to application level exception on node " + v.getNodeId() + " for key '" + v.getKey() + "' with version " + v.getVersion() + ": " + e.getMessage()));
                                                          }
                                                          catch (Exception e)
                                                          {
                                                            logger.debug("Read repair failed: ", e);
                                                          }
                                                        }
  }
  pipeline.addEvent(completeEvent);
}