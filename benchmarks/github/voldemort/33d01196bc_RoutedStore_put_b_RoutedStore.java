{
  long startNs = System.nanoTime();
  StoreUtils.assertValidKey(key);
  final List<Node> nodes = availableNodes(routingStrategy.routeRequest(key.get()));
  final int numNodes = nodes.size();
  if (numNodes < this.storeDef.getRequiredWrites())
    throw new InsufficientOperationalNodesException("Only " + numNodes + " nodes in preference list, but " + this.storeDef.getRequiredWrites() + " writes required.");
  final AtomicInteger successes = new AtomicInteger(0);
  final List<Exception> failures = Collections.synchronizedList(new ArrayList<Exception>(1));
  Node master = null;
  int currentNode = 0;
  Versioned<byte[]> versionedCopy = null;
  for (; currentNode < numNodes ; currentNode++)
  {
    Node current = nodes.get(currentNode);
    try
    {
      versionedCopy = incremented(versioned, current.getId());
      innerStores.get(current.getId()).put(key, versionedCopy);
      successes.getAndIncrement();
      current.getStatus().setAvailable();
      master = current;
      break;
    }
    catch (UnreachableStoreException e)
    {
      markUnavailable(current, e);
      failures.add(e);
    }
    catch (ObsoleteVersionException e)
    {
      throw e;
    }
    catch (Exception e)
    {
      failures.add(e);
    }
  }
  if (successes.get() < 1)
    throw new InsufficientOperationalNodesException("No master node succeeded!", failures.size() > 0 ? failures.get(0) : null);
  else
    currentNode++;
  final Versioned<byte[]> finalVersionedCopy = versionedCopy;
  final Semaphore semaphore = new Semaphore(0, false);
  int attempts = 0;
  for (; currentNode < numNodes ; currentNode++)
  {
    attempts++;
    final Node node = nodes.get(currentNode);
    this.executor.execute(new Runnable()
                          {
                            public void run ()
                            {
                              try
                              {
                                innerStores.get(node.getId()).put(key, finalVersionedCopy);
                                successes.incrementAndGet();
                                node.getStatus().setAvailable();
                              }
                              catch (UnreachableStoreException e)
                              {
                                markUnavailable(node, e);
                                failures.add(e);
                              }
                              catch (ObsoleteVersionException e)
                              {
                              }
                              catch (Exception e)
                              {
                                logger.warn(("Error in PUT on node " + node.getId() + "(" + node.getHost() + ")"), e);
                                failures.add(e);
                              }
                              finally {
                                        semaphore.release();
                                      }
                            }
                          });
  }
  int blockCount = Math.min((storeDef.getPreferredWrites() - 1), attempts);
  boolean noTimeout = blockOnPut(startNs, semaphore, 0, blockCount, successes, storeDef.getPreferredWrites());
  if (successes.get() < storeDef.getRequiredWrites())
  {
    if (noTimeout)
    {
      int startingIndex = blockCount - 1;
      blockCount = Math.max((storeDef.getPreferredWrites() - 1), attempts);
      blockOnPut(startNs, semaphore, startingIndex, blockCount, successes, storeDef.getRequiredWrites());
    }
    if (successes.get() < storeDef.getRequiredWrites())
      throw new InsufficientOperationalNodesException(successes.get() + " writes succeeded, but " + this.storeDef.getRequiredWrites() + " are required.", failures);
  }
  VectorClock versionedClock = (VectorClock) versioned.getVersion();
  versionedClock.incrementVersion(master.getId(), time.getMilliseconds());
}