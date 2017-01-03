{
  StoreUtils.assertValidKey(key);
  final List<Node> nodes = availableNodes(routingStrategy.routeRequest(key.get()));
  final int numNodes = nodes.size();
  if (numNodes < this.storeDef.getRequiredWrites())
    throw new InsufficientOperationalNodesException("Only " + numNodes + " nodes in preference list, but " + this.storeDef.getRequiredWrites() + " writes required.");
  final AtomicInteger successes = new AtomicInteger(0);
  final AtomicBoolean deletedSomething = new AtomicBoolean(false);
  final List<Exception> failures = Collections.synchronizedList(new LinkedList<Exception>());
  final Semaphore semaphore = new Semaphore(0, false);
  for (final Node node : nodes) {
                                  this.executor.execute(new Runnable()
                                                        {
                                                          public void run ()
                                                          {
                                                            try
                                                            {
                                                              boolean deleted = innerStores.get(node.getId()).delete(key, version);
                                                              successes.incrementAndGet();
                                                              deletedSomething.compareAndSet(false, deleted);
                                                              node.getStatus().setAvailable();
                                                            }
                                                            catch (UnreachableStoreException e)
                                                            {
                                                              failures.add(e);
                                                              markUnavailable(node, e);
                                                            }
                                                            catch (Exception e)
                                                            {
                                                              failures.add(e);
                                                              logger.warn(("Error in DELETE on node " + node.getId() + "(" + node.getHost() + ")"), e);
                                                            }
                                                            finally {
                                                                      semaphore.release();
                                                                    }
                                                          }
                                                        });
                                }
  int attempts = Math.min(storeDef.getPreferredWrites(), numNodes);
  if (this.storeDef.getPreferredWrites() <= 0)
  {
    return true;
  }
  else
  {
    for (int i = 0 ; i < numNodes ; i++)
    {
      try
      {
        boolean acquired = semaphore.tryAcquire(timeoutMs, TimeUnit.MILLISECONDS);
        if (!acquired)
          logger.warn(("Delete operation timed out waiting for operation " + i + " to complete after waiting " + timeoutMs + " ms."));
        if (successes.get() >= attempts)
          return deletedSomething.get();
      }
      catch (InterruptedException e)
      {
        throw new InsufficientOperationalNodesException("Delete operation interrupted!", e);
      }
    }
  }
  if (successes.get() < storeDef.getRequiredWrites())
    throw new InsufficientOperationalNodesException(this.storeDef.getRequiredWrites() + " deletes required, but " + successes.get() + " succeeded.", failures);
  else
    return deletedSomething.get();
}