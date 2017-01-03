{
  logger.debug("Pushing slop...");
  int slopsPushed = 0;
  int attemptedPushes = 0;
  ClosableIterator<Pair<ByteArray, Versioned<Slop>>> iterator = null;
  try
  {
    StorageEngine<ByteArray, Slop> slopStore = storeRepo.getSlopStore();
    EventThrottler throttler = new EventThrottler(maxWriteBytesPerSec);
    iterator = slopStore.entries();
    while (iterator.hasNext())
    {
      if (Thread.interrupted())
        throw new InterruptedException("Task cancelled!");
      attemptedPushes++;
      if (attemptedPushes % 1000 == 0)
        logger.info(("Attempted pushing " + attemptedPushes + " slops"));
      try
      {
        Pair<ByteArray, Versioned<Slop>> keyAndVal = iterator.next();
        Versioned<Slop> versioned = keyAndVal.getSecond();
        Slop slop = versioned.getValue();
        Node node = cluster.getNodeById(slop.getNodeId());
        if (failureDetector.isAvailable(node))
        {
          Store<ByteArray, byte[]> store = storeRepo.getNodeStore(slop.getStoreName(), node.getId());
          Long startNs = System.nanoTime();
          try
          {
            int nBytes = slop.getKey().length();
            if (slop.getOperation() == Operation.PUT)
            {
              store.put(slop.getKey(), new Versioned<byte[]>(slop.getValue(), versioned.getVersion()));
              nBytes += slop.getValue().length + ((VectorClock) versioned.getVersion()).sizeInBytes() + 1;
            }
            else
              if (slop.getOperation() == Operation.DELETE)
                store.delete(slop.getKey(), versioned.getVersion());
              else
                logger.error(("Unknown slop operation: " + slop.getOperation()));
            failureDetector.recordSuccess(node, deltaMs(startNs));
            slopStore.delete(slop.makeKey(), versioned.getVersion());
            slopsPushed++;
            throttler.maybeThrottle(nBytes);
          }
          catch (ObsoleteVersionException e)
          {
            slopStore.delete(slop.makeKey(), versioned.getVersion());
          }
          catch (UnreachableStoreException e)
          {
            failureDetector.recordException(node, deltaMs(startNs), e);
          }
        }
      }
      catch (Exception e)
      {
        logger.error(e);
      }
    }
  }
  catch (Exception e)
  {
    logger.error(e);
  }
  finally {
            try
            {
              if (iterator != null)
                iterator.close();
            }
            catch (Exception e)
            {
              logger.error("Failed to close iterator.", e);
            }
          }
  logger.log((attemptedPushes > 0 ? Level.INFO : Level.DEBUG), ("Attempted " + attemptedPushes + " hinted handoff pushes of which " + slopsPushed + " succeeded."));
}