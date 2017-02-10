class SlopPusherJob{ 
 void run() {
  logger.debug("Pushing slop...");
  int slopsPushed = 0;
  int attemptedPushes = 0;
  ClosableIterator<Pair<ByteArray, Versioned<Slop>>> iterator = null;
  try
  {
    StorageEngine<ByteArray, Slop> slopStore = storeRepo.getSlopStore();
    iterator = slopStore.entries();
    while (iterator.hasNext())
    {
      if (Thread.interrupted())
        throw new InterruptedException("Task cancelled!");
      attemptedPushes++;
      try
      {
        Pair<ByteArray, Versioned<Slop>> keyAndVal = iterator.next();
        Versioned<Slop> versioned = keyAndVal.getSecond();
        Slop slop = versioned.getValue();
        Store<ByteArray, byte[]> store = storeRepo.getNodeStore(slop.getStoreName(), slop.getNodeId());
        try
        {
          if (slop.getOperation() == Operation.PUT)
            store.put(keyAndVal.getFirst(), new Versioned<byte[]>(slop.getValue(), versioned.getVersion()));
          else
            if (slop.getOperation() == Operation.DELETE)
              store.delete(keyAndVal.getFirst(), versioned.getVersion());
            else
              logger.error(("Unknown slop operation: " + slop.getOperation()));
          slopStore.delete(slop.makeKey(), versioned.getVersion());
          slopsPushed++;
        }
        catch (ObsoleteVersionException e)
        {
          slopStore.delete(slop.makeKey(), versioned.getVersion());
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
}