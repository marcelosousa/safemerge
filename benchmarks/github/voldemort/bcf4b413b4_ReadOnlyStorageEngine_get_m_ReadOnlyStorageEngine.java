{
  StoreUtils.assertValidKey(key);
  try
  {
    fileModificationLock.readLock().lock();
    int chunk = fileSet.getChunkForKey(key.get());
    int location = searchStrategy.indexOf(fileSet.indexFileFor(chunk), ByteUtils.md5(key.get()), fileSet.getIndexFileSize(chunk));
    if (location >= 0)
    {
      byte[] value = readValue(chunk, location);
      return Collections.singletonList(Versioned.value(value));
    }
    else
    {
      return Collections.emptyList();
    }
  }
  finally {
            fileModificationLock.readLock().unlock();
          }
}