class ReadOnlyStorageEngine{ 
 void get() {
  StoreUtils.assertValidKey(key);
  byte[] keyMd5 = ByteUtils.md5(key.get());
  int chunk = fileSet.getChunkForKey(keyMd5);
  int location = searchStrategy.indexOf(fileSet.indexFileFor(chunk), keyMd5, fileSet.getIndexFileSize(chunk));
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
}