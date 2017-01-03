{
  fileModificationLock.writeLock().lock();
  try
  {
    if (isOpen)
      throw new IllegalStateException("Attempt to open already open store.");
    if (versionDir == null)
    {
      versionDir = getCurrentVersion();
      if (versionDir == null)
        versionDir = new File(storeDir, "version-0");
    }
    long versionId = ReadOnlyUtils.getVersionId(versionDir);
    if (versionId == -1)
    {
      throw new VoldemortException("Unable to parse id from version directory " + versionDir.getAbsolutePath());
    }
    currentVersionId = versionId;
    Utils.mkdirs(versionDir);
    logger.info(("Creating symbolic link for '" + getName() + "' using directory " + versionDir.getAbsolutePath()));
    Utils.symlink(versionDir.getAbsolutePath(), (storeDir.getAbsolutePath() + File.separator + "latest"));
    this.fileSet = new ChunkedFileSet(versionDir, routingStrategy, nodeId);
    this.lastSwapped = System.currentTimeMillis();
    this.isOpen = true;
  }
  finally {
            fileModificationLock.writeLock().unlock();
          }
}