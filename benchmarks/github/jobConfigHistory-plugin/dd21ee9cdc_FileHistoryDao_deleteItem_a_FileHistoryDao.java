{
  final AbstractItem aItem = (AbstractItem) item;
  createNewHistoryEntry(aItem.getConfigFile(), Messages.ConfigHistoryListenerHelper_DELETED(), null, null);
  final File configFile = aItem.getConfigFile().getFile();
  final File currentHistoryDir = getHistoryDir(configFile);
  final SimpleDateFormat buildDateFormat = new SimpleDateFormat("yyyyMMdd_HHmmss_SSS");
  final String timestamp = buildDateFormat.format(new Date());
  final String deletedHistoryName = item.getName() + JobConfigHistoryConsts.DELETED_MARKER + timestamp;
  final File deletedHistoryDir = new File(currentHistoryDir.getParentFile(), deletedHistoryName);
  if (!currentHistoryDir.renameTo(deletedHistoryDir))
  {
    LOG.log(Level.WARNING, "unable to rename deleted history dir to: {0}", deletedHistoryDir);
  }
}