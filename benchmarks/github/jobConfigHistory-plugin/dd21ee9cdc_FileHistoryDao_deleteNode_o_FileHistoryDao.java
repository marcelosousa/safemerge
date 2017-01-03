{
  createNewHistoryEntry(node, Messages.ConfigHistoryListenerHelper_DELETED());
  final File currentHistoryDir = getHistoryDirForNode(node);
  final SimpleDateFormat buildDateFormat = new SimpleDateFormat("yyyyMMdd_HHmmss_SSS");
  final String timestamp = buildDateFormat.format(new Date());
  final String deletedHistoryName = node.getNodeName() + JobConfigHistoryConsts.DELETED_MARKER + timestamp;
  final File deletedHistoryDir = new File(currentHistoryDir.getParentFile(), deletedHistoryName);
  if (!currentHistoryDir.renameTo(deletedHistoryDir))
  {
    LOG.log(Level.WARNING, "unable to rename deleted history dir to: {0}", deletedHistoryDir);
  }
}