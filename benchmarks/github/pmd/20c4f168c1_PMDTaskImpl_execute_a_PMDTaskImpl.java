{
  final Handler antLogHandler = new AntLogHandler(project);
  final ScopedLogHandlersManager logManager = new ScopedLogHandlersManager(Level.FINEST, antLogHandler);
  try
  {
    doTask();
  }
  finally {
            logManager.close();
            IOUtil.tryCloseClassLoader(configuration.getClassLoader());
          }
}