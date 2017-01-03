{
  final Handler antLogHandler = new AntLogHandler(project);
  final ScopedLogHandlersManager logManager = new ScopedLogHandlersManager(Level.FINEST, antLogHandler);
  try
  {
    doTask();
  }
  finally {
            tryClose(configuration.getClassLoader());
            logManager.close();
          }
}