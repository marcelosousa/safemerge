private long checkpointInterval = 500;
private int consoleFileCount = 1;
private int consoleFileLimit = -1;
private String consoleFileName = "tm.out";
private AtomikosLoggingLevel consoleLogLevel = AtomikosLoggingLevel.WARN;
private long defaultJtaTimeout = 10000;
private boolean enableLogging = true;
private boolean forceShutdownOnVmExit;
private String logBaseDir;
private String logBaseName = "tmlog";
private int maxActives = 50;
private long maxTimeout = 300000;
private String outputDir;
private boolean serialJtaTransactions = true;
private String service;
private boolean threadedTwoPhaseCommit;
private String transactionManagerUniqueName;
private boolean allowSubTransactions = true;
private final Recovery recovery = new Recovery();
public Properties asProperties ()
{
  Properties properties = new Properties();
  set(properties, "service", getService());
  set(properties, "max_timeout", getMaxTimeout());
  set(properties, "default_jta_timeout", getDefaultJtaTimeout());
  set(properties, "max_actives", getMaxActives());
  set(properties, "enable_logging", isEnableLogging());
  set(properties, "tm_unique_name", getTransactionManagerUniqueName());
  set(properties, "serial_jta_transactions", isSerialJtaTransactions());
  set(properties, "allow_subtransactions", isAllowSubTransactions());
  set(properties, "force_shutdown_on_vm_exit", isForceShutdownOnVmExit());
  set(properties, "log_base_name", getLogBaseName());
  set(properties, "log_base_dir", getLogBaseDir());
  set(properties, "checkpoint_interval", getCheckpointInterval());
  set(properties, "threaded_2pc", isThreadedTwoPhaseCommit());
  Recovery recovery = getRecovery();
  set(properties, "forget_orphaned_log_entries_delay", recovery.getForgetOrphanedLogEntriesDelay());
  set(properties, "recovery_delay", recovery.getDelay());
  set(properties, "oltp_max_retries", recovery.getMaxRetries());
  set(properties, "oltp_retry_interval", recovery.getRetryInterval());
  return properties;
}
public boolean isAllowSubTransactions ()
{
  return this.allowSubTransactions;
}
private void set (Properties properties, String key, Object value)
{
  String id = "com.atomikos.icatch." + key;
  if (value != null && !properties.containsKey(id))
  {
    properties.setProperty(id, value.toString());
  }
}
public AtomikosLoggingLevel getConsoleLogLevel ()
{
  return this.consoleLogLevel;
}
public String getOutputDir ()
{
  return this.outputDir;
}
public String getConsoleFileName ()
{
  return this.consoleFileName;
}
public int getConsoleFileCount ()
{
  return this.consoleFileCount;
}
public int getConsoleFileLimit ()
{
  return this.consoleFileLimit;
}
public boolean isThreadedTwoPhaseCommit ()
{
  return this.threadedTwoPhaseCommit;
}
public Recovery getRecovery ()
{
  return this.recovery;
}
