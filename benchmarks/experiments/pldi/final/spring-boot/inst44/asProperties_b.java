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
  set(properties, "force_shutdown_on_vm_exit", isForceShutdownOnVmExit());
  set(properties, "log_base_name", getLogBaseName());
  set(properties, "log_base_dir", getLogBaseDir());
  set(properties, "checkpoint_interval", getCheckpointInterval());
  set(properties, "threaded_2pc", isThreadedTwoPhaseCommit());
  return properties;
}