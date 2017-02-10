class StorageService{ 
 void scheduleCleanupJob() {
  GregorianCalendar cal = new GregorianCalendar();
  cal.add(Calendar.DAY_OF_YEAR, 1);
  cal.set(Calendar.HOUR, voldemortConfig.getRetentionCleanupFirstStartTimeInHour());
  cal.set(Calendar.MINUTE, 0);
  cal.set(Calendar.SECOND, 0);
  cal.set(Calendar.MILLISECOND, 0);
  Date startTime = cal.getTime();
  int maxReadRate = storeDef.hasRetentionScanThrottleRate() ? storeDef.getRetentionScanThrottleRate() : Integer.MAX_VALUE;
  logger.info(("Scheduling data retention cleanup job for store '" + storeDef.getName() + "' at " + startTime + " with retention scan throttle rate:" + maxReadRate + " Entries/second."));
  EventThrottler throttler = new EventThrottler(maxReadRate);
  Runnable cleanupJob = new DataCleanupJob<ByteArray, byte[]>(engine, cleanupPermits, storeDef.getRetentionDays() * Time.MS_PER_DAY, SystemTime.INSTANCE, throttler);
  this.scheduler.schedule(cleanupJob, startTime, (voldemortConfig.getRetentionCleanupScheduledPeriodInHour() * Time.MS_PER_HOUR));
}
}