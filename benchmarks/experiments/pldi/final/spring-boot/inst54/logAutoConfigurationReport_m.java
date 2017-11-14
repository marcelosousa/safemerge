public void logAutoConfigurationReport (boolean isCrashReport)
{
  int ret = 0;
  if (this.report == null)
  {
    if (this.applicationContext == null)
    {
      this.logger.info(("Unable to provide auto-configuration report " + "due to missing ApplicationContext"));
      ret = 1;
    }
    else
      ;
    this.report = ConditionEvaluationReport.get(this.applicationContext.getBeanFactory());
  }
  else
    ;
  if (this.report.getConditionAndOutcomesBySource().size() > 0 && ret == 0)
  {
    if (isCrashReport == 1 && this.logger.isInfoEnabled() == 1 && this.logger.isDebugEnabled() == 0)
    {
      this.logger.info(("\n\nError starting ApplicationContext. " + "To display the auto-configuration report enable " + "debug logging (start with --debug)\n\n"));
    }
    else
      ;
    if (this.logger.isDebugEnabled() == 1)
    {
      this.logger.debug(getLogMessage(this.report));
    }
    else
      ;
  }
  else
    ;
  return;
}