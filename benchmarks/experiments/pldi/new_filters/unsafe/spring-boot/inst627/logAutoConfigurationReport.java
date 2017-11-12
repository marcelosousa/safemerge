private ConfigurableApplicationContext applicationContext;
private final Log logger = LogFactory.getLog(getClass());
private ConditionEvaluationReport report;
public void logAutoConfigurationReport (boolean isCrashReport)
{
  if (this.report == null)
  {
    if (this.applicationContext == null)
    {
      this.logger.info(("Unable to provide auto-configuration report " + "due to missing ApplicationContext"));
      return;
    }
    else
      ;
    this.report = ConditionEvaluationReport.get(this.applicationContext.getBeanFactory());
  }
  else
    ;
  if (this.report.getConditionAndOutcomesBySource().size() > 0)
  {
    if (isCrashReport && this.logger.isInfoEnabled() && !this.logger.isDebugEnabled())
    {
      this.logger.info(("\n\nError starting ApplicationContext. " + "To display the auto-configuration report enable " + "debug logging (start with --debug)\n\n"));
    }
    else
      ;
    if (this.logger.isDebugEnabled())
    {
      this.logger.debug(getLogMessage(this.report));
    }
    else
      ;
  }
  else
    ;
}
private StringBuilder getLogMessage (ConditionEvaluationReport report)
{
  StringBuilder message = new StringBuilder();
  message.append("\n\n\n");
  message.append("=========================\n");
  message.append("AUTO-CONFIGURATION REPORT\n");
  message.append("=========================\n\n\n");
  message.append("Positive matches:\n");
  message.append("-----------------\n");
  Map<String, ConditionAndOutcomes> shortOutcomes = orderByName(report.getConditionAndOutcomesBySource());
  for (Map.Entry<String, ConditionAndOutcomes> entry : shortOutcomes.entrySet()) {
                                                                                   if (entry.getValue().isFullMatch())
                                                                                   {
                                                                                     addLogMessage(message, entry.getKey(), entry.getValue());
                                                                                   }
                                                                                 }
  message.append("\n\n");
  message.append("Negative matches:\n");
  message.append("-----------------\n");
  for (Map.Entry<String, ConditionAndOutcomes> entry : shortOutcomes.entrySet()) {
                                                                                   if (!entry.getValue().isFullMatch())
                                                                                   {
                                                                                     addLogMessage(message, entry.getKey(), entry.getValue());
                                                                                   }
                                                                                 }
  message.append("\n\n");
  message.append("Exclusions:\n");
  message.append("-----------\n");
  if (report.getExclusions().isEmpty())
  {
    message.append("\n    None\n");
  }
  else
  {
    for (String exclusion : report.getExclusions()) {
                                                      message.append(("\n   " + exclusion + "\n"));
                                                    }
  }
  message.append("\n\n");
  return message;
}
private StringBuilder getLogMessage (Map<String, ConditionAndOutcomes> outcomes)
{
  StringBuilder message = new StringBuilder();
  message.append("\n\n\n");
  message.append("=========================\n");
  message.append("AUTO-CONFIGURATION REPORT\n");
  message.append("=========================\n\n\n");
  message.append("Positive matches:\n");
  message.append("-----------------\n");
  Map<String, ConditionAndOutcomes> shortOutcomes = orderByName(outcomes);
  for (Map.Entry<String, ConditionAndOutcomes> entry : shortOutcomes.entrySet()) {
                                                                                   if (entry.getValue().isFullMatch())
                                                                                   {
                                                                                     addLogMessage(message, entry.getKey(), entry.getValue());
                                                                                   }
                                                                                 }
  message.append("\n\n");
  message.append("Negative matches:\n");
  message.append("-----------------\n");
  for (Map.Entry<String, ConditionAndOutcomes> entry : shortOutcomes.entrySet()) {
                                                                                   if (!entry.getValue().isFullMatch())
                                                                                   {
                                                                                     addLogMessage(message, entry.getKey(), entry.getValue());
                                                                                   }
                                                                                 }
  message.append("\n\n");
  return message;
}
