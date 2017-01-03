{
  Rule rule = rv.getRule();
  CodeClimateIssue issue = new CodeClimateIssue();
  issue.check_name = rule.getName();
  issue.description = rv.getDescription();
  issue.content = new CodeClimateIssue.Content(rule.getDescription());
  issue.location = getLocation(rv);
  switch (rule.getPriority())
  {
    case HIGH:
      issue.severity = "critical";
      break;
    case MEDIUM_HIGH:
    case MEDIUM:
    case MEDIUM_LOW:
      issue.severity = "normal";
      break;
    case LOW:
      issue.severity = "info";
      break;
  }
  issue.remediation_points = REMEDIATION_POINTS_DEFAULT;
  if (rule.hasDescriptor(CODECLIMATE_REMEDIATION_MULTIPLIER))
  {
    issue.remediation_points *= rule.getProperty(CODECLIMATE_REMEDIATION_MULTIPLIER);
  }
  if (rule.hasDescriptor(CODECLIMATE_CATEGORIES))
  {
    issue.categories = rule.getProperty(CODECLIMATE_CATEGORIES);
  }
  else
  {
    issue.categories = CODECLIMATE_DEFAULT_CATEGORIES;
  }
  return issue;
}