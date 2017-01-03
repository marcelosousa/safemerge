{
  String result = "## " + rule.getName() + "\\n\\n" + "Since: PMD " + rule.getSince() + "\\n\\n" + "Priority: " + rule.getPriority() + "\\n\\n" + "[Categories](https://github.com/codeclimate/spec/blob/master/SPEC.md#categories): " + Arrays.toString(getCategories()).replaceAll("[\\[\\]]", "") + "\\n\\n" + "[Remediation Points](https://github.com/codeclimate/spec/blob/master/SPEC.md#remediation-points): " + getRemediationPoints() + "\\n\\n" + cleaned(rule.getDescription());
  if (!rule.getExamples().isEmpty())
  {
    result += "\\n\\n### Example:\\n\\n";
    for (String snippet : rule.getExamples()) {
                                                snippet = snippet.replaceAll("\\n", "\\\\n");
                                                snippet = snippet.replaceAll("\\t", "\\\\t");
                                                result += "```java\\n" + snippet + "\\n```  ";
                                              }
  }
  if (!rule.getPropertyDescriptors().isEmpty())
  {
    result += "\\n\\n### [PMD properties](http://pmd.github.io/pmd-5.1.3/pmd-developer.html)\\n\\n";
    result += "Name | Value | Description\\n";
    result += "--- | --- | ---\\n";
    for (PropertyDescriptor<?> property : rule.getPropertyDescriptors()) {
                                                                           String propertyValue;
                                                                           try
                                                                           {
                                                                             propertyValue = Arrays.toString(((String[]) rule.getProperty(property))).replaceAll("[\\[\\]]", "");
                                                                           }
                                                                           catch (Exception ignore)
                                                                           {
                                                                             propertyValue = rule.getProperty(property).toString();
                                                                           }
                                                                           String propertyName = property.name();
                                                                           propertyName = propertyName.replaceAll("\\_", "\\\\_");
                                                                           result += propertyName + " | " + propertyValue + " | " + property.description() + "\\n";
                                                                         }
  }
  return cleaned(result);
}