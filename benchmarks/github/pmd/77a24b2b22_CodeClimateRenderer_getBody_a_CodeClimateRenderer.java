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
    result += "\\n\\n### [PMD properties](" + pmdDeveloperUrl + ")\\n\\n";
    result += "Name | Value | Description\\n";
    result += "--- | --- | ---\\n";
    for (PropertyDescriptor<?> property : rule.getPropertyDescriptors()) {
                                                                           @SuppressWarnings("unchecked")
                                                                           PropertyDescriptor<T> typed = (PropertyDescriptor<T>) property;
                                                                           T value = rule.getProperty(typed);
                                                                           String propertyValue = typed.asDelimitedString(value);
                                                                           if (propertyValue == null)
                                                                             propertyValue = "";
                                                                           propertyValue = propertyValue.replaceAll("(\n|\r\n|\r)", "\\\\n");
                                                                           String porpertyName = property.name();
                                                                           porpertyName = porpertyName.replaceAll("\\_", "\\\\_");
                                                                           result += porpertyName + " | " + propertyValue + " | " + property.description() + "\\n";
                                                                         }
  }
  return result;
}