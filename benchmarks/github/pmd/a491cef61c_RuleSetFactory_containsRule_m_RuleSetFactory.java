{
  boolean found = false;
  InputStream ruleset = null;
  try
  {
    ruleset = ruleSetReferenceId.getInputStream(classLoader);
    DocumentBuilder builder = DocumentBuilderFactory.newInstance().newDocumentBuilder();
    Document document = builder.parse(ruleset);
    Element ruleSetElement = document.getDocumentElement();
    NodeList rules = ruleSetElement.getElementsByTagName("rule");
    for (int i = 0 ; i < rules.getLength() ; i++)
    {
      Element rule = (Element) rules.item(i);
      if (rule.hasAttribute("name") && rule.getAttribute("name").equals(ruleName))
      {
        found = true;
        break;
      }
    }
  }
  catch (Exception e)
  {
    throw new RuntimeException(e);
  }
  finally {
            IOUtils.closeQuietly(ruleset);
          }
  return found;
}