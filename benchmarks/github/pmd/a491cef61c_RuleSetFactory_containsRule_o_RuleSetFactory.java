{
  boolean found = false;
  try
  {
    DocumentBuilder builder = DocumentBuilderFactory.newInstance().newDocumentBuilder();
    Document document = builder.parse(ruleSetReferenceId.getInputStream(classLoader));
    Element ruleSetElement = document.getDocumentElement();
    NodeList rules = ruleSetElement.getElementsByTagName("rule");
    for (int i = 0 ; i < rules.getLength() ; i++)
    {
      Element rule = (Element) rules.item(i);
      if (rule.hasAttribute("name"))
      {
        if (rule.getAttribute("name").equals(ruleName))
        {
          found = true;
          break;
        }
      }
    }
  }
  catch (Exception e)
  {
    throw new RuntimeException(e);
  }
  return found;
}