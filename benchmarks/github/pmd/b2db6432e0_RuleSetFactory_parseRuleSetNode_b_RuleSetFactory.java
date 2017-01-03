{
  if (!ruleSetReferenceId.isExternal())
  {
    throw new IllegalArgumentException("Cannot parse a RuleSet from a non-external reference: <" + ruleSetReferenceId + ">.");
  }
  try
  {
    DocumentBuilder builder = DocumentBuilderFactory.newInstance().newDocumentBuilder();
    Document document = builder.parse(inputStream);
    Element ruleSetElement = document.getDocumentElement();
    RuleSet ruleSet = new RuleSet();
    ruleSet.setFileName(ruleSetReferenceId.getRuleSetFileName());
    ruleSet.setName(ruleSetElement.getAttribute("name"));
    NodeList nodeList = ruleSetElement.getChildNodes();
    for (int i = 0 ; i < nodeList.getLength() ; i++)
    {
      Node node = nodeList.item(i);
      if (node.getNodeType() == Node.ELEMENT_NODE)
      {
        String nodeName = node.getNodeName();
        if (DESCRIPTION.equals(nodeName))
        {
          ruleSet.setDescription(parseTextNode(node));
        }
        else
          if ("include-pattern".equals(nodeName))
          {
            ruleSet.addIncludePattern(parseTextNode(node));
          }
          else
            if ("exclude-pattern".equals(nodeName))
            {
              ruleSet.addExcludePattern(parseTextNode(node));
            }
            else
              if ("rule".equals(nodeName))
              {
                parseRuleNode(ruleSetReferenceId, ruleSet, node, withDeprecatedRuleReferences);
              }
              else
              {
                throw new IllegalArgumentException(UNEXPECTED_ELEMENT + node.getNodeName() + "> encountered as child of <ruleset> element.");
              }
      }
    }
    return ruleSet;
  }
  catch (ClassNotFoundException cnfe)
  {
    return classNotFoundProblem(cnfe);
  }
  catch (InstantiationException ie)
  {
    return classNotFoundProblem(ie);
  }
  catch (IllegalAccessException iae)
  {
    return classNotFoundProblem(iae);
  }
  catch (ParserConfigurationException pce)
  {
    return classNotFoundProblem(pce);
  }
  catch (RuleSetNotFoundException rsnfe)
  {
    return classNotFoundProblem(rsnfe);
  }
  catch (IOException ioe)
  {
    return classNotFoundProblem(ioe);
  }
  catch (SAXException se)
  {
    return classNotFoundProblem(se);
  }
}