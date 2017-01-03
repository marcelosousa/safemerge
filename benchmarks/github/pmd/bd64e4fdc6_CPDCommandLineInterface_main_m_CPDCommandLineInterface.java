{
  CPDConfiguration arguments = new CPDConfiguration();
  JCommander jcommander = new JCommander(arguments);
  jcommander.setProgramName(PROGRAM_NAME);
  try
  {
    jcommander.parse(args);
    if (arguments.isHelp())
    {
      jcommander.usage();
      System.out.println(buildUsageText());
      setStatusCodeOrExit(ERROR_STATUS);
      return;
    }
  }
  catch (ParameterException e)
  {
    jcommander.usage();
    System.out.println(buildUsageText());
    System.err.println((" " + e.getMessage()));
    setStatusCodeOrExit(ERROR_STATUS);
    return;
  }
  arguments.postContruct();
  CPDConfiguration.setSystemProperties(arguments);
  CPD cpd = new CPD(arguments);
  try
  {
    if (null != arguments.getFiles() && !arguments.getFiles().isEmpty())
    {
      addSourcesFilesToCPD(arguments.getFiles(), cpd, (!arguments.isNonRecursive()));
    }
    if (null != arguments.getURI() && !"".equals(arguments.getURI()))
    {
      addSourceURIToCPD(arguments.getURI(), cpd);
    }
    cpd.go();
    if (cpd.getMatches().hasNext())
    {
      System.out.println(arguments.getRenderer().render(cpd.getMatches()));
      if (arguments.isFailOnViolation())
      {
        setStatusCodeOrExit(DUPLICATE_CODE_FOUND);
      }
      else
      {
        setStatusCodeOrExit(0);
      }
    }
    else
    {
      setStatusCodeOrExit(0);
    }
  }
  catch (RuntimeException e)
  {
    e.printStackTrace();
    setStatusCodeOrExit(ERROR_STATUS);
  }
}