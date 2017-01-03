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
      setStatusCodeOrExit(1);
      return;
    }
  }
  catch (ParameterException e)
  {
    jcommander.usage();
    System.out.println(buildUsageText());
    System.err.println((" " + e.getMessage()));
    setStatusCodeOrExit(1);
    return;
  }
  arguments.postContruct();
  CPDConfiguration.setSystemProperties(arguments);
  CPD cpd = new CPD(arguments);
  if (null != arguments.getFiles() && !arguments.getFiles().isEmpty())
  {
    addSourcesFilesToCPD(arguments.getFiles(), cpd, (!arguments.isNonRecursive()));
  }
  if (null != arguments.getURI() && !"".equals(arguments.getURI()))
  {
    addSourceURIToCPD(arguments.getURI(), cpd);
  }
  cpd.go();
  System.out.println(arguments.getRenderer().render(cpd.getMatches()));
  if (cpd.getMatches().hasNext())
  {
    setStatusCodeOrExit(DUPLICATE_CODE_FOUND);
  }
}