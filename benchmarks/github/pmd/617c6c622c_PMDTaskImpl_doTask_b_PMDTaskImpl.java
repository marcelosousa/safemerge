{
  setupClassLoader();
  RuleSetFactory ruleSetFactory = RulesetsFactoryUtils.getRulesetFactory(configuration);
  try
  {
    String ruleSets = configuration.getRuleSets();
    if (StringUtil.isNotEmpty(ruleSets))
    {
      configuration.setRuleSets(project.replaceProperties(ruleSets));
    }
    RuleSets rules = ruleSetFactory.createRuleSets(configuration.getRuleSets());
    logRulesUsed(rules);
  }
  catch (RuleSetNotFoundException e)
  {
    throw new BuildException(e.getMessage(), e);
  }
  if (configuration.getSuppressMarker() != null)
  {
    project.log(("Setting suppress marker to be " + configuration.getSuppressMarker()), Project.MSG_VERBOSE);
  }
  for (Formatter formatter : formatters) {
                                           project.log(("Sending a report to " + formatter), Project.MSG_VERBOSE);
                                           formatter.start(project.getBaseDir().toString());
                                         }
  RuleContext ctx = new RuleContext();
  Report errorReport = new Report();
  final AtomicInteger reportSize = new AtomicInteger();
  final String separator = System.getProperty("file.separator");
  for (FileSet fs : filesets) {
                                List<DataSource> files = new LinkedList();
                                DirectoryScanner ds = fs.getDirectoryScanner(project);
                                String[] srcFiles = ds.getIncludedFiles();
                                for (String srcFile : srcFiles) {
                                                                  File file = new File(ds.getBasedir() + separator + srcFile);
                                                                  files.add(new FileDataSource(file));
                                                                }
                                final String inputPaths = ds.getBasedir().getPath();
                                configuration.setInputPaths(inputPaths);
                                Renderer logRenderer = new AbstractRenderer("log", "Logging renderer")
                                                       {
                                                         public void start ()
                                                         {
                                                         }
                                                         public void startFileAnalysis (DataSource dataSource)
                                                         {
                                                           project.log(("Processing file " + dataSource.getNiceFileName(false, inputPaths)), Project.MSG_VERBOSE);
                                                         }
                                                         public void renderFileReport (Report r)
                                                         {
                                                           int size = r.size();
                                                           if (size > 0)
                                                           {
                                                             reportSize.addAndGet(size);
                                                           }
                                                         }
                                                         public void end ()
                                                         {
                                                         }
                                                         public String defaultFileExtension ()
                                                         {
                                                           return null;
                                                         }
                                                       };
                                List<Renderer> renderers = new ArrayList(formatters.size() + 1);
                                renderers.add(logRenderer);
                                for (Formatter formatter : formatters) {
                                                                         renderers.add(formatter.getRenderer());
                                                                       }
                                try
                                {
                                  PMD.processFiles(configuration, ruleSetFactory, files, ctx, renderers);
                                }
                                catch (RuntimeException pmde)
                                {
                                  handleError(ctx, errorReport, pmde);
                                }
                              }
  int problemCount = reportSize.get();
  project.log((problemCount + " problems found"), Project.MSG_VERBOSE);
  for (Formatter formatter : formatters) {
                                           formatter.end(errorReport);
                                         }
  if ((failuresPropertyName != null && problemCount) > 0)
  {
    project.setProperty(failuresPropertyName, String.valueOf(problemCount));
    project.log(("Setting property " + failuresPropertyName + " to " + problemCount), Project.MSG_VERBOSE);
  }
  if ((failOnRuleViolation && problemCount) > maxRuleViolations)
  {
    throw new BuildException("Stopping build since PMD found " + problemCount + " rule violations in the code");
  }
}