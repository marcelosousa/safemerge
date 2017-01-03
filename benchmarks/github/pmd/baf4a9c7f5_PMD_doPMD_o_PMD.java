{
  RuleSetFactory ruleSetFactory = RulesetsFactoryUtils.getRulesetFactory(configuration);
  RuleSets ruleSets = RulesetsFactoryUtils.getRuleSetsWithBenchmark(configuration.getRuleSets(), configuration.getPmdRuleSets(), ruleSetFactory);
  configuration.setPmdRuleSets(ruleSets);
  if (ruleSets == null)
  {
    return 0;
  }
  Set<Language> languages = getApplicableLanguages(configuration, ruleSets);
  List<DataSource> files = getApplicableFiles(configuration, languages);
  long reportStart = System.nanoTime();
  try
  {
    Renderer renderer = configuration.createRenderer();
    List<Renderer> renderers = new LinkedList<Renderer>();
    renderers.add(renderer);
    renderer.setWriter(IOUtil.createWriter(configuration.getReportFile()));
    renderer.start();
    Benchmarker.mark(Benchmark.Reporting, (System.nanoTime() - reportStart), 0);
    RuleContext ctx = new RuleContext();
    final AtomicInteger violations = new AtomicInteger(0);
    ctx.getReport().addListener(new ReportListener()
                                {
                                  @Override
                                  public void ruleViolationAdded (RuleViolation ruleViolation)
                                  {
                                    violations.incrementAndGet();
                                  }
                                  @Override
                                  public void metricAdded (Metric metric)
                                  {
                                  }
                                });
    processFiles(configuration, ruleSetFactory, files, ctx, renderers);
    reportStart = System.nanoTime();
    renderer.end();
    renderer.flush();
    return violations.get();
  }
  catch (Exception e)
  {
    String message = e.getMessage();
    if (message != null)
    {
      LOG.severe(message);
    }
    else
    {
      LOG.log(Level.SEVERE, "Exception during processing", e);
    }
    LOG.log(Level.FINE, "Exception during processing", e);
    LOG.info(PMDCommandLineInterface.buildUsageText());
    return 0;
  }
  finally {
            Benchmarker.mark(Benchmark.Reporting, (System.nanoTime() - reportStart), 0);
          }
}