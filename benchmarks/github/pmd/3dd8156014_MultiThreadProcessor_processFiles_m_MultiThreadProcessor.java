{
  RuleSets rs = createRuleSets(ruleSetFactory);
  rs.start(ctx);
  PmdThreadFactory factory = new PmdThreadFactory(ruleSetFactory, ctx);
  ExecutorService executor = Executors.newFixedThreadPool(configuration.getThreads(), factory);
  List<Future<Report>> tasks = new LinkedList();
  for (DataSource dataSource : files) {
                                        String niceFileName = filenameFrom(dataSource);
                                        PmdRunnable r = new PmdRunnable(executor, configuration, dataSource, niceFileName, renderers);
                                        Future<Report> future = executor.submit(r);
                                        tasks.add(future);
                                      }
  executor.shutdown();
  processReports(renderers, tasks);
  rs.end(ctx);
  super.renderReports(renderers, ctx.getReport());
}