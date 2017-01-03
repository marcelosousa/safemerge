{
  PMDConfiguration configuration = new PMDConfiguration();
  configuration.setInputPaths(params.getSourceDir());
  configuration.setReportFormat(params.getFormat());
  configuration.setBenchmark(params.isBenchmark());
  configuration.setDebug(params.isDebug());
  configuration.setMinimumPriority(params.getMinimumPriority());
  configuration.setReportFile(params.getReportfile());
  configuration.setReportProperties(params.getProperties());
  configuration.setReportShortNames(params.isShortnames());
  configuration.setRuleSets(params.getRulesets());
  configuration.setShowSuppressedViolations(params.isShowsuppressed());
  configuration.setSourceEncoding(params.getEncoding());
  configuration.setStressTest(params.isStress());
  configuration.setSuppressMarker(params.getSuppressmarker());
  configuration.setThreads(params.getThreads());
  for (LanguageVersion language : LanguageVersion.findVersionsForLanguageTerseName(params.getLanguage())) {
                                                                                                            configuration.getLanguageVersionDiscoverer().setDefaultLanguageVersion(language.getLanguage().getVersion(params.getVersion()));
                                                                                                          }
  try
  {
    configuration.prependClasspath(params.getAuxclasspath());
  }
  catch (IOException e)
  {
    throw new IllegalArgumentException("Invalid auxiliary classpath: " + e.getMessage(), e);
  }
  return configuration;
}