{
  if ((null == params.getSourceDir() && null) == params.getUri())
  {
    throw new IllegalArgumentException("Please provide either source root directory (-dir or -d) or database URI (-uri or -u) parameter");
  }
  PMDConfiguration configuration = new PMDConfiguration();
  configuration.setInputPaths(params.getSourceDir());
  configuration.setInputUri(params.getUri());
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
                                                                                                            String languageVersion = null == params.getVersion() ? LanguageVersion.getDefaultVersion(language.getLanguage()).getVersion() : params.getVersion();
                                                                                                            configuration.getLanguageVersionDiscoverer().setDefaultLanguageVersion(language.getLanguage().getVersion(languageVersion));
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