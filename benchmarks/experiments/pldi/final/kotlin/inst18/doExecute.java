@NotNull
 @Override
 protected ExitCode doExecute (K2JSCompilerArguments arguments, PrintingMessageCollector messageCollector, Disposable rootDisposable)
{
  if (arguments.sourceFiles == null)
  {
    messageCollector.report(CompilerMessageSeverity.ERROR, "Specify sources location via -sourceFiles", NO_LOCATION);
    return ExitCode.INTERNAL_ERROR;
  }
  else
    ;
  JetCoreEnvironment environmentForJS = JetCoreEnvironment.createCoreEnvironmentForJS(rootDisposable);
  {
    int wiz_i = 0;
    String sourceFile = arguments.sourceFiles.get(wiz_i);
    while (wiz_i < arguments.sourceFiles.length())
    {
      {
        environmentForJS.addSources(sourceFile);
      }
      wiz_i++;
    }
  }
  Project project = environmentForJS.getProject();
  ClassPathLibrarySourcesLoader sourceLoader = new ClassPathLibrarySourcesLoader(project);
  List<JetFile> sourceFiles = sourceLoader.findSourceFiles();
  environmentForJS.getSourceFiles().addAll(sourceFiles);
  if (arguments.isVerbose())
  {
    reportCompiledSourcesList(messageCollector, environmentForJS);
  }
  else
    ;
  Config config = getConfig(arguments, project);
  if (analyzeAndReportErrors(messageCollector, environmentForJS.getSourceFiles(), config))
  {
    return ExitCode.COMPILATION_ERROR;
  }
  else
    ;
  String outputFile = arguments.outputFile;
  if (outputFile == null)
  {
    messageCollector.report(CompilerMessageSeverity.ERROR, "Specify output file via -output", CompilerMessageLocation.NO_LOCATION);
    return ExitCode.INTERNAL_ERROR;
  }
  else
    ;
  MainCallParameters mainCallParameters = arguments.createMainCallParameters();
  return translateAndGenerateOutputFile(mainCallParameters, messageCollector, environmentForJS, config, outputFile);
}
private static boolean analyzeAndReportErrors (@NotNull
                                               PrintingMessageCollector messageCollector, @NotNull
                                                                                          final List<JetFile> sources, @NotNull
                                                                                                                       final Config config)
{
  AnalyzerWithCompilerReport analyzerWithCompilerReport = new AnalyzerWithCompilerReport(messageCollector);
  analyzerWithCompilerReport.analyzeAndReport(new Function0<AnalyzeExhaust>()
                                              {
                                                @Override
                                                public AnalyzeExhaust invoke ()
                                                {
                                                  return AnalyzerFacadeForJS.analyzeFiles(sources, Predicates.<PsiFile>alwaysTrue(), config);
                                                }
                                              }, sources);
  return analyzerWithCompilerReport.hasErrors();
}
@NotNull
 private static ExitCode translateAndGenerateOutputFile (@NotNull
                                                        MainCallParameters mainCall, @NotNull
                                                                                     PrintingMessageCollector messageCollector, @NotNull
                                                                                                                                JetCoreEnvironment environmentForJS, @NotNull
                                                                                                                                                                     Config config, @NotNull
                                                                                                                                                                                    String outputFile)
{
  try
  {
    K2JSTranslator.translateWithMainCallParametersAndSaveToFile(mainCall, environmentForJS.getSourceFiles(), outputFile, config);
  }
  catch (Exception e)
  {
    messageCollector.report(CompilerMessageSeverity.ERROR, ("Exception while translating:\n" + e.getMessage()), CompilerMessageLocation.NO_LOCATION);
    return ExitCode.INTERNAL_ERROR;
  }
  return ExitCode.OK;
}
