@NotNull
 @Override
 protected ExitCode doExecute (K2JSCompilerArguments arguments, PrintingMessageCollector messageCollector, Disposable rootDisposable)
{
  if (arguments.srcdir == null && arguments.sourceFiles == null)
  {
    messageCollector.report(CompilerMessageSeverity.ERROR, "Specify sources location via -srcdir", NO_LOCATION);
    return ExitCode.INTERNAL_ERROR;
  }
  else
    ;
  JetCoreEnvironment environmentForJS = JetCoreEnvironment.getCoreEnvironmentForJS(rootDisposable);
  if (arguments.srcdir != null)
  {
    environmentForJS.addSources(arguments.srcdir);
  }
  else
    ;
  if (arguments.sourceFiles != null)
  {
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
  }
  else
    ;
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