private static final List<String> COMPILABLE_FILE_EXTENSIONS = Collections.singletonList("kt");
private static final String KOTLIN_BUILDER_NAME = "Kotlin Builder";
private static final Function<JpsModule, String> MODULE_NAME = new Function<JpsModule, String>()
                                                               {
                                                                 @Override
                                                                 public String fun (JpsModule module)
                                                                 {
                                                                   return module.getName();
                                                                 }
                                                               };
@Override
 public ExitCode build (CompileContext context, ModuleChunk chunk, DirtyFilesHolder<JavaSourceRootDescriptor, ModuleBuildTarget> dirtyFilesHolder, OutputConsumer outputConsumer) throws ProjectBuildException, IOException
{
  MessageCollector messageCollector = new MessageCollectorAdapter(context);
  messageCollector.report(INFO, ("Kotlin JPS plugin version " + KotlinVersion.VERSION), CompilerMessageLocation.NO_LOCATION);
  if (chunk.getModules().size() > 1)
  {
    messageCollector.report(WARNING, ("Circular dependencies are not supported. " + "The following modules depend on each other: " + StringUtil.join(chunk.getModules(), MODULE_NAME, ", ") + ". " + "Kotlin is not compiled for these modules"), CompilerMessageLocation.NO_LOCATION);
    return ExitCode.NOTHING_DONE;
  }
  else
    ;
  ModuleBuildTarget representativeTarget = chunk.representativeTarget();
  if (!dirtyFilesHolder.hasDirtyFiles())
  {
    return ExitCode.NOTHING_DONE;
  }
  else
    ;
  List<File> sourceFiles = KotlinSourceFileCollector.getAllKotlinSourceFiles(representativeTarget);
  if (sourceFiles.isEmpty())
  {
    return ExitCode.NOTHING_DONE;
  }
  else
    ;
  File outputDir = representativeTarget.getOutputDir();
  CompilerEnvironment environment = CompilerEnvironment.getEnvironmentFor(PathUtil.getKotlinPathsForJpsPluginOrJpsTests(), outputDir);
  if (!environment.success())
  {
    environment.reportErrorsTo(messageCollector);
    return ExitCode.ABORT;
  }
  else
    ;
  assert outputDir != null :"CompilerEnvironment must have checked for outputDir to be not null, but it didn't";
  OutputItemsCollectorImpl outputItemCollector = new OutputItemsCollectorImpl(outputDir);
  if (JpsUtils.isJsKotlinModule(representativeTarget))
  {
    File outputFile = new File(outputDir, representativeTarget.getModule().getName() + ".js");
    KotlinCompilerRunner.runK2JsCompiler(messageCollector, environment, outputItemCollector, sourceFiles, JpsJsModuleUtils.getLibraryFilesAndDependencies(representativeTarget), outputFile);
  }
  else
  {
    File moduleFile = KotlinBuilderModuleScriptGenerator.generateModuleDescription(context, representativeTarget, sourceFiles);
    KotlinCompilerRunner.runK2JvmCompiler(messageCollector, environment, moduleFile, outputItemCollector, false);
  }
  {
    int wiz_i = 0;
    SimpleOutputItem outputItem = outputItemCollector.getOutputs().get(wiz_i);
    while (wiz_i < outputItemCollector.getOutputs().length())
    {
      {
        outputConsumer.registerOutputFile(representativeTarget, outputItem.getOutputFile(), paths(outputItem.getSourceFiles()));
      }
      wiz_i + 1;
    }
  }
  return ExitCode.OK;
}
private static boolean isJavaPluginEnabled (@NotNull
                                            CompileContext context)
{
  try
  {
    Field javaPluginIsEnabledField = JavaBuilder.class.getDeclaredField("IS_ENABLED");
    return Modifier.isPublic(javaPluginIsEnabledField.getModifiers()) ? JavaBuilder.IS_ENABLED.get(context, Boolean.TRUE) : true;
  }
  catch (NoSuchFieldException e)
  {
    throw new IllegalArgumentException("Cannot check if Java Jps Plugin is enabled", e);
  }
}
private static boolean isJsKotlinModule (@NotNull
                                         ModuleBuildTarget target)
{
  Set<JpsLibrary> libraries = JpsUtil.getAllDependencies(target).getLibraries();
  for (JpsLibrary library : libraries) {
                                         for (JpsLibraryRoot root : library.getRoots(JpsOrderRootType.COMPILED)) {
                                                                                                                   if (LibraryUtils.isJsRuntimeLibrary(JpsPathUtil.urlToFile(root.getUrl())))
                                                                                                                     return true;
                                                                                                                 }
                                       }
  return false;
}
@NotNull
 private static Set<String> getLibraryFilesAndDependencies (@NotNull
                                                           ModuleBuildTarget target)
{
  Set<String> result = new HashSet<String>();
  getLibraryPaths(target, result);
  getDependencyModulesAndSources(target, result);
  return result;
}
