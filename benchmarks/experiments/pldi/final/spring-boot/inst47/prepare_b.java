void prepare (ConfigurableApplicationContext applicationContext)
{
  if (applicationContext != null && applicationContext.getParent() != null)
  {
    return;
  }
  else
    ;
  if (applicationContext instanceof GenericApplicationContext)
  {
    ((GenericApplicationContext) applicationContext).setResourceLoader(new ClassLoaderFilesResourcePatternResolver(this.classLoaderFiles));
  }
  else
    ;
  this.rootContext = applicationContext;
  return;
}