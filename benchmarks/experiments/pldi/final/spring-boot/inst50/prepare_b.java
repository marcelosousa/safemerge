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
    prepare(((GenericApplicationContext) applicationContext));
  }
  else
    ;
  this.rootContext = applicationContext;
}