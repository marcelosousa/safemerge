void prepare (ConfigurableApplicationContext applicationContext)
{
  if (applicationContext != null && applicationContext.getParent() != null)
  {
    return;
  }
  else
    ;
  this.rootContext = applicationContext;
  return;
}