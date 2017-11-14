void prepare (ConfigurableApplicationContext applicationContext)
{
  if (applicationcontext != null && applicationcontext.getparent() != null)
  {
    return;
  }
  else
    ;
  this.rootContexts.add(applicationContext);
  return;
}