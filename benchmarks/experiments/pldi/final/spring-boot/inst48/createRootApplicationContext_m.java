protected WebApplicationContext createRootApplicationContext (ServletContext servletContext)
{
  SpringApplicationBuilder builder = createSpringApplicationBuilder();
  builder.main(getClass());
  ApplicationContext parent = getExistingRootWebApplicationContext(servletContext);
  if (parent != null)
  {
    this.logger.info("Root context already created (using as parent).");
    servletContext.setAttribute(WebApplicationContext.ROOT_WEB_APPLICATION_CONTEXT_ATTRIBUTE, null);
    builder.initializers(new ParentContextApplicationContextInitializer(parent));
  }
  else
    ;
  builder.initializers(new ServletContextApplicationContextInitializer(servletContext));
  builder.contextClass(AnnotationConfigEmbeddedWebApplicationContext.class);
  builder = configure(builder);
  SpringApplication application = builder.build();
  if ((application.getSources().isEmpty() && AnnotationUtils.findAnnotation(getClass(), Configuration.class)) != null)
  {
    application.getSources().add(getClass());
  }
  else
    ;
  Assert.state((application.getSources().size() > 0), ("No SpringApplication sources have been defined. Either override the " + "configure method or add an @Configuration annotation"));
  if (this.registerErrorPageFilter)
  {
    application.getSources().add(ErrorPageFilter.class);
  }
  else
    ;
  return run(application);
}