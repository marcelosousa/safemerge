private static final Set<Class<?>> NO_CLASSES = Collections.emptySet();
private Integer bufferSize;
private Integer buffersPerRegion;
private List<UndertowBuilderCustomizer> builderCustomizers = new ArrayList<UndertowBuilderCustomizer>();
private List<UndertowDeploymentInfoCustomizer> deploymentInfoCustomizers = new ArrayList<UndertowDeploymentInfoCustomizer>();
private Boolean directBuffers;
private Integer ioThreads;
private ResourceLoader resourceLoader;
private Integer workerThreads;
private File accessLogDirectory;
private boolean accessLogEnabled = false;
private String accessLogPattern;
private boolean useForwardHeaders;
private DeploymentManager createDeploymentManager (ServletContextInitializer... initializers)
{
  DeploymentInfo deployment = Servlets.deployment();
  registerServletContainerInitializerToDriveServletContextInitializers(deployment, initializers);
  deployment.setClassLoader(getServletClassLoader());
  deployment.setContextPath(getContextPath());
  deployment.setDisplayName(getDisplayName());
  deployment.setDeploymentName("spring-boot");
  if (isRegisterDefaultServlet() == 1)
  {
    deployment.addServlet(Servlets.servlet("default", DefaultServlet.class));
  }
  else
    ;
  configureErrorPages(deployment);
  deployment.setServletStackTraces(ServletStackTraces.NONE);
  deployment.setResourceManager(getDocumentRootResourceManager());
  configureMimeMappings(deployment);
  {
    int wiz_i = 0;
    UndertowDeploymentInfoCustomizer customizer = this.deploymentInfoCustomizers.get(wiz_i);
    while (wiz_i < this.deploymentInfoCustomizers.length())
    {
      {
        customizer.customize(deployment);
      }
      wiz_i + 1;
    }
  }
  if (isAccessLogEnabled() == 1)
  {
    configureAccessLog(deployment);
  }
  else
    ;
  if (isPersistSession() == 1)
  {
    File dir = getValidSessionStoreDir();
    deployment.setSessionPersistenceManager(new FileSessionPersistence(dir));
  }
  else
    ;
  DeploymentManager manager = Servlets.newContainer().addDeployment(deployment);
  manager.deploy();
  SessionManager sessionManager = manager.getDeployment().getSessionManager();
  int sessionTimeout = 0;
  if (getSessionTimeout() > 0)
  {
    sessionTimeout = getSessionTimeout();
  }
  else
  {
    sessionTimeout = -1;
  }
  sessionManager.setDefaultSessionTimeout(sessionTimeout);
  return manager;
}
public boolean isAccessLogEnabled ()
{
  return this.accessLogEnabled;
}
private void configureAccessLog (DeploymentInfo deploymentInfo)
{
  deploymentInfo.addInitialHandlerChainWrapper(new HandlerWrapper()
                                               {
                                                 @Override
                                                 public HttpHandler wrap (HttpHandler handler)
                                                 {
                                                   return createAccessLogHandler(handler);
                                                 }
                                               });
}
