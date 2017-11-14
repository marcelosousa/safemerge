private DeploymentManager createDeploymentManager (ServletContextInitializer... initializers)
{
  DeploymentInfo deployment = Servlets.deployment();
  registerServletContainerInitializerToDriveServletContextInitializers(deployment, initializers);
  deployment.setClassLoader(getServletClassLoader());
  deployment.setContextPath(getContextPath());
  deployment.setDeploymentName("spring-boot");
  if (isRegisterDefaultServlet())
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
      wiz_i++;
    }
  }
  DeploymentManager manager = Servlets.newContainer().addDeployment(deployment);
  manager.deploy();
  SessionManager sessionManager = manager.getDeployment().getSessionManager();
  int sessionTimeout = getSessionTimeout() > 0 ? getSessionTimeout() : -1;
  sessionManager.setDefaultSessionTimeout(sessionTimeout);
  return manager;
}