@Override
 protected void setUp () throws Exception
{
  super.setUp();
  ((StartupManagerImpl) StartupManager.getInstance(getProject())).runPostStartupActivities();
  VirtualDirectoryImpl.allowRootAccess(JetTestCaseBuilder.getHomeDirectory());
  return;
}