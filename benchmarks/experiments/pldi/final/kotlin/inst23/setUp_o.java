@Override
 protected void setUp () throws Exception
{
  super.setUp();
  ((StartupManagerImpl) StartupManager.getInstance(getProject())).runPostStartupActivities();
  VfsRootAccess.allowRootAccess(JetTestCaseBuilder.getHomeDirectory());
  return;
}