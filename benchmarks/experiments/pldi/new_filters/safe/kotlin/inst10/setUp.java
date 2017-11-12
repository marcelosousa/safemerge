private boolean kotlinInternalModeOriginalValue;
@Override
 protected void setUp () throws Exception
{
  super.setUp();
  ((StartupManagerImpl) StartupManager.getInstance(getProject())).runPostStartupActivities();
  VirtualDirectoryImpl.allowRootAccess(JetTestCaseBuilder.getHomeDirectory());
  kotlinInternalModeOriginalValue = KotlinInternalMode.OBJECT$.getEnabled();
  KotlinInternalMode.OBJECT$.setEnabled(true);
  return;
}
