private boolean kotlinInternalModeOriginalValue;
@Override
 protected void setUp () throws Exception
{
  super.setUp();
  ((StartupManagerImpl) StartupManager.getInstance(getProject())).runPostStartupActivities();
  VirtualDirectoryImpl.allowRootAccess(JetTestUtils.getHomeDirectory());
  kotlinInternalModeOriginalValue = KotlinInternalMode.Instance.getEnabled();
  KotlinInternalMode.Instance.setEnabled(true);
  return;
}
