@Override
 protected void setUp () throws Exception
{
  super.setUp();
  ((StartupManagerImpl) StartupManager.getInstance(getProject())).runPostStartupActivities();
  VfsRootAccess.allowRootAccess(JetTestUtils.getHomeDirectory());
  kotlinInternalModeOriginalValue = KotlinInternalMode.Instance.getEnabled();
  KotlinInternalMode.Instance.setEnabled(true);
  return;
}