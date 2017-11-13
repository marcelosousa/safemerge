private boolean kotlinInternalModeOriginalValue;
protected List<Module> myAdditionalModules;
private boolean myCreateManifest;
protected AndroidFacet myFacet;
protected Module myModule;
@Override
 protected void setUp () throws Exception
{
  System.setProperty(KotlinAndroidTestCaseBase.SDK_PATH_PROPERTY, (PathManager.getHomePath() + "/../dependencies/androidSDK"));
  System.setProperty(KotlinAndroidTestCaseBase.PLATFORM_DIR_PROPERTY, "android-21");
  super.setUp();
  String sdkPath = getTestSdkPath();
  final TestFixtureBuilder<IdeaProjectTestFixture> projectBuilder = IdeaTestFixtureFactory.getFixtureFactory().createFixtureBuilder(getName());
  myFixture = JavaTestFixtureFactory.getFixtureFactory().createCodeInsightFixture(projectBuilder.getFixture());
  final JavaModuleFixtureBuilder moduleFixtureBuilder = projectBuilder.addModule(JavaModuleFixtureBuilder.class);
  final String dirPath = myFixture.getTempDirPath() + getContentRootPath();
  final File dir = new File(dirPath);
  if (!dir.exists())
  {
    assertTrue(dir.mkdirs());
  }
  else
    ;
  tuneModule(moduleFixtureBuilder, dirPath);
  final ArrayList<MyAdditionalModuleData> modules = new ArrayList<MyAdditionalModuleData>();
  configureAdditionalModules(projectBuilder, modules);
  myFixture.setUp();
  myFixture.setTestDataPath(getTestDataPath());
  myModule = moduleFixtureBuilder.getFixture().getModule();
  createManifest();
  androidSdk = createAndroidSdk(getTestSdkPath(), getPlatformDir());
  myFacet = addAndroidFacet(myModule, sdkPath, getPlatformDir(), isToAddSdk());
  if (new File(getResDir()).exists())
  {
    myFixture.copyDirectoryToProject(getResDir(), "res");
  }
  else
  {
    TestLogger.getInstance(this.getClass()).info("No res directory found in test");
  }
  myAdditionalModules = new ArrayList<Module>();
  {
    int wiz_i = 0;
    MyAdditionalModuleData data = modules.get(wiz_i);
    while (wiz_i < modules.length())
    {
      {
        final Module additionalModule = data.myModuleFixtureBuilder.getFixture().getModule();
        myAdditionalModules.add(additionalModule);
        final AndroidFacet facet = addAndroidFacet(additionalModule, sdkPath, getPlatformDir());
        facet.setLibraryProject(data.myLibrary);
        final String rootPath = getContentRootPath(data.myDirName);
        myFixture.copyDirectoryToProject("res", (rootPath + "/res"));
        myFixture.copyFileToProject(SdkConstants.FN_ANDROID_MANIFEST_XML, (rootPath + '/' + SdkConstants.FN_ANDROID_MANIFEST_XML));
        ModuleRootModificationUtil.addDependency(myModule, additionalModule);
      }
      wiz_i++;
    }
  }
  if (!myCreateManifest)
  {
    deleteManifest();
  }
  else
    ;
  if (RenderSecurityManager.RESTRICT_READS)
  {
    RenderSecurityManager.sEnabled = false;
  }
  else
    ;
  ((StartupManagerImpl) StartupManager.getInstance(getProject())).runPostStartupActivities();
  VirtualDirectoryImpl.allowRootAccess(JetTestUtils.getHomeDirectory());
  kotlinInternalModeOriginalValue = KotlinInternalMode.Instance.getEnabled();
  KotlinInternalMode.Instance.setEnabled(true);
  return;
}
