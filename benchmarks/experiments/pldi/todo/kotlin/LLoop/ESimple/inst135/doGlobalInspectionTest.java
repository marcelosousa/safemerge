private boolean kotlinInternalModeOriginalValue;
protected List<Module> myAdditionalModules;
private boolean myCreateManifest;
protected AndroidFacet myFacet;
protected Module myModule;
protected void doGlobalInspectionTest (@NotNull
                                       GlobalInspectionToolWrapper wrapper, @NotNull
                                                                            String globalTestDir, @NotNull
                                                                                                  AnalysisScope scope)
{
  myFixture.enableInspections(wrapper.getTool());
  scope.invalidate();
  final InspectionManagerEx inspectionManager = (InspectionManagerEx) InspectionManager.getInstance(getProject());
  final GlobalInspectionContextImpl globalContext = CodeInsightTestFixtureImpl.createGlobalContextForTool(scope, getProject(), inspectionManager, wrapper);
  InspectionTestUtil.runTool(wrapper, scope, globalContext, new InspectionManagerEx(getProject()));
  InspectionTestUtil.compareToolResults(globalContext, wrapper, false, (getTestDataPath() + globalTestDir));
  return;
}
