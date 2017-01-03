{
  MyRule r = new MyRule();
  r.definePropertyDescriptor(new IntegerProperty("testInt", "description", 0, 100, 10, 0));
  r.setMessage("Message ${packageName} ${className} ${methodName} ${variableName} ${testInt} ${noSuchProperty}");
  RuleContext ctx = new RuleContext();
  ctx.setLanguageVersion(LanguageRegistry.getLanguage(JavaLanguageModule.NAME).getDefaultVersion());
  ctx.setReport(new Report());
  ctx.setSourceCodeFilename("filename");
  DummyJavaNode s = new DummyJavaNode(1);
  s.testingOnly__setBeginColumn(5);
  s.testingOnly__setBeginLine(5);
  s.setImage("TestImage");
  s.setScope(new SourceFileScope("foo"));
  r.addViolation(ctx, s);
  RuleViolation rv = ctx.getReport().getViolationTree().iterator().next();
  assertEquals("Message foo    10 ${noSuchProperty}", rv.getDescription());
}