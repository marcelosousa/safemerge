{
  runCPD("--minimum-tokens", "100", "--files", "src/test/resources/net/sourceforge/pmd/cpd/badandgood/", "--language", "c", "--format", "csv");
  String out = getOutput();
  Assert.assertFalse(out.contains("Couldn't instantiate renderer"));
}