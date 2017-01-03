{
  runCPD("--minimum-tokens", "34", "--language", "java", "--files", "src/test/resources/net/sourceforge/pmd/cpd/clitest/", "--ignore-identifiers");
  String out = getOutput();
  Assert.assertTrue(out.contains("Found a 7 line (36 tokens) duplication"));
}