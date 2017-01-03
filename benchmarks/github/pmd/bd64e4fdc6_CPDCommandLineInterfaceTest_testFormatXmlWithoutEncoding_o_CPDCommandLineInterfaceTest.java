{
  runCPD("--minimum-tokens", "10", "--language", "java", "--files", "src/test/resources/net/sourceforge/pmd/cpd/clitest/", "--format", "xml");
  String out = bufferStdout.toString("UTF-8");
  Assert.assertTrue(out.contains("<duplication lines=\"3\" tokens=\"10\">"));
}