{
  runCPD("--minimum-tokens", "34", "--language", "java", "--files", "src/test/resources/net/sourceforge/pmd/cpd/clitest/", "--ignore-identifiers");
  String out = bufferStdout.toString("UTF-8");
  Assert.assertTrue(out.contains("Found a 7 line (36 tokens) duplication"));
  Assert.assertEquals(4, Integer.parseInt(System.getProperty(CPDCommandLineInterface.STATUS_CODE_PROPERTY)));
}