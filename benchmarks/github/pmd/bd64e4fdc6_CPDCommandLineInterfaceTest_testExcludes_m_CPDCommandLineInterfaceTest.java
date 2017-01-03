{
  runCPD("--minimum-tokens", "34", "--language", "java", "--ignore-identifiers", "--files", "src/test/resources/net/sourceforge/pmd/cpd/clitest/", "--exclude", "src/test/resources/net/sourceforge/pmd/cpd/clitest/File2.java");
  String out = getOutput();
  Assert.assertFalse(out.contains("Found a 7 line (34 tokens) duplication"));
  Assert.assertEquals(0, Integer.parseInt(System.getProperty(CPDCommandLineInterface.STATUS_CODE_PROPERTY)));
}