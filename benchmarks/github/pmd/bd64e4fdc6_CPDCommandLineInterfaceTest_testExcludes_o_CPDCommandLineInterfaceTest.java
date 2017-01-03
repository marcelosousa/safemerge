{
  runCPD("--minimum-tokens", "34", "--language", "java", "--ignore-identifiers", "--files", "src/test/resources/net/sourceforge/pmd/cpd/clitest/", "--exclude", "src/test/resources/net/sourceforge/pmd/cpd/clitest/File2.java");
  String out = bufferStdout.toString("UTF-8");
  Assert.assertFalse(out.contains("Found a 7 line (34 tokens) duplication"));
}