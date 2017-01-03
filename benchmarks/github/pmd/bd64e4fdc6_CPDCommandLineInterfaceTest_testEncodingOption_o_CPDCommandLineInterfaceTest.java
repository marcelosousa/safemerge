{
  String origEncoding = System.getProperty("file.encoding");
  System.setProperty("file.encoding", "Cp1252");
  runCPD("--minimum-tokens", "34", "--language", "java", "--files", "src/test/resources/net/sourceforge/pmd/cpd/clitest/", "--ignore-identifiers", "--format", "xml", "--encoding", "UTF-8");
  System.setProperty("file.encoding", origEncoding);
  String out = bufferStdout.toString("UTF-8");
  Assert.assertTrue(out.startsWith("<?xml version=\"1.0\" encoding=\"UTF-8\"?>"));
  Assert.assertTrue(Pattern.compile("System\\.out\\.println\\([ij] \\+ \"\u00e4\"\\);").matcher(out).find());
}