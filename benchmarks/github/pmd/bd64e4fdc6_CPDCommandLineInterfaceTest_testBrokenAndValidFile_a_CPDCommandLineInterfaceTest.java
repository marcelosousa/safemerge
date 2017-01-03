{
  runCPD("--minimum-tokens", "10", "--language", "java", "--files", "src/test/resources/net/sourceforge/pmd/cpd/badandgood/", "--format", "text", "--skip-lexical-errors");
  String out = bufferStdout.toString("UTF-8");
  Assert.assertTrue(Pattern.compile("Skipping .*?BadFile\\.java\\. Reason: Lexical error in file").matcher(out).find());
  Assert.assertTrue(out.contains("Found a 5 line (13 tokens) duplication"));
  Assert.assertEquals(4, Integer.parseInt(System.getProperty(CPDCommandLineInterface.STATUS_CODE_PROPERTY)));
}