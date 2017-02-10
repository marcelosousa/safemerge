class AdminTest{ 
 void main() {
  OptionParser parser = new OptionParser();
  parser.accepts("native", "use native admin client");
  parser.accepts("f", "execute fetch operation");
  parser.accepts("fu", "fetch and update").withRequiredArg().ofType(Integer.class);
  parser.accepts("n", "node id").withRequiredArg().ofType(Integer.class).withValuesSeparatedBy(',');
  parser.accepts("p", "partition id").withRequiredArg().ofType(Integer.class).withValuesSeparatedBy(',');
  OptionSet options = parser.parse(args);
  List<String> nonOptions = options.nonOptionArguments();
  String bootstrapUrl = nonOptions.get(0);
  String storeName = nonOptions.get(1);
  if (!options.has("p") && !options.has("n"))
  {
    printUsage(System.err, parser, ("One or more node and/or one or more partition has" + " to be specified"));
  }
  AdminTest adminTest;
  if (options.has("native"))
    adminTest = new AdminTest(bootstrapUrl, storeName, true);
  else
    adminTest = new AdminTest(bootstrapUrl, storeName);
  SetMultimap<Integer, Integer> nodePartitions = adminTest.getNodePartitions((options.has("n") ? options.valuesOf("n") : null), (options.has("p") ? options.valuesOf("p") : null));
  adminTest.testFetch(nodePartitions);
}
}