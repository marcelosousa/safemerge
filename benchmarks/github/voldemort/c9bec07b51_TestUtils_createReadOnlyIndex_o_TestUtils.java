{
  File dataFile = File.createTempFile("test", ".txt");
  dataFile.deleteOnExit();
  BufferedWriter writer = new BufferedWriter(new FileWriter(dataFile));
  for (Map.Entry<String, String> entry : data.entrySet()) writer.write(("\"" + entry.getKey() + "\"\t\"" + entry.getValue() + "\"\n"));
  writer.close();
  BufferedReader reader = new BufferedReader(new FileReader(dataFile));
  JsonReader jsonReader = new JsonReader(reader);
  SerializerDefinition serDef = new SerializerDefinition("json", "'string'");
  StoreDefinition storeDef = new StoreDefinition("test", ReadOnlyStorageConfiguration.TYPE_NAME, serDef, serDef, RoutingTier.CLIENT, RoutingStrategyType.CONSISTENT_STRATEGY, 1, 1, 1, 1, 1, 1);
  RoutingStrategy router = new RoutingStrategyFactory(cluster).getRoutingStrategy(storeDef);
  File dataDir = new File(baseDir + File.separatorChar + "read-only-temp-index-" + new Integer((int) (Math.random() * 1000)));
  JsonStoreBuilder storeBuilder = new JsonStoreBuilder(jsonReader, cluster, storeDef, router, dataDir, null, 100, 1, 2, 10000);
  storeBuilder.build();
  return dataDir.getAbsolutePath();
}