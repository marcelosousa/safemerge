{
  List<StoreDefinition> storeDefinitionList = adminClient.getRemoteStoreDefList(nodeId).getValue();
  Map<String, StoreDefinition> storeDefinitionMap = Maps.newHashMap();
  for (StoreDefinition storeDefinition : storeDefinitionList) {
                                                                storeDefinitionMap.put(storeDefinition.getName(), storeDefinition);
                                                              }
  File directory = null;
  if (outputDir != null)
  {
    directory = new File(outputDir);
    if (!(directory.exists() || directory.mkdir()))
    {
      Utils.croak(("Can't find or create directory " + outputDir));
    }
  }
  List<String> stores = storeNames;
  if (stores == null)
  {
    stores = Lists.newArrayList();
    stores.addAll(storeDefinitionMap.keySet());
  }
  else
  {
    storeDefinitionMap.putAll(getSystemStoreDefs());
  }
  if (partitionIdList == null)
  {
    partitionIdList = Lists.newArrayList();
    for (Node node : adminClient.getAdminClientCluster().getNodes()) {
                                                                       partitionIdList.addAll(node.getPartitionIds());
                                                                     }
  }
  StoreDefinition storeDefinition = null;
  for (String store : stores) {
                                storeDefinition = storeDefinitionMap.get(store);
                                if (null == storeDefinition)
                                {
                                  System.out.println(("No store found under the name '" + store + "'"));
                                  continue;
                                }
                                else
                                {
                                  System.out.println(("Fetching keys in partitions " + Joiner.on(", ").join(partitionIdList) + " of " + store));
                                }
                                Iterator<ByteArray> keyIterator = adminClient.fetchKeys(nodeId, store, partitionIdList, null, false);
                                File outputFile = null;
                                if (directory != null)
                                {
                                  outputFile = new File(directory, store + ".keys");
                                }
                                if (useAscii)
                                {
                                  writeKeysAscii(keyIterator, outputFile, storeDefinition);
                                }
                                else
                                {
                                  writeKeysBinary(keyIterator, outputFile);
                                }
                                if (outputFile != null)
                                  System.out.println(("Fetched keys from " + store + " to " + outputFile));
                              }
}