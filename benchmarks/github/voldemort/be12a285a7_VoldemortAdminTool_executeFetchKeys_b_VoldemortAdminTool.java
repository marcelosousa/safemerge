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
                                final Iterator<ByteArray> keyIterator = adminClient.fetchKeys(nodeId, store, partitionIdList, null, false);
                                File outputFile = null;
                                if (directory != null)
                                {
                                  outputFile = new File(directory, store + ".keys");
                                }
                                if (useAscii)
                                {
                                  final SerializerDefinition serializerDef = storeDefinition.getKeySerializer();
                                  final SerializerFactory serializerFactory = new DefaultSerializerFactory();
                                  @SuppressWarnings("unchecked")
                                  final Serializer<Object> serializer = (Serializer<Object>) serializerFactory.getSerializer(serializerDef);
                                  final CompressionStrategy keysCompressionStrategy;
                                  if (serializerDef != null && serializerDef.hasCompression())
                                  {
                                    keysCompressionStrategy = new CompressionStrategyFactory().get(serializerDef.getCompression());
                                  }
                                  else
                                  {
                                    keysCompressionStrategy = null;
                                  }
                                  writeAscii(outputFile, new Writable()
                                                         {
                                                           @Override
                                                           public void writeTo (BufferedWriter out) throws IOException
                                                           {
                                                             final StringWriter stringWriter = new StringWriter();
                                                             final JsonGenerator generator = new JsonFactory(new ObjectMapper()).createJsonGenerator(stringWriter);
                                                             while (keyIterator.hasNext())
                                                             {
                                                               byte[] keyBytes = keyIterator.next().get();
                                                               Object keyObject = serializer.toObject((null == keysCompressionStrategy ? keyBytes : keysCompressionStrategy.inflate(keyBytes)));
                                                               generator.writeObject(keyObject);
                                                               StringBuffer buf = stringWriter.getBuffer();
                                                               if (buf.charAt(0) == ' ')
                                                               {
                                                                 buf.setCharAt(0, '\n');
                                                               }
                                                               out.write(buf.toString());
                                                               buf.setLength(0);
                                                             }
                                                             out.write('\n');
                                                           }
                                                         });
                                }
                                else
                                {
                                  writeBinary(outputFile, new Printable()
                                                          {
                                                            @Override
                                                            public void printTo (DataOutputStream out) throws IOException
                                                            {
                                                              while (keyIterator.hasNext())
                                                              {
                                                                byte[] keyBytes = keyIterator.next().get();
                                                                out.writeInt(keyBytes.length);
                                                                out.write(keyBytes);
                                                              }
                                                            }
                                                          });
                                }
                                if (outputFile != null)
                                  System.out.println(("Fetched keys from " + store + " to " + outputFile));
                              }
}