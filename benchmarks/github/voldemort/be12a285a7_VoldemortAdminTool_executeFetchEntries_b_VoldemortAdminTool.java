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
                                  System.out.println(("Fetching entries in partitions " + Joiner.on(", ").join(partitionIdList) + " of " + store));
                                }
                                final Iterator<Pair<ByteArray, Versioned<byte[]>>> entriesIterator = adminClient.fetchEntries(nodeId, store, partitionIdList, null, false);
                                File outputFile = null;
                                if (directory != null)
                                {
                                  outputFile = new File(directory, store + ".entries");
                                }
                                if (useAscii)
                                {
                                  SerializerDefinition keySerializerDef = storeDefinition.getKeySerializer();
                                  SerializerDefinition valueSerializerDef = storeDefinition.getValueSerializer();
                                  SerializerFactory serializerFactory = new DefaultSerializerFactory();
                                  @SuppressWarnings("unchecked")
                                  final Serializer<Object> keySerializer = (Serializer<Object>) serializerFactory.getSerializer(keySerializerDef);
                                  @SuppressWarnings("unchecked")
                                  final Serializer<Object> valueSerializer = (Serializer<Object>) serializerFactory.getSerializer(valueSerializerDef);
                                  final CompressionStrategy keyCompressionStrategy;
                                  final CompressionStrategy valueCompressionStrategy;
                                  if (keySerializerDef != null && keySerializerDef.hasCompression())
                                  {
                                    keyCompressionStrategy = new CompressionStrategyFactory().get(keySerializerDef.getCompression());
                                  }
                                  else
                                  {
                                    keyCompressionStrategy = null;
                                  }
                                  if (valueSerializerDef != null && valueSerializerDef.hasCompression())
                                  {
                                    valueCompressionStrategy = new CompressionStrategyFactory().get(valueSerializerDef.getCompression());
                                  }
                                  else
                                  {
                                    valueCompressionStrategy = null;
                                  }
                                  writeAscii(outputFile, new Writable()
                                                         {
                                                           @Override
                                                           public void writeTo (BufferedWriter out) throws IOException
                                                           {
                                                             final StringWriter stringWriter = new StringWriter();
                                                             final JsonGenerator generator = new JsonFactory(new ObjectMapper()).createJsonGenerator(stringWriter);
                                                             while (entriesIterator.hasNext())
                                                             {
                                                               Pair<ByteArray, Versioned<byte[]>> kvPair = entriesIterator.next();
                                                               byte[] keyBytes = kvPair.getFirst().get();
                                                               byte[] valueBytes = kvPair.getSecond().getValue();
                                                               VectorClock version = (VectorClock) kvPair.getSecond().getVersion();
                                                               Object keyObject = keySerializer.toObject((null == keyCompressionStrategy ? keyBytes : keyCompressionStrategy.inflate(keyBytes)));
                                                               Object valueObject = valueSerializer.toObject((null == valueCompressionStrategy ? valueBytes : valueCompressionStrategy.inflate(valueBytes)));
                                                               generator.writeObject(keyObject);
                                                               stringWriter.write(' ');
                                                               stringWriter.write(version.toString());
                                                               generator.writeObject(valueObject);
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
                                                              while (entriesIterator.hasNext())
                                                              {
                                                                Pair<ByteArray, Versioned<byte[]>> kvPair = entriesIterator.next();
                                                                byte[] keyBytes = kvPair.getFirst().get();
                                                                byte[] versionBytes = ((VectorClock) kvPair.getSecond().getVersion()).toBytes();
                                                                byte[] valueBytes = kvPair.getSecond().getValue();
                                                                out.writeInt(keyBytes.length);
                                                                out.write(keyBytes);
                                                                out.writeInt(versionBytes.length);
                                                                out.write(versionBytes);
                                                                out.writeInt(valueBytes.length);
                                                                out.write(valueBytes);
                                                              }
                                                            }
                                                          });
                                }
                                if (outputFile != null)
                                  System.out.println(("Fetched keys from " + store + " to " + outputFile));
                              }
}