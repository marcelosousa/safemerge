{
  Map<ByteArray, List<Versioned<byte[]>>> proxyKeyValues = proxyGetAll(keys, stealInfoList);
  for (Map.Entry<ByteArray, List<Versioned<byte[]>>> keyValuePair : proxyKeyValues.entrySet()) {
                                                                                                 for (Versioned<byte[]> proxyValue : keyValuePair.getValue()) {
                                                                                                                                                                try
                                                                                                                                                                {
                                                                                                                                                                  getInnerStore().put(keyValuePair.getKey(), proxyValue);
                                                                                                                                                                }
                                                                                                                                                                catch (ObsoleteVersionException e)
                                                                                                                                                                {
                                                                                                                                                                }
                                                                                                                                                              }
                                                                                               }
}