{
  int maxLength = Iterables.size(keys);
  List<ByteArray> redirectingKeys = Lists.newArrayListWithExpectedSize(maxLength);
  List<RebalancePartitionsInfo> rebalancePartitionsInfos = Lists.newArrayListWithExpectedSize(maxLength);
  for (ByteArray key : keys) {
                               RebalancePartitionsInfo info;
                               info = redirectingKey(key);
                               if (info != null)
                               {
                                 redirectingKeys.add(key);
                                 rebalancePartitionsInfos.add(info);
                               }
                             }
  if (!redirectingKeys.isEmpty())
    proxyGetAllAndLocalPut(redirectingKeys, rebalancePartitionsInfos);
  return getInnerStore().getAll(keys);
}