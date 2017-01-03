{
  registerBean(server, JmxUtils.createObjectName(VoldemortServer.class));
  registerBean(cluster, JmxUtils.createObjectName(Cluster.class));
  for (VoldemortService service : services) registerBean(service, JmxUtils.createObjectName(service.getClass()));
  for (Store<ByteArray, byte[], byte[]> store : this.storeRepository.getAllStorageEngines()) {
                                                                                               if (server.getVoldemortConfig().isEnableJmxClusterName())
                                                                                                 registerBean(store, JmxUtils.createObjectName((this.cluster.getName() + "." + JmxUtils.getPackageName(store.getClass())), store.getName()));
                                                                                               else
                                                                                                 registerBean(store, JmxUtils.createObjectName(JmxUtils.getPackageName(store.getClass()), store.getName()));
                                                                                               if (store instanceof BdbStorageEngine)
                                                                                               {
                                                                                                 BdbStorageEngine bdbStore = (BdbStorageEngine) store;
                                                                                                 registerBean(bdbStore.getBdbEnvironmentStats(), JmxUtils.createObjectName(JmxUtils.getPackageName(BdbEnvironmentStats.class), store.getName()));
                                                                                               }
                                                                                             }
}