private Node newNode ()
{
  Settings settings = Settings.builder().put(ClusterName.SETTING, InternalTestCluster.clusterName("single-node-cluster", randomLong())).put(Environment.PATH_HOME_SETTING.getKey(), createTempDir()).put(Environment.PATH_SHARED_DATA_SETTING.getKey(), createTempDir().getParent()).put("node.name", nodeName()).put(IndexMetaData.SETTING_NUMBER_OF_SHARDS, 1).put(IndexMetaData.SETTING_NUMBER_OF_REPLICAS, 0).put("script.inline", "on").put("script.indexed", "on").put(EsExecutors.PROCESSORS, 1).put("http.enabled", false).put(Node.NODE_LOCAL_SETTING.getKey(), true).put(Node.NODE_DATA_SETTING.getKey(), true).put(InternalSettingsPreparer.IGNORE_SYSTEM_PROPERTIES_SETTING, true).build();
  Node build = new MockNode(settings, getVersion(), getPlugins());
  build.start();
  assertThat(DiscoveryNode.localNode(build.settings()), is(true));
  return build;
}