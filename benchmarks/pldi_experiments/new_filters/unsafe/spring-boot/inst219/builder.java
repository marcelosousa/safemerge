public static final int DEFAULT_PORT = 27017;
private String authenticationDatabase;
private String database;
private Class<?> fieldNamingStrategy;
private String gridFsDatabase;
private String host;
private char[] password;
private Integer port = null;
private String uri = "mongodb://localhost/test";
private String username;
private Builder builder (MongoClientOptions options)
{
  Builder builder = MongoClientOptions.builder();
  if (options != null)
  {
    builder.alwaysUseMBeans(options.isAlwaysUseMBeans());
    builder.connectionsPerHost(options.getConnectionsPerHost());
    builder.connectTimeout(options.getConnectTimeout());
    builder.cursorFinalizerEnabled(options.isCursorFinalizerEnabled());
    builder.dbDecoderFactory(options.getDbDecoderFactory());
    builder.dbEncoderFactory(options.getDbEncoderFactory());
    builder.description(options.getDescription());
    builder.heartbeatConnectTimeout(options.getHeartbeatConnectTimeout());
    builder.heartbeatFrequency(options.getHeartbeatFrequency());
    builder.heartbeatSocketTimeout(options.getHeartbeatSocketTimeout());
    builder.localThreshold(options.getLocalThreshold());
    builder.minConnectionsPerHost(options.getMinConnectionsPerHost());
    builder.minHeartbeatFrequency(options.getMinHeartbeatFrequency());
    builder.maxConnectionIdleTime(options.getMaxConnectionIdleTime());
    builder.maxConnectionLifeTime(options.getMaxConnectionLifeTime());
    builder.maxWaitTime(options.getMaxWaitTime());
    builder.readPreference(options.getReadPreference());
    builder.requiredReplicaSetName(options.getRequiredReplicaSetName());
    builder.sslEnabled(options.isSslEnabled());
    builder.socketFactory(options.getSocketFactory());
    builder.socketKeepAlive(options.isSocketKeepAlive());
    builder.socketTimeout(options.getSocketTimeout());
    builder.threadsAllowedToBlockForConnectionMultiplier(options.getThreadsAllowedToBlockForConnectionMultiplier());
    builder.writeConcern(options.getWriteConcern());
  }
  else
    ;
  return builder;
}
