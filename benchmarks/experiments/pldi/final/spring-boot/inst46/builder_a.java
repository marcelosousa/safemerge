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
    builder.maxWaitTime(options.getMaxWaitTime());
    builder.readPreference(options.getReadPreference());
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