{
  Props props = new Props(properties);
  if (props.containsKey(MAX_CONNECTIONS_PER_NODE_PROPERTY))
    this.setMaxConnectionsPerNode(props.getInt(MAX_CONNECTIONS_PER_NODE_PROPERTY));
  if (props.containsKey(MAX_TOTAL_CONNECTIONS_PROPERTY))
    this.setMaxTotalConnections(props.getInt(MAX_TOTAL_CONNECTIONS_PROPERTY));
  if (props.containsKey(MAX_THREADS_PROPERTY))
    this.setMaxThreads(props.getInt(MAX_THREADS_PROPERTY));
  if (props.containsKey(MAX_QUEUED_REQUESTS_PROPERTY))
    this.setMaxQueuedRequests(props.getInt(MAX_QUEUED_REQUESTS_PROPERTY));
  if (props.containsKey(THREAD_IDLE_MS_PROPERTY))
    this.setThreadIdleTime(props.getLong(THREAD_IDLE_MS_PROPERTY), TimeUnit.MILLISECONDS);
  if (props.containsKey(CONNECTION_TIMEOUT_MS_PROPERTY))
    this.setConnectionTimeout(props.getInt(CONNECTION_TIMEOUT_MS_PROPERTY), TimeUnit.MILLISECONDS);
  if (props.containsKey(SOCKET_TIMEOUT_MS_PROPERTY))
    this.setSocketTimeout(props.getInt(SOCKET_TIMEOUT_MS_PROPERTY), TimeUnit.MILLISECONDS);
  if (props.containsKey(SOCKET_KEEPALIVE_PROPERTY))
    this.setSocketKeepAlive(props.getBoolean(SOCKET_KEEPALIVE_PROPERTY));
  if (props.containsKey(SELECTORS_PROPERTY))
    this.setSelectors(props.getInt(SELECTORS_PROPERTY));
  if (props.containsKey(ROUTING_TIMEOUT_MS_PROPERTY))
    this.setRoutingTimeout(props.getInt(ROUTING_TIMEOUT_MS_PROPERTY), TimeUnit.MILLISECONDS);
  timeoutConfig = new TimeoutConfig(routingTimeoutMs, false);
  if (props.containsKey(GETALL_ROUTING_TIMEOUT_MS_PROPERTY))
    timeoutConfig.setOperationTimeout(VoldemortOpCode.GET_ALL_OP_CODE, props.getInt(GETALL_ROUTING_TIMEOUT_MS_PROPERTY));
  if (props.containsKey(GET_ROUTING_TIMEOUT_MS_PROPERTY))
    timeoutConfig.setOperationTimeout(VoldemortOpCode.GET_OP_CODE, props.getInt(GET_ROUTING_TIMEOUT_MS_PROPERTY));
  if (props.containsKey(PUT_ROUTING_TIMEOUT_MS_PROPERTY))
  {
    long putTimeoutMs = props.getInt(PUT_ROUTING_TIMEOUT_MS_PROPERTY);
    timeoutConfig.setOperationTimeout(VoldemortOpCode.PUT_OP_CODE, putTimeoutMs);
    timeoutConfig.setOperationTimeout(VoldemortOpCode.GET_VERSION_OP_CODE, putTimeoutMs);
  }
  if (props.containsKey(GET_VERSIONS_ROUTING_TIMEOUT_MS_PROPERTY))
    timeoutConfig.setOperationTimeout(VoldemortOpCode.GET_VERSION_OP_CODE, props.getInt(GET_VERSIONS_ROUTING_TIMEOUT_MS_PROPERTY));
  if (props.containsKey(DELETE_ROUTING_TIMEOUT_MS_PROPERTY))
    timeoutConfig.setOperationTimeout(VoldemortOpCode.DELETE_OP_CODE, props.getInt(DELETE_ROUTING_TIMEOUT_MS_PROPERTY));
  if (props.containsKey(ALLOW_PARTIAL_GETALLS_PROPERTY))
    timeoutConfig.setPartialGetAllAllowed(props.getBoolean(ALLOW_PARTIAL_GETALLS_PROPERTY));
  if (props.containsKey(SOCKET_BUFFER_SIZE_PROPERTY))
    this.setSocketBufferSize(props.getInt(SOCKET_BUFFER_SIZE_PROPERTY));
  if (props.containsKey(SERIALIZER_FACTORY_CLASS_PROPERTY))
  {
    Class<?> factoryClass = ReflectUtils.loadClass(props.getString(SERIALIZER_FACTORY_CLASS_PROPERTY));
    SerializerFactory factory = (SerializerFactory) ReflectUtils.callConstructor(factoryClass, new Object[] {
                                                                                                            });
    this.setSerializerFactory(factory);
  }
  if (props.containsKey(BOOTSTRAP_URLS_PROPERTY))
    this.setBootstrapUrls(props.getList(BOOTSTRAP_URLS_PROPERTY));
  if (props.containsKey(REQUEST_FORMAT_PROPERTY))
    this.setRequestFormatType(RequestFormatType.fromCode(props.getString(REQUEST_FORMAT_PROPERTY)));
  if (props.containsKey(ENABLE_JMX_PROPERTY))
    this.setEnableJmx(props.getBoolean(ENABLE_JMX_PROPERTY));
  if (props.containsKey(ENABLE_LAZY_PROPERTY))
    this.setEnableLazy(props.getBoolean(ENABLE_LAZY_PROPERTY));
  if (props.containsKey(ENABLE_PIPELINE_ROUTED_STORE_PROPERTY))
    this.setEnablePipelineRoutedStore(props.getBoolean(ENABLE_PIPELINE_ROUTED_STORE_PROPERTY));
  if (props.containsKey(CLIENT_ZONE_ID))
    this.setClientZoneId(props.getInt(CLIENT_ZONE_ID));
  if (props.containsKey(USE_DEFAULT_CLIENT))
    this.enableDefaultClient(props.getBoolean(USE_DEFAULT_CLIENT));
  if (props.containsKey(FAILUREDETECTOR_IMPLEMENTATION_PROPERTY))
    this.setFailureDetectorImplementation(props.getString(FAILUREDETECTOR_IMPLEMENTATION_PROPERTY));
  if (props.containsKey(NODE_BANNAGE_MS_PROPERTY) && !props.containsKey(FAILUREDETECTOR_BANNAGE_PERIOD_PROPERTY))
  {
    props.put(FAILUREDETECTOR_BANNAGE_PERIOD_PROPERTY, props.get(NODE_BANNAGE_MS_PROPERTY));
  }
  if (props.containsKey(FAILUREDETECTOR_BANNAGE_PERIOD_PROPERTY))
    this.setFailureDetectorBannagePeriod(props.getLong(FAILUREDETECTOR_BANNAGE_PERIOD_PROPERTY));
  if (props.containsKey(FAILUREDETECTOR_THRESHOLD_PROPERTY))
    this.setFailureDetectorThreshold(props.getInt(FAILUREDETECTOR_THRESHOLD_PROPERTY));
  if (props.containsKey(FAILUREDETECTOR_THRESHOLD_COUNTMINIMUM_PROPERTY))
    this.setFailureDetectorThresholdCountMinimum(props.getInt(FAILUREDETECTOR_THRESHOLD_COUNTMINIMUM_PROPERTY));
  if (props.containsKey(FAILUREDETECTOR_THRESHOLD_INTERVAL_PROPERTY))
    this.setFailureDetectorThresholdInterval(props.getLong(FAILUREDETECTOR_THRESHOLD_INTERVAL_PROPERTY));
  if (props.containsKey(FAILUREDETECTOR_ASYNCRECOVERY_INTERVAL_PROPERTY))
    this.setFailureDetectorAsyncRecoveryInterval(props.getLong(FAILUREDETECTOR_ASYNCRECOVERY_INTERVAL_PROPERTY));
  if (props.containsKey(FAILUREDETECTOR_CATASTROPHIC_ERROR_TYPES_PROPERTY))
    this.setFailureDetectorCatastrophicErrorTypes(props.getList(FAILUREDETECTOR_CATASTROPHIC_ERROR_TYPES_PROPERTY));
  if (props.containsKey(FAILUREDETECTOR_REQUEST_LENGTH_THRESHOLD_PROPERTY))
    this.setFailureDetectorRequestLengthThreshold(props.getLong(FAILUREDETECTOR_REQUEST_LENGTH_THRESHOLD_PROPERTY));
  else
    this.setFailureDetectorRequestLengthThreshold(getSocketTimeout(TimeUnit.MILLISECONDS));
  if (props.containsKey(MAX_BOOTSTRAP_RETRIES))
    this.setMaxBootstrapRetries(props.getInt(MAX_BOOTSTRAP_RETRIES));
  if (props.containsKey(CLIENT_CONTEXT_NAME))
  {
    this.setClientContextName(props.getString(CLIENT_CONTEXT_NAME, null));
  }
  if (props.containsKey(ASYNC_CHECK_METADATA_INTERVAL))
  {
    this.setAsyncMetadataRefreshInMs(props.getLong(ASYNC_CHECK_METADATA_INTERVAL, 5000));
  }
}