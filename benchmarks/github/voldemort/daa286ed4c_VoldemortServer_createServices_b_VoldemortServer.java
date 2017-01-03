{
  List<VoldemortService> services = new ArrayList<VoldemortService>();
  SchedulerService scheduler = new SchedulerService(voldemortConfig.getSchedulerThreads(), SystemTime.INSTANCE);
  StorageService storageService = new StorageService(storeRepository, metadata, scheduler, voldemortConfig);
  asyncRunner = new AsyncOperationRunner(scheduler, ASYNC_REQUEST_CACHE_SIZE);
  services.add(storageService);
  services.add(scheduler);
  services.add(asyncRunner);
  if (voldemortConfig.isHttpServerEnabled())
    services.add(new HttpService(this, storageService, storeRepository, RequestFormatType.VOLDEMORT_V1, voldemortConfig.getMaxThreads(), identityNode.getHttpPort()));
  if (voldemortConfig.isSocketServerEnabled())
  {
    RequestHandlerFactory socketRequestHandlerFactory = new SocketRequestHandlerFactory(storageService, this.storeRepository, this.metadata, this.voldemortConfig, this.asyncRunner, null);
    if (voldemortConfig.getUseNioConnector())
    {
      logger.info("Using NIO Connector.");
      services.add(new NioSocketService(socketRequestHandlerFactory, identityNode.getSocketPort(), voldemortConfig.getSocketBufferSize(), voldemortConfig.getNioConnectorSelectors(), "nio-socket-server", voldemortConfig.isJmxEnabled()));
    }
    else
    {
      logger.info("Using BIO Connector.");
      services.add(new SocketService(socketRequestHandlerFactory, identityNode.getSocketPort(), voldemortConfig.getCoreThreads(), voldemortConfig.getMaxThreads(), voldemortConfig.getSocketBufferSize(), "socket-server", voldemortConfig.isJmxEnabled()));
    }
  }
  if (voldemortConfig.isAdminServerEnabled())
  {
    Rebalancer rebalancer = null;
    if (voldemortConfig.isEnableRebalanceService())
    {
      RebalancerService rebalancerService = new RebalancerService(metadata, voldemortConfig, asyncRunner, scheduler);
      services.add(rebalancerService);
      rebalancer = rebalancerService.getRebalancer();
    }
    SocketRequestHandlerFactory adminRequestHandlerFactory = new SocketRequestHandlerFactory(storageService, this.storeRepository, this.metadata, this.voldemortConfig, this.asyncRunner, rebalancer);
    services.add(new AdminService(adminRequestHandlerFactory, identityNode.getAdminPort(), voldemortConfig.getAdminCoreThreads(), voldemortConfig.getAdminMaxThreads(), voldemortConfig.getAdminSocketBufferSize(), "admin-server", voldemortConfig.isJmxEnabled()));
  }
  if (voldemortConfig.isGossipEnabled())
  {
    services.add(new GossipService(this.metadata, scheduler, voldemortConfig));
  }
  if (voldemortConfig.isJmxEnabled())
    services.add(new JmxService(this, this.metadata.getCluster(), storeRepository, services));
  return ImmutableList.copyOf(services);
}