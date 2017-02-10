class ClientRegistryRefresher{ 
 void run() {
  clientInfo.setUpdateTime(System.currentTimeMillis());
  logger.info(("updating client registry with the following info for client: " + clientId + "\n" + clientInfo));
  try
  {
    lastVersion = clientRegistry.putSysStore(clientId, new Versioned<String>(clientInfo.toString(), lastVersion));
  }
  catch (Exception e)
  {
    logger.warn(("encounted the following error while trying to update client registry: " + e));
  }
}
}