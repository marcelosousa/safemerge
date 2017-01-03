{
  if (hadConflict)
  {
    lastVersion = clientRegistry.getSysStore(clientId).getVersion();
    hadConflict = false;
  }
  clientInfo.setUpdateTime(System.currentTimeMillis());
  logger.info(("updating client registry with the following info for client: " + clientId + "\n" + clientInfo));
  try
  {
    lastVersion = clientRegistry.putSysStore(clientId, new Versioned<ClientInfo>(clientInfo, lastVersion));
  }
  catch (ObsoleteVersionException e)
  {
    Versioned<ClientInfo> existingValue = clientRegistry.getSysStore(clientId);
    logger.warn("Multiple clients are updating the same client registry entry");
    logger.warn(("  current value: " + clientInfo + " " + lastVersion));
    logger.warn(("  existing value: " + existingValue.getValue() + " " + existingValue.getVersion()));
    hadConflict = true;
  }
  catch (Exception e)
  {
    logger.warn(("encountered the following error while trying to update client registry: " + e));
  }
}