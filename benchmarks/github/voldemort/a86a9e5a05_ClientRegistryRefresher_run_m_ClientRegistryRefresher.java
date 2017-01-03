{
  if (hadConflict)
  {
    lastVersion = this.systemStoreRepository.getClientRegistryStore().getSysStore(clientId).getVersion();
    hadConflict = false;
  }
  clientInfo.setUpdateTime(System.currentTimeMillis());
  logger.info(("updating client registry with the following info for client: " + clientId + "\n" + clientInfo));
  try
  {
    lastVersion = this.systemStoreRepository.getClientRegistryStore().putSysStore(clientId, new Versioned<String>(clientInfo.toString(), lastVersion));
  }
  catch (ObsoleteVersionException e)
  {
    Versioned<String> existingValue = this.systemStoreRepository.getClientRegistryStore().getSysStore(clientId);
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