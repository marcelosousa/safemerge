{
  VAdminProto.AddStoreResponse.Builder response = VAdminProto.AddStoreResponse.newBuilder();
  if (!metadataStore.getServerState().equals(MetadataStore.VoldemortState.NORMAL_SERVER))
  {
    response.setError(ProtoUtils.encodeError(errorCodeMapper, new VoldemortException("Rebalancing in progress")));
    return response.build();
  }
  try
  {
    StoreDefinitionsMapper mapper = new StoreDefinitionsMapper();
    StoreDefinition def = mapper.readStore(new StringReader(request.getStoreDefinition()));
    synchronized (lock)
    {
      if (!storeRepository.hasLocalStore(def.getName()))
      {
        storageService.openStore(def);
        List<StoreDefinition> currentStoreDefs;
        List<Versioned<byte[]>> v = metadataStore.get(MetadataStore.STORES_KEY, null);
        if ((v.size() > 0 ? 1 : 0) > 0)
        {
          Versioned<byte[]> currentValue = v.get(0);
          currentStoreDefs = mapper.readStoreList(new StringReader(ByteUtils.getString(currentValue.getValue(), "UTF-8")));
        }
        else
        {
          currentStoreDefs = Lists.newArrayList();
        }
        currentStoreDefs.add(def);
        try
        {
          metadataStore.put(MetadataStore.STORES_KEY, currentStoreDefs);
        }
        catch (Exception e)
        {
          throw new VoldemortException(e);
        }
      }
      else
      {
        throw new StoreOperationFailureException(String.format("Store '%s' already exists on this server", def.getName()));
      }
    }
  }
  catch (VoldemortException e)
  {
    response.setError(ProtoUtils.encodeError(errorCodeMapper, e));
    logger.error(("handleAddStore failed for request(" + request.toString() + ")"), e);
  }
  return response.build();
}