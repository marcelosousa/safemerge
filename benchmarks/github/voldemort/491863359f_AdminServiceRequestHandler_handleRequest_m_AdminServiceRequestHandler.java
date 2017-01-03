{
  VoldemortAdminRequest.Builder request = VoldemortAdminRequest.newBuilder();
  int size = inputStream.readInt();
  if (logger.isTraceEnabled())
    logger.trace(("In handleRequest, request specified size of " + size + " bytes"));
  if (size < 0)
    throw new IOException("In handleRequest, request specified size of " + size + " bytes");
  byte[] input = new byte[size];
  ByteUtils.read(inputStream, input);
  request.mergeFrom(input);
  switch (request.getType())
  {
    case GET_METADATA:
      ProtoUtils.writeMessage(outputStream, handleGetMetadata(request.getGetMetadata()));
      break;
    case UPDATE_METADATA:
      ProtoUtils.writeMessage(outputStream, handleUpdateMetadata(request.getUpdateMetadata()));
      break;
    case DELETE_PARTITION_ENTRIES:
      ProtoUtils.writeMessage(outputStream, handleDeletePartitionEntries(request.getDeletePartitionEntries()));
      break;
    case FETCH_PARTITION_ENTRIES:
      return handleFetchPartitionEntries(request.getFetchPartitionEntries());
    case UPDATE_PARTITION_ENTRIES:
      return handleUpdatePartitionEntries(request.getUpdatePartitionEntries());
    case INITIATE_FETCH_AND_UPDATE:
      ProtoUtils.writeMessage(outputStream, handleFetchAndUpdate(request.getInitiateFetchAndUpdate()));
      break;
    case ASYNC_OPERATION_STATUS:
      ProtoUtils.writeMessage(outputStream, handleAsyncStatus(request.getAsyncOperationStatus()));
      break;
    case INITIATE_REBALANCE_NODE:
      ProtoUtils.writeMessage(outputStream, handleRebalanceNode(request.getInitiateRebalanceNode()));
      break;
    case INITIATE_REBALANCE_NODE_ON_DONOR:
      ProtoUtils.writeMessage(outputStream, handleRebalanceNodeOnDonor(request.getInitiateRebalanceNodeOnDonor()));
      break;
    case ASYNC_OPERATION_LIST:
      ProtoUtils.writeMessage(outputStream, handleAsyncOperationList(request.getAsyncOperationList()));
      break;
    case ASYNC_OPERATION_STOP:
      ProtoUtils.writeMessage(outputStream, handleAsyncOperationStop(request.getAsyncOperationStop()));
      break;
    case TRUNCATE_ENTRIES:
      ProtoUtils.writeMessage(outputStream, handleTruncateEntries(request.getTruncateEntries()));
      break;
    case ADD_STORE:
      ProtoUtils.writeMessage(outputStream, handleAddStore(request.getAddStore()));
      break;
    case DELETE_STORE:
      ProtoUtils.writeMessage(outputStream, handleDeleteStore(request.getDeleteStore()));
      break;
    case FETCH_STORE:
      ProtoUtils.writeMessage(outputStream, handleFetchStore(request.getFetchStore()));
      break;
    case SWAP_STORE:
      ProtoUtils.writeMessage(outputStream, handleSwapStore(request.getSwapStore()));
      break;
    case ROLLBACK_STORE:
      ProtoUtils.writeMessage(outputStream, handleRollbackStore(request.getRollbackStore()));
      break;
    case GET_RO_MAX_VERSION_DIR:
      ProtoUtils.writeMessage(outputStream, handleGetROMaxVersionDir(request.getGetRoMaxVersionDir()));
      break;
    case GET_RO_CURRENT_VERSION_DIR:
      ProtoUtils.writeMessage(outputStream, handleGetROCurrentVersionDir(request.getGetRoCurrentVersionDir()));
      break;
    case GET_RO_STORAGE_FORMAT:
      ProtoUtils.writeMessage(outputStream, handleGetROStorageFormat(request.getGetRoStorageFormat()));
      break;
    case FETCH_PARTITION_FILES:
      return handleFetchPartitionFiles(request.getFetchPartitionFiles());
    case UPDATE_SLOP_ENTRIES:
      return handleUpdateSlopEntries(request.getUpdateSlopEntries());
    case FAILED_FETCH_STORE:
      ProtoUtils.writeMessage(outputStream, handleFailedFetch(request.getFailedFetchStore()));
      break;
    case REBALANCE_STATE_CHANGE:
      ProtoUtils.writeMessage(outputStream, handleRebalanceStateChange(request.getRebalanceStateChange()));
      break;
    case DELETE_STORE_REBALANCE_STATE:
      ProtoUtils.writeMessage(outputStream, handleDeleteStoreRebalanceState(request.getDeleteStoreRebalanceState()));
      break;
    case REPAIR_JOB:
      ProtoUtils.writeMessage(outputStream, handleRepairJob(request.getRepairJob()));
      break;
    case NATIVE_BACKUP:
      ProtoUtils.writeMessage(outputStream, handleNativeBackup(request.getNativeBackup()));
      break;
    default:
      throw new VoldemortException("Unkown operation " + request.getType());
  }
  return null;
}