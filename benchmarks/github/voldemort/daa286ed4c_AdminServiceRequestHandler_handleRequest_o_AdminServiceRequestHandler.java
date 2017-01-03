{
  VoldemortAdminRequest.Builder request = VoldemortAdminRequest.newBuilder();
  int size = inputStream.readInt();
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
      handleFetchPartitionEntries(request.getFetchPartitionEntries(), outputStream);
      break;
    case UPDATE_PARTITION_ENTRIES:
      handleUpdatePartitionEntries(request.getUpdatePartitionEntries(), inputStream, outputStream);
      break;
    case INITIATE_FETCH_AND_UPDATE:
      ProtoUtils.writeMessage(outputStream, handleFetchAndUpdate(request.getInitiateFetchAndUpdate()));
      break;
    case ASYNC_OPERATION_STATUS:
      ProtoUtils.writeMessage(outputStream, handleAsyncStatus(request.getAsyncOperationStatus()));
      break;
    case INITIATE_REBALANCE_NODE:
      ProtoUtils.writeMessage(outputStream, handleRebalanceNode(request.getInitiateRebalanceNode()));
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
    default:
      throw new VoldemortException("Unkown operation " + request.getType());
  }
}