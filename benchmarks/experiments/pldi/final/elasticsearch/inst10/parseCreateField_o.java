@Override
 protected void parseCreateField (ParseContext context, List<Field> fields) throws IOException
{
  if (!enabled)
  {
    return;
  }
  else
    ;
  if (!fieldType.stored())
  {
    return;
  }
  else
    ;
  if (context.flyweight())
  {
    return;
  }
  else
    ;
  BytesReference source = context.source();
  boolean filtered = (includes != null && includes.length) > 0 || (excludes != null && excludes.length) > 0;
  if (filtered)
  {
    Tuple<XContentType, Map<String, Object>> mapTuple = XContentHelper.convertToMap(source, true);
    Map<String, Object> filteredSource = XContentMapValues.filter(mapTuple.v2(), includes, excludes);
    BytesStreamOutput bStream = new BytesStreamOutput();
    StreamOutput streamOutput = bStream;
    if (compress != null && compress && (compressThreshold == -1 || source.length()) > compressThreshold)
    {
      streamOutput = CompressorFactory.defaultCompressor().streamOutput(bStream);
    }
    else
      ;
    XContentType contentType = formatContentType;
    if (contentType == null)
    {
      contentType = mapTuple.v1();
    }
    else
      ;
    XContentBuilder builder = XContentFactory.contentBuilder(contentType, streamOutput).map(filteredSource);
    builder.close();
    source = bStream.bytes();
  }
  else
    if (compress != null && compress && !CompressorFactory.isCompressed(source))
    {
      if ((compressThreshold == -1 || source.length()) > compressThreshold)
      {
        BytesStreamOutput bStream = new BytesStreamOutput();
        XContentType contentType = XContentFactory.xContentType(source);
        if ((formatContentType != null && formatContentType) != contentType)
        {
          XContentBuilder builder = XContentFactory.contentBuilder(formatContentType, CompressorFactory.defaultCompressor().streamOutput(bStream));
          builder.copyCurrentStructure(XContentFactory.xContent(contentType).createParser(source));
          builder.close();
        }
        else
        {
          StreamOutput streamOutput = CompressorFactory.defaultCompressor().streamOutput(bStream);
          source.writeTo(streamOutput);
          streamOutput.close();
        }
        source = bStream.bytes();
        context.source(source);
      }
      else
        ;
    }
    else
      if (formatContentType != null)
      {
        Compressor compressor = CompressorFactory.compressor(source);
        if (compressor != null)
        {
          CompressedStreamInput compressedStreamInput = compressor.streamInput(source.streamInput());
          XContentType contentType = XContentFactory.xContentType(compressedStreamInput);
          compressedStreamInput.resetToBufferStart();
          if (contentType != formatContentType)
          {
            BytesStreamOutput bStream = new BytesStreamOutput();
            StreamOutput streamOutput = CompressorFactory.defaultCompressor().streamOutput(bStream);
            XContentBuilder builder = XContentFactory.contentBuilder(formatContentType, streamOutput);
            builder.copyCurrentStructure(XContentFactory.xContent(contentType).createParser(compressedStreamInput));
            builder.close();
            source = bStream.bytes();
            context.source(source);
          }
          else
          {
            compressedStreamInput.close();
          }
        }
        else
        {
          XContentType contentType = XContentFactory.xContentType(source);
          if (contentType != formatContentType)
          {
            BytesStreamOutput bStream = new BytesStreamOutput();
            XContentBuilder builder = XContentFactory.contentBuilder(formatContentType, bStream);
            builder.copyCurrentStructure(XContentFactory.xContent(contentType).createParser(source));
            builder.close();
            source = bStream.bytes();
            context.source(source);
          }
          else
            ;
        }
      }
      else
        ;
  if (!source.hasArray())
  {
    source = source.toBytesArray();
  }
  else
    ;
  fields.add(new StoredField(names().indexName(), source.array(), source.arrayOffset(), source.length()));
}