private Bitmap decodeStream (InputStream stream, Request data) throws IOException
{
  MarkableInputStream markStream = new MarkableInputStream(stream);
  stream = markStream;
  long mark = markStream.savePosition(MARKER);
  final BitmapFactory.Options options = createBitmapOptions(data);
  final boolean calculateSize = requiresInSampleSize(options);
  boolean isWebPFile = Utils.isWebPFile(stream);
  markStream.reset(mark);
  if (isWebPFile)
  {
    byte[] bytes = Utils.toByteArray(stream);
    if (calculateSize)
    {
      BitmapFactory.decodeByteArray(bytes, 0, bytes.length, options);
      calculateInSampleSize(data.targetWidth, data.targetHeight, options);
    }
    else
      ;
    return BitmapFactory.decodeByteArray(bytes, 0, bytes.length, options);
  }
  else
  {
    if (calculateSize)
    {
      BitmapFactory.decodeStream(stream, null, options);
      calculateInSampleSize(data.targetWidth, data.targetHeight, options);
      markStream.reset(mark);
    }
    else
      ;
    return BitmapFactory.decodeStream(stream, null, options);
  }
}