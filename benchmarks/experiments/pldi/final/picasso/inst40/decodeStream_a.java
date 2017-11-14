private Bitmap decodeStream (InputStream stream, Request data) throws IOException
{
  MarkableInputStream markStream = new MarkableInputStream(stream);
  stream = markStream;
  long mark = markStream.savePosition(MARKER);
  boolean isWebPFile = Utils.isWebPFile(stream);
  markStream.reset(mark);
  if (isWebPFile)
  {
    byte[] bytes = Utils.toByteArray(stream);
    BitmapFactory.Options options = createBitmapOptions(data);
    if (data.hasSize())
    {
      options.inJustDecodeBounds = true;
      BitmapFactory.decodeByteArray(bytes, 0, bytes.length, options);
      calculateInSampleSize(data.targetWidth, data.targetHeight, options);
    }
    else
      ;
    return BitmapFactory.decodeByteArray(bytes, 0, bytes.length, options);
  }
  else
  {
    BitmapFactory.Options options = createBitmapOptions(data);
    if (data.hasSize())
    {
      options.inJustDecodeBounds = true;
      BitmapFactory.decodeStream(stream, null, options);
      calculateInSampleSize(data.targetWidth, data.targetHeight, options);
      markStream.reset(mark);
    }
    else
      ;
    return BitmapFactory.decodeStream(stream, null, options);
  }
}