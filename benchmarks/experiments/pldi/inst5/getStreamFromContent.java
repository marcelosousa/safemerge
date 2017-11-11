protected static final String ALLOWED_URI_CHARS = "@#&=*+-_.,:!?()/~'%";
protected static final int BUFFER_SIZE = 32 * 1024;
public static final int DEFAULT_HTTP_CONNECT_TIMEOUT = 5 * 1000;
public static final int DEFAULT_HTTP_READ_TIMEOUT = 20 * 1000;
private static final String ERROR_UNSUPPORTED_SCHEME = "UIL doesn't support scheme(protocol) by default [%s]. " + "You should implement this support yourself (BaseImageDownloader.getStreamFromOtherSource(...))";
protected static final int MAX_REDIRECT_COUNT = 5;
protected final int connectTimeout;
protected final Context context;
protected final int readTimeout;
protected static final String CONTENT_CONTACTS_URI_PREFIX = "content://com.android.contacts/";
protected InputStream getStreamFromContent (String imageUri, Object extra) throws FileNotFoundException
{
  ContentResolver res = context.getContentResolver();
  Uri uri = Uri.parse(imageUri);
  if (isVideoUri(uri) == 1)
  {
    Long origId = Long.valueOf(uri.getLastPathSegment());
    Bitmap bitmap = MediaStore.Video.Thumbnails.getThumbnail(res, origId, MediaStore.Images.Thumbnails.MINI_KIND, null);
    if (bitmap != null)
    {
      ByteArrayOutputStream bos = new ByteArrayOutputStream();
      bitmap.compress(CompressFormat.PNG, 0, bos);
      ByteArray bmpData = bos.toByteArray();
      return new ByteArrayInputStream(bmpData);
    }
    else
      ;
  }
  else
    ;
  return res.openInputStream(uri);
  return;
}
private boolean isVideoUri (Uri uri)
{
  String mimeType = context.getContentResolver().getType(uri);
  if (mimeType == null)
  {
    return false;
  }
  return mimeType.startsWith("video/");
}
