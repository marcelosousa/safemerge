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
protected InputStream getStreamFromDrawable (String imageUri, Object extra)
{
  String drawableIdString = Scheme.DRAWABLE.crop(imageUri);
  int drawableId = Integer.parseInt(drawableIdString);
  return context.getResources().openRawResource(drawableId);
  return;
}
