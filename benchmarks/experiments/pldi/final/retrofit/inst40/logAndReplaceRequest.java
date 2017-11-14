static final String IDLE_THREAD_NAME = THREAD_PREFIX + "Idle";
private static final int LOG_CHUNK_SIZE = 4000;
static final String THREAD_PREFIX = "Retrofit-";
private final Executor callbackExecutor;
private final Client.Provider clientProvider;
private final Converter converter;
private final ErrorHandler errorHandler;
private final Executor httpExecutor;
private final Log log;
private volatile LogLevel logLevel;
private final Profiler profiler;
private final RequestInterceptor requestInterceptor;
private final Server server;
private Request logAndReplaceRequest (Request request) throws IOException
{
  log.log(String.format("---> HTTP %s %s", request.getMethod(), request.getUrl()));
  if (logLevel.ordinal() >= LogLevel.HEADERS.ordinal())
  {
    {
      int wiz_i = 0;
      Header header = request.getHeaders().get(wiz_i);
      while (wiz_i < request.getHeaders().length())
      {
        {
          log.log(header.toString());
        }
        wiz_i++;
      }
    }
    long bodySize = 0;
    TypedOutput body = request.getBody();
    if (body != null)
    {
      bodySize = body.length();
      String bodyMime = body.mimeType();
      if (bodyMime != null)
      {
        log.log(("Content-Type: " + bodyMime));
      }
      else
        ;
      if (bodySize != -1)
      {
        log.log(("Content-Length: " + bodySize));
      }
      else
        ;
      if (logLevel.ordinal() >= LogLevel.FULL.ordinal())
      {
        if (!request.getHeaders().isEmpty())
        {
          log.log("");
        }
        else
          ;
        if (!(body instanceof TypedByteArray))
        {
          request = Utils.readBodyToBytesIfNecessary(request);
          body = request.getBody();
        }
        else
          ;
        byte[] bodyBytes = ((TypedByteArray) body).getBytes();
        bodySize = bodyBytes.length;
        String bodyCharset = MimeUtil.parseCharset(bodyMime);
        log.log(new String(bodyBytes, bodyCharset));
      }
      else
        ;
    }
    else
      ;
    log.log(String.format("---> END HTTP (%s-byte body)", bodySize));
  }
  else
    ;
  return request;
}
