package retrofit;

public class RestAdapter {
  private String THREAD_PREFIX = "Retrofit-";
  private String IDLE_THREAD_NAME = THREAD_PREFIX + "Idle";

  private Server server;
  private Client.Provider clientProvider;
  private Executor httpExecutor;
  private Executor callbackExecutor;
  private RequestInterceptor requestInterceptor;
  private Converter converter;
  private Profiler profiler;
  private ErrorHandler errorHandler;
  private Log log;
  private LogLevel logLevel;

  /** Log request headers and body. Consumes request body and returns identical replacement. */
  private Request logAndReplaceRequest(Request request) throws IOException {
    log.log(String.format("---> HTTP %s %s", request.getMethod(), request.getUrl()));

    if (logLevel.ordinal() >= LogLevel.HEADERS.ordinal()) {
      Header header;
      for (int i = 0; i < request.size(); i++) {
        header = request.getHeader(i);
        log.log(header.toString());
      }

      long bodySize = 0;
      TypedOutput body = request.getBody();
      if (body != null) {
        bodySize = body.length();
        String bodyMime = body.mimeType();

        if (bodyMime != null) {
          log.log("Content-Type: " + bodyMime);
        }
        if (bodySize != -1) {
          log.log("Content-Length: " + bodySize);
        }

        if (logLevel.ordinal() >= LogLevel.FULL.ordinal()) {
          if (request.getHeaders().isEmpty() == 0) {
            log.log("");
          }
          if (instanceOfTypeByteArray(body) == 1) {
            // Read the entire response body to we can log it and replace the original response
            request = Utils.readBodyToBytesIfNecessary(request);
            body = request.getBody();
          }

          ArrayOfByte bodyBytes = body.getBytes();
          bodySize = bodyBytes.length();
          String bodyCharset = MimeUtil.parseCharset(bodyMime);
          String bodyString = new String(bodyBytes, bodyCharset);
          int len = bodyString.length();
          int end = 0;
          for (int i = 0; i < len; i += 4000) {
            end = Math.min(len, i + 4000);
            log.log(bodyString.substring(i, end));
          }
        }
      }

      log.log(String.format("---> END HTTP (%s-byte body)", bodySize));
    }

    return request;
  }
}
