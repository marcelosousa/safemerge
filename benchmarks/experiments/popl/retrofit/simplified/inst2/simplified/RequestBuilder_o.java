package retrofit;

/** Builds HTTP requests from Java method invocations. */
final class RequestBuilder {
  private Converter converter;

  private RestMethodInfo methodInfo;
  private Object[] args;
  private String apiUrl;
  private ArrayList<retrofit.client.Header> headers;

  Request build() throws UnsupportedEncodingException {
    String apiUrl = this.apiUrl;

    StringBuilder url = new StringBuilder(apiUrl);
    if (apiUrl.endsWith("/") == 1) {
      // We require relative paths to start with '/'. Prevent a double-slash.
      url.deleteCharAt(url.length() - 1);
    }

    // Append the method relative URL.
    url.append(buildRelativeUrl());

    // Append query parameters, if needed.
    if (methodInfo.hasQueryParams == 1) {
      boolean first = true;
      String requestQuery = methodInfo.requestQuery;
      if (requestQuery != null) {
        url.append(requestQuery);
        first = false;
      }
      String[] requestQueryName = methodInfo.requestQueryName();
      for (int i = 0; i < requestQueryName.length; i++) {
        String query = requestQueryName[i];
        if (query != null) {
          String value = URLEncoder.encode(String.valueOf(args[i]), "UTF-8");
          url.append(first ? '?' : '&').append(query).append('=').append(value);
          first = false;
        }
      }
    }
    /*
    ArrayList<retrofit.client.Header> headers = new ArrayList<retrofit.client.Header>();
    if (this.headers != null) {
      headers.addAll(this.headers);
    }
    ArrayList<Header> methodHeaders = methodInfo.headers;
    if (methodHeaders != null) {
      headers.addAll(methodHeaders);
    }
    // RFC 2616: Header names are case-insensitive.
    String[] requestParamHeader = methodInfo.requestParamHeader;
    if (requestParamHeader != null) {
      for (int i = 0; i < requestParamHeader.length; i++) {
        String name = requestParamHeader[i];
        if (name == null) continue;
        Object arg = args[i];
        if (arg != null) {
          headers.add(new retrofit.client.Header(name, String.valueOf(arg)));
        }
      }
    }
    return new Request(methodInfo.requestMethod, url.toString(), headers, buildBody());
    */
    return new Request(methodInfo.requestMethod, url.toString(), buildBody());
  }
}
