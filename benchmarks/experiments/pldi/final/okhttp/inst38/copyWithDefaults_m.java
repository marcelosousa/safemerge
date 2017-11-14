private OkHttpClient copyWithDefaults ()
{
  OkHttpClient result = new OkHttpClient(this);
  result.proxy = proxy;
  result.proxySelector = proxySelector != null ? proxySelector : ProxySelector.getDefault();
  result.cookieHandler = cookieHandler != null ? cookieHandler : CookieHandler.getDefault();
  result.responseCache = responseCache != null ? responseCache : ResponseCache.getDefault();
  result.sslSocketFactory = sslSocketFactory != null ? sslSocketFactory : HttpsURLConnection.getDefaultSSLSocketFactory();
  result.hostnameVerifier = hostnameVerifier != null ? hostnameVerifier : new OkHostnameVerifier();
  result.authenticator = authenticator != null ? authenticator : HttpAuthenticator.SYSTEM_DEFAULT;
  result.connectionPool = connectionPool != null ? connectionPool : ConnectionPool.getDefault();
  result.followProtocolRedirects = followProtocolRedirects;
  result.transports = transports != null ? transports : DEFAULT_TRANSPORTS;
  return result;
}