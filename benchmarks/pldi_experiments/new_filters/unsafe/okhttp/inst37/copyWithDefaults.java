private OkAuthenticator authenticator;
private ConnectionPool connectionPool;
private CookieHandler cookieHandler;
private Set<Route> failedRoutes = Collections.synchronizedSet(new LinkedHashSet<Route>());
private boolean followProtocolRedirects = true;
private HostnameVerifier hostnameVerifier;
private Proxy proxy;
private ProxySelector proxySelector;
private ResponseCache responseCache;
private SSLSocketFactory sslSocketFactory;
private static final List<String> DEFAULT_TRANSPORTS = Util.immutableList(Arrays.asList("spdy/3", "http/1.1"));
private final Set<Route> failedRoutes;
private List<String> transports;
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
