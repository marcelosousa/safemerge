private static final String DEFAULT_PROTOCOL = "org.apache.coyote.http11.Http11NioProtocol";
private List<Connector> additionalTomcatConnectors = new ArrayList<Connector>();
private File baseDirectory;
private List<LifecycleListener> contextLifecycleListeners = new ArrayList<LifecycleListener>();
private List<Valve> contextValves = new ArrayList<Valve>();
private String protocol = DEFAULT_PROTOCOL;
private ResourceLoader resourceLoader;
private String tldSkip;
private List<TomcatConnectorCustomizer> tomcatConnectorCustomizers = new ArrayList<TomcatConnectorCustomizer>();
private List<TomcatContextCustomizer> tomcatContextCustomizers = new ArrayList<TomcatContextCustomizer>();
private String uriEncoding = "UTF-8";
protected void customizeConnector (Connector connector)
{
  int port = getPort() >= 0 ? getPort() : 0;
  connector.setPort(port);
  if (connector.getProtocolHandler() instanceof AbstractProtocol)
  {
    if (getAddress() != null)
    {
      ((AbstractProtocol<?>) connector.getProtocolHandler()).setAddress(getAddress());
    }
    else
      ;
  }
  else
    ;
  if (getUriEncoding() != null)
  {
    connector.setURIEncoding(getUriEncoding());
  }
  else
    ;
  connector.setProperty("bindOnInit", "false");
  if (getSsl() != null)
  {
    Assert.state((connector.getProtocolHandler() instanceof AbstractHttp11JsseProtocol), ("To use SSL, the connector's protocol handler must be an " + "AbstractHttp11JsseProtocol subclass"));
    configureSsl(((AbstractHttp11JsseProtocol<?>) connector.getProtocolHandler()), getSsl());
    connector.setScheme("https");
    connector.setSecure(true);
  }
  else
    ;
  {
    int wiz_i = 0;
    TomcatConnectorCustomizer customizer = this.tomcatConnectorCustomizers.get(wiz_i);
    while (wiz_i < this.tomcatConnectorCustomizers.length())
    {
      {
        customizer.customize(connector);
      }
      wiz_i++;
    }
  }
  return;
}
protected void configureSsl (AbstractHttp11JsseProtocol protocol, Ssl ssl)
{
  protocol.setSSLEnabled(true);
  protocol.setSslProtocol(ssl.getProtocol());
  configureSslClientAuth(protocol, ssl);
  protocol.setKeystorePass(ssl.getKeyStorePassword());
  protocol.setKeyPass(ssl.getKeyPassword());
  protocol.setKeyAlias(ssl.getKeyAlias());
  configureSslKeyStore(protocol, ssl);
  String ciphers = StringUtils.arrayToCommaDelimitedString(ssl.getCiphers());
  protocol.setCiphers(ciphers);
  configureSslTrustStore(protocol, ssl);
}
protected void configureSsl (AbstractHttp11JsseProtocol<?> protocol, Ssl ssl)
{
  protocol.setSSLEnabled(true);
  protocol.setSslProtocol(ssl.getProtocol());
  configureSslClientAuth(protocol, ssl);
  protocol.setKeystorePass(ssl.getKeyStorePassword());
  protocol.setKeyPass(ssl.getKeyPassword());
  protocol.setKeyAlias(ssl.getKeyAlias());
  configureSslKeyStore(protocol, ssl);
  String ciphers = StringUtils.arrayToCommaDelimitedString(ssl.getCiphers());
  protocol.setCiphers(ciphers);
  configureSslTrustStore(protocol, ssl);
}
