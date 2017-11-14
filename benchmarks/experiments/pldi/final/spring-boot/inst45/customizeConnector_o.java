protected void customizeConnector (Connector connector)
{
  int port = getPort() >= 0 ? getPort() : 0;
  connector.setPort(port);
  if (connector.getProtocolHandler() instanceof AbstractProtocol)
  {
    if (getAddress() != null)
    {
      ((AbstractProtocol) connector.getProtocolHandler()).setAddress(getAddress());
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