private static final int KEEP_ALIVE_DURATION_MS = 5000;
private Connection httpA;
private Address httpAddress;
private Connection httpB;
private Connection httpC;
private Connection httpD;
private Connection httpE;
private MockWebServer httpServer;
private InetSocketAddress httpSocketAddress;
private Object owner;
private ConnectionPool pool;
private Connection spdyA;
private Address spdyAddress;
private MockWebServer spdyServer;
private InetSocketAddress spdySocketAddress;
private static final SSLContext sslContext = SslContextBuilder.localhost();
private FakeExecutor cleanupExecutor;
@Test
 public void poolSingleHttpConnection () throws Exception
{
  resetWithPoolSize(1);
  Connection connection = pool.get(httpAddress);
  assertNull(connection);
  connection = new Connection(pool, new Route(httpAddress, Proxy.NO_PROXY, httpSocketAddress, ConnectionSpec.CLEARTEXT));
  connection.connect(200, 200, 200, null);
  connection.setOwner(owner);
  assertEquals(0, pool.getConnectionCount());
  pool.recycle(connection);
  assertNull(connection.getOwner());
  assertEquals(1, pool.getConnectionCount());
  assertEquals(1, pool.getHttpConnectionCount());
  assertEquals(0, pool.getMultiplexedConnectionCount());
  Connection recycledConnection = pool.get(httpAddress);
  assertNull(connection.getOwner());
  assertEquals(connection, recycledConnection);
  assertTrue(recycledConnection.isAlive());
  recycledConnection = pool.get(httpAddress);
  assertNull(recycledConnection);
  return;
}
