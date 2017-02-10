class SimpleSocketPoolTest{ 
 void testSocketPoolLimitSomeTimeout() {
  SocketServer server = new SocketServer(7666, 50, 50, 1000, new SocketRequestHandlerFactory(null, null, null, null, null, null), "test");
  server.start();
  final ResourcePoolConfig config = new ResourcePoolConfig().setTimeout(50, TimeUnit.MILLISECONDS).setMaxPoolSize(20);
  ResourceFactory<SocketDestination, SocketAndStreams> factory = ResourcePoolTestUtils.getSocketPoolFactory();
  final AbstractSocketPoolTest<SocketDestination, SocketAndStreams> test = new AbstractSocketPoolTest<SocketDestination, SocketAndStreams>()
                                                                           {
                                                                             @Override
                                                                             protected void doSomethingWithResource (SocketDestination key, SocketAndStreams resource) throws Exception
                                                                             {
                                                                               Thread.sleep(100);
                                                                               int random = (int) (Math.random() * 10);
                                                                               if (random >= 5)
                                                                                 resource.getSocket().close();
                                                                             }
                                                                             @Override
                                                                             protected SocketDestination getRequestKey () throws Exception
                                                                             {
                                                                               return new SocketDestination("localhost", 7666, RequestFormatType.VOLDEMORT_V1);
                                                                             }
                                                                           };
  TestStats testStats = test.startTest(factory, config, 50, 200);
  assertEquals("We should see some timeoutRequests", true, (testStats.timeoutRequests > 0));
  server.shutdown();
}
}