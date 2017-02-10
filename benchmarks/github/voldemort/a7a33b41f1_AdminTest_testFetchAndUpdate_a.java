class AdminTest{ 
 void testFetchAndUpdate() {
  for (final Integer node : from.keySet()) {
                                             timeFunction(new Timed()
                                                          {
                                                            public void apply ()
                                                            {
                                                              adminClient.fetchAndUpdateStreams(node, to, storeName, new ArrayList<Integer>(from.get(node)), null);
                                                            }
                                                          }, 1);
                                           }
}
}