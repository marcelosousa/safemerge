{
  for (Versioned<Slop> vs : delivered) {
                                         Slop slop = vs.getValue();
                                         assertEquals("Slop remains.", 0, repo.getSlopStore().get(slop.makeKey()).size());
                                         assertTrue(bytesEqual(slop.getValue(), repo.getNodeStore(STORE_NAME, slop.getNodeId()).get(slop.getKey()).get(0).getValue()));
                                       }
  for (Versioned<Slop> vs : undelivered) {
                                           Slop slop = vs.getValue();
                                           assertEquals("Slop is gone!", 1, repo.getSlopStore().get(slop.makeKey()).size());
                                         }
}