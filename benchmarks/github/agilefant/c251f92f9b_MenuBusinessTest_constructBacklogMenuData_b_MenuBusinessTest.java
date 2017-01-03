{
  expect(productBusiness.retrieveAllOrderByName()).andReturn(products);
  final ArrayList<Iteration> emptyStandAloneIterations = new ArrayList<Iteration>();
  expect(iterationBusiness.retrieveAllStandAloneIterations()).andReturn(emptyStandAloneIterations);
  expect(transferObjectBusiness.getBacklogScheduleStatus(isA(Backlog.class))).andReturn(ScheduleStatus.FUTURE).times(8);
  replayAll();
  List<MenuDataNode> actual = menuBusiness.constructBacklogMenuData();
  verifyAll();
  assertEquals(2, actual.size());
  checkProducts(actual);
}