{
  expect(productBusiness.retrieveAllOrderByName()).andReturn(products);
  expect(transferObjectBusiness.getBacklogScheduleStatus(isA(Backlog.class))).andReturn(ScheduleStatus.FUTURE).times(8);
  replayAll();
  List<MenuDataNode> actual = menuBusiness.constructBacklogMenuData();
  verifyAll();
  assertEquals(2, actual.size());
  checkProducts(actual);
}