{
  User user = new User();
  user.setId(10);
  SecurityUtil.setLoggedUser(user);
  Team team = new Team();
  Collection<User> users = new ArrayList<User>();
  users.add(user);
  team.setUsers(users);
  Collection<Team> teams = new ArrayList<Team>();
  teams.add(team);
  user.setTeams(teams);
  team.setProducts(products);
  expect(productBusiness.retrieveAllOrderByName()).andReturn(products);
  final ArrayList<Iteration> emptyStandAloneIterations = new ArrayList<Iteration>();
  expect(iterationBusiness.retrieveAllStandAloneIterations()).andReturn(emptyStandAloneIterations);
  expect(transferObjectBusiness.getBacklogScheduleStatus(isA(Backlog.class))).andReturn(ScheduleStatus.FUTURE).times(8);
  replayAll();
  List<MenuDataNode> actual = menuBusiness.constructBacklogMenuData(user);
  verifyAll();
  assertEquals(2, actual.size());
  checkProducts(actual);
}