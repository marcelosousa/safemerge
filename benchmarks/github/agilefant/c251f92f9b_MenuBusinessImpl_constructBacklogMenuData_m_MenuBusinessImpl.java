{
  List<MenuDataNode> nodes = new ArrayList<MenuDataNode>();
  List<Product> products = new ArrayList<Product>(productBusiness.retrieveAllOrderByName());
  Collections.sort(products, new PropertyComparator("name", true, true));
  Collection<Product> allowedProducts = new HashSet<Product>();
  for (Team team : user.getTeams()) {
                                      allowedProducts.addAll(team.getProducts());
                                    }
  for (Product prod : products) {
                                  if (allowedProducts.contains(prod))
                                  {
                                    nodes.add(constructMenuDataNode(prod));
                                  }
                                }
  final List<Iteration> standAloneIterations = new ArrayList<Iteration>(iterationBusiness.retrieveAllStandAloneIterations());
  for (Iteration iteration : standAloneIterations) {
                                                     nodes.add(constructMenuDataNode(iteration));
                                                   }
  return nodes;
}