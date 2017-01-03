{
  List<MenuDataNode> nodes = new ArrayList<MenuDataNode>();
  List<Product> products = new ArrayList<Product>(productBusiness.retrieveAllOrderByName());
  Collections.sort(products, new PropertyComparator("name", true, true));
  for (Product prod : products) {
                                  nodes.add(constructMenuDataNode(prod));
                                }
  final List<Iteration> standAloneIterations = new ArrayList<Iteration>(iterationBusiness.retrieveAllStandAloneIterations());
  for (Iteration iteration : standAloneIterations) {
                                                     nodes.add(constructMenuDataNode(iteration));
                                                   }
  return nodes;
}