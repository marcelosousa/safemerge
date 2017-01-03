{
  List<MenuDataNode> nodes = new ArrayList<MenuDataNode>();
  List<Product> products = new ArrayList<Product>(productBusiness.retrieveAllOrderByName());
  Collections.sort(products, new PropertyComparator("name", true, true));
  for (Product prod : products) {
                                  nodes.add(constructMenuDataNode(prod));
                                }
  return nodes;
}