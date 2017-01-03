{
  Story parent = story.getParent();
  Story firstSibling;
  if (parent == null)
  {
    Product prod = backlogBusiness.getParentProduct(story.getBacklog());
    firstSibling = this.retrieveProductRootStories(prod.getId(), null).get(0);
  }
  else
  {
    firstSibling = parent.getChildren().get(0);
  }
  if (firstSibling != null && story != null)
  {
    this.moveBefore(story, firstSibling);
  }
}