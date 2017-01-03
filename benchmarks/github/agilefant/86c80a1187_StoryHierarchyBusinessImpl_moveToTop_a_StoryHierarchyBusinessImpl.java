{
  Story parent = story.getParent();
  Story firstSibling;
  if (parent == null)
  {
    Product prod = backlogBusiness.getParentProduct(story.getBacklog());
    if (prod == null)
    {
      Set<Story> stories = story.getIteration().getStories();
      if (stories.size() == 0)
      {
        firstSibling = null;
      }
      else
      {
        firstSibling = stories.iterator().next();
      }
    }
    else
    {
      firstSibling = this.retrieveProductRootStories(prod.getId(), null).get(0);
    }
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