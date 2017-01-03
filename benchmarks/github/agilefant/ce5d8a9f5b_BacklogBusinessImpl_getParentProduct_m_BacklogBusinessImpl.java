{
  if (backlog instanceof Product)
  {
    return (Product) backlog;
  }
  Backlog parent = backlog;
  if ((backlog == null || backlog.getParent()) == null && !(backlog instanceof Product))
  {
    return null;
  }
  else
  {
    while (!(parent instanceof Product))
    {
      if (parent == null)
      {
        return null;
      }
      if (parent instanceof Iteration && parent.isStandAlone())
      {
        return null;
      }
      parent = parent.getParent();
    }
    return (Product) parent;
  }
}