{
  Map<Integer, LeafStoryContainer> backlogs = new HashMap<Integer, LeafStoryContainer>();
  ProductTO root = new ProductTO(product);
  root.setChildren(new HashSet<Backlog>());
  backlogs.put(root.getId(), root);
  createBacklogTo(product, backlogs, root);
  List<Story> stories = this.productDAO.retrieveLeafStories(product);
  for (Story story : stories) {
                                final Iteration assignedIteration = story.getIteration();
                                int backlog_id;
                                if (assignedIteration != null && !assignedIteration.isStandAlone())
                                {
                                  backlog_id = assignedIteration.getId();
                                }
                                else
                                {
                                  backlog_id = story.getBacklog().getId();
                                }
                                backlogs.get(backlog_id).getLeafStories().add(new StoryTO(story));
                              }
  Comparator<Scheduled> backlogComparator = new Comparator<Scheduled>()
                                            {
                                              private Comparator<Scheduled> inner = new PropertyComparator("startDate", true, false);
                                              public int compare (Scheduled o1, Scheduled o2)
                                              {
                                                if (o1 == null)
                                                {
                                                  return -1;
                                                }
                                                if (o2 == null)
                                                {
                                                  return 1;
                                                }
                                                if (o1.getScheduleStatus() != o2.getScheduleStatus())
                                                {
                                                  if (o1.getScheduleStatus().ordinal() < o2.getScheduleStatus().ordinal())
                                                  {
                                                    return 1;
                                                  }
                                                  else
                                                  {
                                                    return -1;
                                                  }
                                                }
                                                else
                                                {
                                                  return this.inner.compare(o1, o2);
                                                }
                                              }
                                            };
  for (ProjectTO project : root.getChildProjects()) {
                                                      Collections.sort(project.getChildIterations(), backlogComparator);
                                                    }
  Collections.sort(root.getChildProjects(), backlogComparator);
  Comparator<Story> comparator = new StoryComparator();
  for (LeafStoryContainer container : backlogs.values()) {
                                                           Collections.sort(container.getLeafStories(), comparator);
                                                         }
  return root;
}