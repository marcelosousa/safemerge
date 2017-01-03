{
  Map<Integer, LeafStoryContainer> backlogs = new HashMap<Integer, LeafStoryContainer>();
  ProductTO root = new ProductTO(product);
  root.setChildren(new HashSet<Backlog>());
  backlogs.put(root.getId(), root);
  createBacklogTo(product, backlogs, root);
  List<Story> stories = this.productDAO.retrieveLeafStories(product);
  for (Story story : stories) {
                                final Iteration assignedIteration = story.getIteration();
                                if (assignedIteration != null && !assignedIteration.isStandAlone())
                                {
                                  backlogs.get(assignedIteration.getId()).getLeafStories().add(new StoryTO(story));
                                }
                                else
                                {
                                  backlogs.get(story.getBacklog().getId()).getLeafStories().add(new StoryTO(story));
                                }
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
  List<Iteration> standAloneIterations = new ArrayList<Iteration>(iterationBusiness.retrieveAllStandAloneIterations());
  List<IterationTO> standIter = new ArrayList<IterationTO>();
  for (Iteration iter : standAloneIterations) {
                                                IterationTO iterTo = transferObjectBusiness.constructIterationTO(iter);
                                                List<Story> standAloneStories = storyBusiness.retrieveStoriesInIteration(iter);
                                                List<StoryTO> standAloneStoriesTo = new ArrayList<StoryTO>();
                                                for (Story s : standAloneStories) {
                                                                                    standAloneStoriesTo.add(transferObjectBusiness.constructStoryTO(s));
                                                                                  }
                                                iterTo.setRankedStories(standAloneStoriesTo);
                                                standIter.add(iterTo);
                                              }
  root.setStandaloneIterations(standIter);
  return root;
}