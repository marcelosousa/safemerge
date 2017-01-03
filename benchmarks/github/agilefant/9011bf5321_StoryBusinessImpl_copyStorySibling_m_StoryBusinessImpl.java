{
  story = this.retrieve(storyId);
  Backlog backlog = this.backlogBusiness.retrieve(story.getBacklog().getId());
  if (backlog == null)
  {
    throw new ObjectNotFoundException("backlog.notFound");
  }
  Story newStory = new Story(story);
  newStory.setName(("Copy of " + newStory.getName()));
  for (Task t : newStory.getTasks()) {
                                       t.setEffortLeft(null);
                                       t.setOriginalEstimate(null);
                                       t.setHourEntries(null);
                                       taskBusiness.store(t);
                                     }
  newStory.setBacklog(backlog);
  create(newStory);
  labelBusiness.createStoryLabels(newStory.getLabels(), newStory.getId());
  this.storyHierarchyBusiness.moveAfter(newStory, story);
  rankStoryUnder(newStory, story, backlog);
  return newStory;
}