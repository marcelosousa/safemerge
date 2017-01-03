{
  story = this.retrieve(storyId);
  Backlog backlog = this.backlogBusiness.retrieve(story.getBacklog().getId());
  if (backlog == null)
  {
    throw new ObjectNotFoundException("backlog.notFound");
  }
  Story newStory = new Story(story);
  newStory.setName(("Copy of " + newStory.getName()));
  for (Task t : newStory.getTasks()) taskBusiness.store(t);
  newStory.setBacklog(backlog);
  create(newStory);
  return newStory;
}