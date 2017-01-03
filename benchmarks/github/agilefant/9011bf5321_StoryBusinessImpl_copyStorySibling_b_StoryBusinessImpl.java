{
  Story refstory = create(story, backlogId, userIds, labelNames);
  refstory.setName(("Copy of " + story.getName()));
  this.labelBusiness.createStoryLabels(labelNames, refstory.getId());
  Set<Label> labels = new HashSet<Label>();
  labels.addAll(story.getLabels());
  Backlog backlog = this.backlogBusiness.retrieve(backlogId);
  this.storyHierarchyBusiness.moveAfter(refstory, story);
  rankStoryUnder(refstory, story, backlog);
  return refstory;
}