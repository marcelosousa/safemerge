{
  Story persisted = null;
  if ((iterationId != null && iterationId) != 0)
  {
    persisted = this.persistNewStory(dataItem, backlogId, iterationId, responsibleIds);
  }
  else
  {
    persisted = this.persistNewStory(dataItem, backlogId, responsibleIds);
  }
  storyHierarchyBusiness.moveToTop(persisted);
  storyRankBusiness.rankToHead(persisted, backlogBusiness.retrieve(backlogId));
  this.labelBusiness.createStoryLabels(labelNames, persisted.getId());
  return persisted;
}