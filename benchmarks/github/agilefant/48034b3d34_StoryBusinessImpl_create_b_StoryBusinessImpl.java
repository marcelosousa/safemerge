{
  Story persisted = null;
  if ((iterationId != null && iterationId) != 0)
  {
    persisted = this.persistNewStory(dataItem, backlogId, iterationId, responsibleIds);
    storyRankBusiness.rankToHead(persisted, backlogBusiness.retrieve(iterationId));
  }
  else
  {
    persisted = this.persistNewStory(dataItem, backlogId, responsibleIds);
  }
  storyHierarchyBusiness.moveToBottom(persisted);
  storyRankBusiness.rankToHead(persisted, backlogBusiness.retrieve(backlogId));
  this.labelBusiness.createStoryLabels(labelNames, persisted.getId());
  return persisted;
}