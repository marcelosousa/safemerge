{
  Story persisted = this.persistNewStory(dataItem, backlogId, responsibleIds);
  storyHierarchyBusiness.moveToBottom(persisted);
  storyRankBusiness.rankToHead(persisted, backlogBusiness.retrieve(backlogId));
  this.labelBusiness.createStoryLabels(labelNames, persisted.getId());
  return persisted;
}