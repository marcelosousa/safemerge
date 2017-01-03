{
  Story persisted = this.persistNewStory(dataItem, backlogId, responsibleIds);
  storyHierarchyBusiness.moveToTop(persisted);
  storyRankBusiness.rankToHead(persisted, backlogBusiness.retrieve(backlogId));
  this.labelBusiness.createStoryLabels(labelNames, persisted.getId());
  return persisted;
}