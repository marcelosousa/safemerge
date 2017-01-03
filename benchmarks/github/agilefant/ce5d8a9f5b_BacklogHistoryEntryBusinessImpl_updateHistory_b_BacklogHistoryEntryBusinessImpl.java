{
  Backlog backlog = backlogDAO.get(backlogId);
  Project project = null;
  if (backlog instanceof Iteration)
  {
    if (backlog.isStandAlone())
    {
      DateTime currentTime = new DateTime();
      BacklogHistoryEntry entry = backlogHistoryEntryDAO.retrieveLatest(currentTime, backlog.getId());
      if (entry == null || entry.getTimestamp().isBefore(currentTime.minus(BacklogHistoryEntryBusiness.UPDATE_INTERVAL)))
      {
        entry = new BacklogHistoryEntry();
      }
      entry.setTimestamp(new DateTime());
      entry.setDoneSum(storyHierarchyDAO.totalLeafDoneStoryPoints(((Iteration) backlog)));
      entry.setEstimateSum(storyHierarchyDAO.totalLeafStoryPoints(((Iteration) backlog)));
      entry.setRootSum(entry.getEstimateSum());
      entry.setBacklog(backlog);
      backlogHistoryEntryDAO.store(entry);
      return;
    }
    project = (Project) backlog.getParent();
  }
  else
    if (backlog instanceof Product)
    {
      return;
    }
    else
    {
      project = (Project) backlog;
    }
  DateTime currentTime = new DateTime();
  BacklogHistoryEntry entry = backlogHistoryEntryDAO.retrieveLatest(currentTime, project.getId());
  if (entry == null || entry.getTimestamp().isBefore(currentTime.minus(BacklogHistoryEntryBusiness.UPDATE_INTERVAL)))
  {
    entry = new BacklogHistoryEntry();
  }
  entry.setTimestamp(new DateTime());
  entry.setDoneSum(storyHierarchyDAO.totalLeafDoneStoryPoints(project));
  entry.setEstimateSum(storyHierarchyDAO.totalLeafStoryPoints(project));
  entry.setRootSum(storyHierarchyDAO.totalRootStoryPoints(project));
  entry.setBacklog(project);
  backlogHistoryEntryDAO.store(entry);
}