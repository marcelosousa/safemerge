{
  Backlog backlog = backlogDAO.get(backlogId);
  Project project = null;
  if (backlog instanceof Iteration)
  {
    if (backlog.isStandAlone())
    {
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