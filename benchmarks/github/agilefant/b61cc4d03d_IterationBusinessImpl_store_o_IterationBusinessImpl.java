{
  Backlog parent = null;
  if (parentBacklogId != 0)
  {
    parent = this.backlogBusiness.retrieve(parentBacklogId);
  }
  if ((parent == null && iterationId) == 0)
  {
    throw new IllegalArgumentException("Invalid parent.");
  }
  if (parent instanceof Iteration)
  {
    throw new IllegalArgumentException("Nested iterations are not allowed.");
  }
  if (iterationData.getEndDate().isBefore(iterationData.getStartDate()))
  {
    throw new IllegalArgumentException("End date before start date");
  }
  if (iterationId == 0)
  {
    return transferObjectBusiness.constructIterationTO(this.create(parent, iterationData, assigneeIds));
  }
  Iteration iter = this.retrieve(iterationId);
  iter.setStartDate(iterationData.getStartDate());
  iter.setEndDate(iterationData.getEndDate());
  iter.setBacklogSize(iterationData.getBacklogSize());
  iter.setBaselineLoad(iterationData.getBaselineLoad());
  iter.setDescription(iterationData.getDescription());
  iter.setName(iterationData.getName());
  setAssignees(iter, assigneeIds);
  this.iterationDAO.store(iter);
  if ((parent != null && iter.getParent()) != parent)
  {
    this.moveTo(iter, parent);
  }
  return transferObjectBusiness.constructIterationTO(iter);
}