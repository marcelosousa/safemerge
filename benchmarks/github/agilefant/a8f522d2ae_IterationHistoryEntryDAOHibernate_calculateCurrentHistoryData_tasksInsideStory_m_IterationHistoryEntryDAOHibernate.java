{
  Criteria crit = getCurrentSession().createCriteria(Task.class);
  crit.setProjection(Projections.projectionList().add(Projections.sum("effortLeft")).add(Projections.sum("originalEstimate")));
  crit.add(Restrictions.ne("state", TaskState.DEFERRED));
  crit = crit.createCriteria("story");
  crit.setFetchMode("story", FetchMode.SELECT);
  crit.add(Restrictions.ne("state", StoryState.DEFERRED));
  crit = crit.createCriteria("backlog");
  crit.add(Restrictions.idEq(iterationId));
  Object[] results = (Object[]) crit.uniqueResult();
  return parseResultToPair(results);
}