{
  Criteria crit = getCurrentSession().createCriteria(Task.class);
  crit.setProjection(Projections.projectionList().add(Projections.sum("effortLeft")).add(Projections.sum("originalEstimate")));
  crit = crit.createCriteria("story");
  crit = crit.createCriteria("backlog");
  crit.add(Restrictions.idEq(iterationId));
  Object[] results = (Object[]) crit.uniqueResult();
  return parseResultToPair(results);
}