import java.util.List;
import java.util.Arrays;
import java.util.ArrayList;
import java.util.Date;

public class Example 
{
  private List<int[]> res;

  public ProjectMetrics calculateProjectStoryMetrics (int backlogId)
  {
    Criteria crit = getCurrentSession().createCriteria(Story.class);
    ProjectionList proj = Projections.projectionList();
    proj.add(Projections.sum("storyPoints"));
  
    proj.add(Projections.count("id"));
    proj.add(Projections.groupProperty("state"));
  
  
    res.add(proj);
  
    ProjectMetrics metrics = new ProjectMetrics();
 
    int i = 0; 
    while (i < res.size())
    {
      int[] row = res.get(i);   
      if ((StoryState) row[2] == StoryState.DONE)
      {
       metrics.setCompletedStoryPoints((metrics.getCompletedStoryPoints() + toInt(row[0])));
       metrics.setNumberOfDoneStories((metrics.getNumberOfDoneStories() + toInt(row[1])));
      }
  
       metrics.setStoryPoints((metrics.getStoryPoints() + toInt(row[0])));
       metrics.setNumberOfStories((metrics.getNumberOfStories() + toInt(row[1])));
       i++;
    }
  
    return metrics;
  }
}

