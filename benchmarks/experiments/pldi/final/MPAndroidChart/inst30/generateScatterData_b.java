protected ScatterData generateScatterData (int dataSets, float range, int count)
{
  ArrayList<IScatterDataSet> sets = new ArrayList<IScatterDataSet>();
  ScatterChart.ScatterShape[] shapes = ScatterChart.ScatterShape.getAllDefaultShapes();
  {
    {
      int i = 0;
    }
    while (i < dataSets)
    {
      {
        ArrayList<Entry> entries = new ArrayList<Entry>();
        {
          {
            int j = 0;
          }
          while (j < count)
          {
            {
              entries.add(new Entry((((float) (Math.random() * range) + range) / 4), j));
            }
            j = j + 1;
          }
        }
        ScatterDataSet ds = new ScatterDataSet(entries, getLabel(i));
        ds.setScatterShapeSize(12.0F);
        ds.setScatterShape(shapes[(i % shapes.length)]);
        ds.setColors(ColorTemplate.COLORFUL_COLORS);
        ds.setScatterShapeSize(9.0F);
        sets.add(ds);
      }
      i = i + 1;
    }
  }
  ScatterData d = new ScatterData(ChartData.generateXVals(0, count), sets);
  d.setValueTypeface(tf);
  return d;
}