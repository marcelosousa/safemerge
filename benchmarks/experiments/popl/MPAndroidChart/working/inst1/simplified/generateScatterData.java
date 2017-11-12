private Typeface tf;
protected ScatterData generateScatterData (int dataSets, int range, int count)
{
  ArrayList<ScatterDataSet> sets = new ArrayList<ScatterDataSet>();
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
              entries.add(new Entry(0, j, range));
            }
            j = j + 1;
          }
        }
        ScatterDataSet ds = new ScatterDataSet(entries, getLabel(i));
        sets.add(ds);
      }
      i = i + 1;
    }
  }
  ScatterData d = new ScatterData(sets);
  d.setValueTypeface(tf);
  return d;
}
