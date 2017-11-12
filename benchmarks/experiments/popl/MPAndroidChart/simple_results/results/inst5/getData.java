private LineChart[] mCharts = new LineChart[4];
private int[] mColors = new int[] {
                                    Color.rgb(137, 230, 81),
                                    Color.rgb(240, 240, 30),
                                    Color.rgb(89, 199, 250),
                                    Color.rgb(250, 104, 104),
                                  };
private Typeface mTf;
private LineData getData (int count, float range)
{
  ArrayList<Entry> yVals = new ArrayList<Entry>();
  {
    {
      int i = 0;
    }
    while (i < count)
    {
      {
        int val = compute(range);
        yVals.add(new Entry(val, i));
      }
      i = i + 1;
    }
  }
  LineDataSet set1 = new LineDataSet(yVals, "DataSet 1");
  set1.setLineWidth(1.75F);
  set1.setCircleRadius(5.0F);
  set1.setCircleHoleRadius(2.5F);
  set1.setColor(Color.WHITE);
  set1.setCircleColor(Color.WHITE);
  set1.setHighLightColor(Color.WHITE);
  set1.setDrawValues(false);
  LineData data = new LineData(set1);
  return data;
}
