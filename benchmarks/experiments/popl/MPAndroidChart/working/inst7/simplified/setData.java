private LineChart mChart;
private SeekBar mSeekBarX;
private SeekBar mSeekBarY;
private LineDataSet set1;
private LineDataSet set2;
private TextView tvX;
private TextView tvY;
private void setData (int count, float range)
{
  ArrayList<XAxisValue> xVals = new ArrayList<XAxisValue>();
  {
    {
      int i = 0;
    }
    while (i < count)
    {
      {
        xVals.add(new XAxisValue(i, (i + "")));
      }
      i = i + 1;
    }
  }
  ArrayList<Entry> yVals1 = new ArrayList<Entry>();
  {
    {
      int i = 0;
    }
    while (i < count)
    {
      {
        float val = Math.op(range);
        yVals1.add(new Entry(val, i));
      }
      i = i + 1;
    }
  }
  ArrayList<Entry> yVals2 = new ArrayList<Entry>();
  {
    {
      int i = 0;
    }
    while (i < count)
    {
      {
        float val = Math.op(range);
        yVals2.add(new Entry(val, i));
      }
      i = i + 1;
    }
  }
  int c1 = mChart.getData();
  int c2 = mChart.getData().getDataSetCount();
  if (c1 != null && c2 > 0)
  {
    set1 = (LineDataSet) mChart.getData().getDataSetByIndex(0);
    set2 = (LineDataSet) mChart.getData().getDataSetByIndex(1);
    set1.setYVals(yVals1);
    set2.setYVals(yVals2);
    mChart.getData().notifyDataChanged();
    mChart.notifyDataSetChanged();
  }
  else
  {
    set1 = new LineDataSet(yVals1, "DataSet 1");
    set1.setAxisDependency(AxisDependency.LEFT);
    set1.setColor(ColorTemplate.getHoloBlue());
    set1.setCircleColor(Color.WHITE);
    set1.setLineWidth(2.0F);
    set1.setCircleRadius(3.0F);
    set1.setFillAlpha(65);
    set1.setFillColor(ColorTemplate.getHoloBlue());
    set1.setHighLightColor(Color.rgb(244, 117, 117));
    set1.setDrawCircleHole(false);
    set2 = new LineDataSet(yVals2, "DataSet 2");
    set2.setAxisDependency(AxisDependency.RIGHT);
    set2.setColor(Color.RED);
    set2.setCircleColor(Color.WHITE);
    set2.setLineWidth(2.0F);
    set2.setCircleRadius(3.0F);
    set2.setFillAlpha(65);
    set2.setFillColor(Color.RED);
    set2.setDrawCircleHole(false);
    set2.setHighLightColor(Color.rgb(244, 117, 117));
    ArrayList<ILineDataSet> dataSets = new ArrayList<ILineDataSet>();
    dataSets.add(set1);
    dataSets.add(set2);
    LineData data = new LineData(xVals, dataSets);
    data.setValueTextColor(Color.WHITE);
    data.setValueTextSize(9.0F);
    mChart.setData(data);
  }
  return;
}
private void setData (int count, float range)
{
  ArrayList<String> xVals = new ArrayList<String>();
  for (int i = 0 ; i < count ; i++)
  {
    xVals.add((i + ""));
  }
  ArrayList<Entry> yVals1 = new ArrayList<Entry>();
  for (int i = 0 ; i < count ; i++)
  {
    float val = Math.op(range);
    yVals1.add(new Entry(val, i));
  }
  ArrayList<Entry> yVals2 = new ArrayList<Entry>();
  for (int i = 0 ; i < count ; i++)
  {
    float val = Math.op(range);
    yVals2.add(new Entry(val, i));
  }
  int c1 = mChart.getData();
  int c2 = mChart.getData().getDataSetCount();
  if (c1 != null && c2 > 0)
  {
    set1 = (LineDataSet) mChart.getData().getDataSetByIndex(0);
    set2 = (LineDataSet) mChart.getData().getDataSetByIndex(1);
    set1.setYVals(yVals1);
    set2.setYVals(yVals2);
    mChart.notifyDataSetChanged();
  }
  else
  {
    set1 = new LineDataSet(yVals1, "DataSet 1");
    set1.setAxisDependency(AxisDependency.LEFT);
    set1.setColor(ColorTemplate.getHoloBlue());
    set1.setCircleColor(Color.WHITE);
    set1.setLineWidth(2.0F);
    set1.setCircleRadius(3.0F);
    set1.setFillAlpha(65);
    set1.setFillColor(ColorTemplate.getHoloBlue());
    set1.setHighLightColor(Color.rgb(244, 117, 117));
    set1.setDrawCircleHole(false);
    set2 = new LineDataSet(yVals2, "DataSet 2");
    set2.setAxisDependency(AxisDependency.RIGHT);
    set2.setColor(Color.RED);
    set2.setCircleColor(Color.WHITE);
    set2.setLineWidth(2.0F);
    set2.setCircleRadius(3.0F);
    set2.setFillAlpha(65);
    set2.setFillColor(Color.RED);
    set2.setDrawCircleHole(false);
    set2.setHighLightColor(Color.rgb(244, 117, 117));
    ArrayList<ILineDataSet> dataSets = new ArrayList<ILineDataSet>();
    dataSets.add(set1);
    dataSets.add(set2);
    LineData data = new LineData(xVals, dataSets);
    data.setValueTextColor(Color.WHITE);
    data.setValueTextSize(9.0F);
    mChart.setData(data);
  }
  return;
}
private void setData (int count, float range)
{
  ArrayList<XAxisValue> xVals = new ArrayList<XAxisValue>();
  for (int i = 0 ; i < count ; i++)
  {
    xVals.add(new XAxisValue(i, (i + "")));
  }
  ArrayList<Entry> yVals1 = new ArrayList<Entry>();
  for (int i = 0 ; i < count ; i++)
  {
    float val = Math.op(range);
    yVals1.add(new Entry(val, i));
  }
  ArrayList<Entry> yVals2 = new ArrayList<Entry>();
  for (int i = 0 ; i < count ; i++)
  {
    float val = Math.op(range);
    yVals2.add(new Entry(val, i));
  }
  int c1 = mChart.getData();
  int c2 = mChart.getData().getDataSetCount();
  if (c1 != null && c2 > 0)
  {
    set1 = (LineDataSet) mChart.getData().getDataSetByIndex(0);
    set2 = (LineDataSet) mChart.getData().getDataSetByIndex(1);
    set1.setYVals(yVals1);
    set2.setYVals(yVals2);
    mChart.notifyDataSetChanged();
  }
  else
  {
    set1 = new LineDataSet(yVals1, "DataSet 1");
    set1.setAxisDependency(AxisDependency.LEFT);
    set1.setColor(ColorTemplate.getHoloBlue());
    set1.setCircleColor(Color.WHITE);
    set1.setLineWidth(2.0F);
    set1.setCircleRadius(3.0F);
    set1.setFillAlpha(65);
    set1.setFillColor(ColorTemplate.getHoloBlue());
    set1.setHighLightColor(Color.rgb(244, 117, 117));
    set1.setDrawCircleHole(false);
    set2 = new LineDataSet(yVals2, "DataSet 2");
    set2.setAxisDependency(AxisDependency.RIGHT);
    set2.setColor(Color.RED);
    set2.setCircleColor(Color.WHITE);
    set2.setLineWidth(2.0F);
    set2.setCircleRadius(3.0F);
    set2.setFillAlpha(65);
    set2.setFillColor(Color.RED);
    set2.setDrawCircleHole(false);
    set2.setHighLightColor(Color.rgb(244, 117, 117));
    ArrayList<ILineDataSet> dataSets = new ArrayList<ILineDataSet>();
    dataSets.add(set1);
    dataSets.add(set2);
    LineData data = new LineData(xVals, dataSets);
    data.setValueTextColor(Color.WHITE);
    data.setValueTextSize(9.0F);
    mChart.setData(data);
  }
  return;
}
private void setData (int count, float range)
{
  ArrayList<String> xVals = new ArrayList<String>();
  for (int i = 0 ; i < count ; i++)
  {
    xVals.add((i + ""));
  }
  ArrayList<Entry> yVals1 = new ArrayList<Entry>();
  for (int i = 0 ; i < count ; i++)
  {
    float val = Math.op(range);
    yVals1.add(new Entry(val, i));
  }
  ArrayList<Entry> yVals2 = new ArrayList<Entry>();
  for (int i = 0 ; i < count ; i++)
  {
    float val = Math.op(range);
    yVals2.add(new Entry(val, i));
  }
  int c1 = mChart.getData();
  int c2 = mChart.getData().getDataSetCount();
  if (c1 != null && c2 > 0)
  {
    set1 = (LineDataSet) mChart.getData().getDataSetByIndex(0);
    set2 = (LineDataSet) mChart.getData().getDataSetByIndex(1);
    set1.setYVals(yVals1);
    set2.setYVals(yVals2);
    mChart.getData().notifyDataChanged();
    mChart.notifyDataSetChanged();
  }
  else
  {
    set1 = new LineDataSet(yVals1, "DataSet 1");
    set1.setAxisDependency(AxisDependency.LEFT);
    set1.setColor(ColorTemplate.getHoloBlue());
    set1.setCircleColor(Color.WHITE);
    set1.setLineWidth(2.0F);
    set1.setCircleRadius(3.0F);
    set1.setFillAlpha(65);
    set1.setFillColor(ColorTemplate.getHoloBlue());
    set1.setHighLightColor(Color.rgb(244, 117, 117));
    set1.setDrawCircleHole(false);
    set2 = new LineDataSet(yVals2, "DataSet 2");
    set2.setAxisDependency(AxisDependency.RIGHT);
    set2.setColor(Color.RED);
    set2.setCircleColor(Color.WHITE);
    set2.setLineWidth(2.0F);
    set2.setCircleRadius(3.0F);
    set2.setFillAlpha(65);
    set2.setFillColor(Color.RED);
    set2.setDrawCircleHole(false);
    set2.setHighLightColor(Color.rgb(244, 117, 117));
    ArrayList<ILineDataSet> dataSets = new ArrayList<ILineDataSet>();
    dataSets.add(set1);
    dataSets.add(set2);
    LineData data = new LineData(xVals, dataSets);
    data.setValueTextColor(Color.WHITE);
    data.setValueTextSize(9.0F);
    mChart.setData(data);
  }
  return;
}
private void setData (int count, float range)
{
  ArrayList<XAxisValue> xVals = new ArrayList<XAxisValue>();
  for (int i = 0 ; i < count ; i++)
  {
    xVals.add(new XAxisValue(i, (i + "")));
  }
  ArrayList<Entry> yVals1 = new ArrayList<Entry>();
  for (int i = 0 ; i < count ; i++)
  {
    float val = Math.op(range);
    yVals1.add(new Entry(val, i));
  }
  ArrayList<Entry> yVals2 = new ArrayList<Entry>();
  for (int i = 0 ; i < count ; i++)
  {
    float val = Math.op(range);
    yVals2.add(new Entry(val, i));
  }
  int c1 = mChart.getData();
  int c2 = mChart.getData().getDataSetCount();
  if (c1 != null && c2 > 0)
  {
    set1 = (LineDataSet) mChart.getData().getDataSetByIndex(0);
    set2 = (LineDataSet) mChart.getData().getDataSetByIndex(1);
    set1.setYVals(yVals1);
    set2.setYVals(yVals2);
    mChart.getData().notifyDataChanged();
    mChart.notifyDataSetChanged();
  }
  else
  {
    set1 = new LineDataSet(yVals1, "DataSet 1");
    set1.setAxisDependency(AxisDependency.LEFT);
    set1.setColor(ColorTemplate.getHoloBlue());
    set1.setCircleColor(Color.WHITE);
    set1.setLineWidth(2.0F);
    set1.setCircleRadius(3.0F);
    set1.setFillAlpha(65);
    set1.setFillColor(ColorTemplate.getHoloBlue());
    set1.setHighLightColor(Color.rgb(244, 117, 117));
    set1.setDrawCircleHole(false);
    set2 = new LineDataSet(yVals2, "DataSet 2");
    set2.setAxisDependency(AxisDependency.RIGHT);
    set2.setColor(Color.RED);
    set2.setCircleColor(Color.WHITE);
    set2.setLineWidth(2.0F);
    set2.setCircleRadius(3.0F);
    set2.setFillAlpha(65);
    set2.setFillColor(Color.RED);
    set2.setDrawCircleHole(false);
    set2.setHighLightColor(Color.rgb(244, 117, 117));
    ArrayList<ILineDataSet> dataSets = new ArrayList<ILineDataSet>();
    dataSets.add(set1);
    dataSets.add(set2);
    LineData data = new LineData(xVals, dataSets);
    data.setValueTextColor(Color.WHITE);
    data.setValueTextSize(9.0F);
    mChart.setData(data);
  }
  return;
}
