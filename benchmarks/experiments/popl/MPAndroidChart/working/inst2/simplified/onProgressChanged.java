private ScatterChart mChart;
private SeekBar mSeekBarX;
private SeekBar mSeekBarY;
private Typeface tf;
private TextView tvX;
private TextView tvY;
@Override
 public void onProgressChanged (SeekBar seekBar, int progress, boolean fromUser)
{
  tvX.setText(("" + mSeekBarX.getProgress() + 1));
  tvY.setText(("" + mSeekBarY.getProgress()));
  ArrayList<String> xVals = new ArrayList<String>();
  {
    {
      int i = 0;
    }
    while (i < mSeekBarX.getProgress() + 1)
    {
      {
        xVals.add((i + ""));
      }
      i = i + 1;
    }
  }
  ArrayList<Entry> yVals1 = new ArrayList<Entry>();
  ArrayList<Entry> yVals2 = new ArrayList<Entry>();
  ArrayList<Entry> yVals3 = new ArrayList<Entry>();
  {
    {
      int i = 0;
    }
    while (i < mSeekBarX.getProgress())
    {
      {
        float val = (float) (Math.random() * mSeekBarY.getProgress()) + 3;
        yVals1.add(new Entry(0, i, val));
      }
      i = i + 1;
    }
  }
  {
    {
      int i = 0;
    }
    while (i < mSeekBarX.getProgress())
    {
      {
        float val = (float) (Math.random() * mSeekBarY.getProgress()) + 3;
        yVals2.add(new Entry(0, (i + 0.33F), val));
      }
      i = i + 1;
    }
  }
  {
    {
      int i = 0;
    }
    while (i < mSeekBarX.getProgress())
    {
      {
        float val = (float) (Math.random() * mSeekBarY.getProgress()) + 3;
        yVals3.add(new Entry(0, (i + 0.66F), val));
      }
      i = i + 1;
    }
  }
  ScatterDataSet set1 = new ScatterDataSet(yVals1, "DS 1");
  set1.setScatterShape(ScatterChart.ScatterShape.SQUARE);
  set1.setColor(ColorTemplate.COLORFUL_COLORS_0);
  ScatterDataSet set2 = new ScatterDataSet(yVals2, "DS 2");
  set2.setScatterShape(ScatterChart.ScatterShape.CIRCLE);
  set2.setScatterShapeHoleColor(ColorTemplate.COLORFUL_COLORS_3);
  set2.setScatterShapeHoleRadius(3.0F);
  set2.setColor(ColorTemplate.COLORFUL_COLORS_1);
  ScatterDataSet set3 = new ScatterDataSet(yVals3, "DS 3");
  set3.setScatterShape(CustomScatterShapeRenderer.IDENTIFIER);
  set3.setColor(ColorTemplate.COLORFUL_COLORS_2);
  set1.setScatterShapeSize(8.0F);
  set2.setScatterShapeSize(8.0F);
  set3.setScatterShapeSize(8.0F);
  ArrayList<IScatterDataSet> dataSets = new ArrayList<IScatterDataSet>();
  dataSets.add(set1);
  dataSets.add(set2);
  dataSets.add(set3);
  ScatterData data = new ScatterData(dataSets);
  data.setValueTypeface(mTfLight);
  mChart.setData(data);
  mChart.invalidate();
  return;
}
