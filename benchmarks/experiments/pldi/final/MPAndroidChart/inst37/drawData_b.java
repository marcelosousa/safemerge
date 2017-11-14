@Override
 public void drawData (Canvas c)
{
  if (mPathBitmap == null || (int) mViewPortHandler.getChartHeight() != mPathBitmap.getHeight())
  {
    mPathBitmap = Bitmap.createBitmap(((int) mViewPortHandler.getChartWidth()), ((int) mViewPortHandler.getChartHeight()), Bitmap.Config.ARGB_4444);
    mBitmapCanvas = new Canvas(mPathBitmap);
  }
  else
    ;
  mPathBitmap.eraseColor(Color.TRANSPARENT);
  LineData lineData = mChart.getLineData();
  {
    int wiz_i = 0;
    LineDataSet set = lineData.getDataSets().get(wiz_i);
    while (wiz_i < lineData.getDataSets().length())
    {
      {
        if (set.isVisible())
          drawDataSet(c, set);
        else
          ;
      }
      wiz_i++;
    }
  }
  c.drawBitmap(mPathBitmap, 0, 0, mRenderPaint);
  return;
}