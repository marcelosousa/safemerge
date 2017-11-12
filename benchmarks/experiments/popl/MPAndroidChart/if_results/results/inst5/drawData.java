protected Path cubicFillPath = new Path();
protected Path cubicPath = new Path();
protected Canvas mBitmapCanvas;
protected LineDataProvider mChart;
protected CircleBuffer[] mCircleBuffers;
protected Paint mCirclePaintInner;
protected LineBuffer[] mLineBuffers;
protected Bitmap mPathBitmap;
protected Object mViewPortHandler;
@Override
 public void drawData (Canvas c)
{
  if (mPathBitmap == null || mPathBitmap.getWidth() != (int) mViewPortHandler.getChartWidth() || mPathBitmap.getHeight() != (int) mViewPortHandler.getChartHeight())
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
      wiz_i + 1;
    }
  }
  c.drawBitmap(mPathBitmap, 0, 0, mRenderPaint);
  return;
}
