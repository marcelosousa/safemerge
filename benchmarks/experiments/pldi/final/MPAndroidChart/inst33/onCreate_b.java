@Override
 protected void onCreate (Bundle savedInstanceState)
{
  super.onCreate(savedInstanceState);
  getWindow().setFlags(WindowManager.LayoutParams.FLAG_FULLSCREEN, WindowManager.LayoutParams.FLAG_FULLSCREEN);
  setContentView(R.layout.activity_radarchart);
  mChart = (RadarChart) findViewById(R.id.chart1);
  tf = Typeface.createFromAsset(getAssets(), "OpenSans-Regular.ttf");
  mChart.setDescription("");
  mChart.setWebLineWidth(1.5F);
  mChart.setWebLineWidthInner(0.75F);
  mChart.setWebAlpha(100);
  MyMarkerView mv = new MyMarkerView(this, R.layout.custom_marker_view);
  mChart.setMarkerView(mv);
  setData();
  XAxis xAxis = mChart.getXAxis();
  xAxis.setTypeface(tf);
  xAxis.setTextSize(9.0F);
  YAxis yAxis = mChart.getYAxis();
  yAxis.setTypeface(tf);
  yAxis.setLabelCount(5, false);
  yAxis.setTextSize(9.0F);
  yAxis.setAxisMinValue(0.0F);
  Legend l = mChart.getLegend();
  l.setPosition(LegendPosition.RIGHT_OF_CHART);
  l.setTypeface(tf);
  l.setXEntrySpace(7.0F);
  l.setYEntrySpace(5.0F);
  return;
}