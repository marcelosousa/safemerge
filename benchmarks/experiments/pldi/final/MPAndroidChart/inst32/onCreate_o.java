@Override
 protected void onCreate (Bundle savedInstanceState)
{
  super.onCreate(savedInstanceState);
  getWindow().setFlags(WindowManager.LayoutParams.FLAG_FULLSCREEN, WindowManager.LayoutParams.FLAG_FULLSCREEN);
  setContentView(R.layout.activity_age_distribution);
  setTitle("Age Distribution Austria");
  mChart = (HorizontalBarChart) findViewById(R.id.chart1);
  mChart.setOnChartValueSelectedListener(this);
  mChart.setDrawGridBackground(false);
  mChart.setDescription("");
  mChart.setPinchZoom(false);
  mChart.setDrawBarShadow(false);
  mChart.setDrawValueAboveBar(true);
  mChart.getAxisLeft().setEnabled(false);
  mChart.getAxisRight().setStartAtZero(false);
  mChart.getAxisRight().setAxisMaxValue(25.0F);
  mChart.getAxisRight().setAxisMinValue((-25.0F));
  mChart.getAxisRight().setLabelCount(7, false);
  mChart.getAxisRight().setValueFormatter(new CustomFormatter());
  mChart.getAxisRight().setTextSize(9.0F);
  XAxis xAxis = mChart.getXAxis();
  xAxis.setPosition(XAxisPosition.BOTH_SIDED);
  xAxis.setDrawGridLines(false);
  xAxis.setDrawAxisLine(false);
  xAxis.setTextSize(9.0F);
  Legend l = mChart.getLegend();
  l.setPosition(LegendPosition.BELOW_CHART_RIGHT);
  l.setFormSize(8.0F);
  l.setFormToTextSpace(4.0F);
  l.setXEntrySpace(6.0F);
  ArrayList<BarEntry> yValues = new ArrayList<BarEntry>();
  yValues.add(new BarEntry(new float[] {
                                         (-10),
                                         10,
                                       }, 0));
  yValues.add(new BarEntry(new float[] {
                                         (-12),
                                         13,
                                       }, 1));
  yValues.add(new BarEntry(new float[] {
                                         (-15),
                                         15,
                                       }, 2));
  yValues.add(new BarEntry(new float[] {
                                         (-17),
                                         17,
                                       }, 3));
  yValues.add(new BarEntry(new float[] {
                                         (-19),
                                         20,
                                       }, 4));
  yValues.add(new BarEntry(new float[] {
                                         (-19),
                                         19,
                                       }, 5));
  yValues.add(new BarEntry(new float[] {
                                         (-16),
                                         16,
                                       }, 6));
  yValues.add(new BarEntry(new float[] {
                                         (-13),
                                         14,
                                       }, 7));
  yValues.add(new BarEntry(new float[] {
                                         (-10),
                                         11,
                                       }, 8));
  yValues.add(new BarEntry(new float[] {
                                         (-5),
                                         6,
                                       }, 9));
  yValues.add(new BarEntry(new float[] {
                                         (-1),
                                         2,
                                       }, 10));
  BarDataSet set = new BarDataSet(yValues, "Age Distribution");
  set.setValueFormatter(new CustomFormatter());
  set.setValueTextSize(7.0F);
  set.setAxisDependency(YAxis.AxisDependency.RIGHT);
  set.setBarSpacePercent(40.0F);
  set.setColors(new int[] {
                            Color.rgb(67, 67, 72),
                            Color.rgb(124, 181, 236),
                          });
  set.setStackLabels(new String[] {
                                    "Men",
                                    "Women",
                                  });
  String[] xVals = new String[] {
                                  "0-10",
                                  "10-20",
                                  "20-30",
                                  "30-40",
                                  "40-50",
                                  "50-60",
                                  "60-70",
                                  "70-80",
                                  "80-90",
                                  "90-100",
                                  "100+",
                                };
  BarData data = new BarData(xVals, set);
  mChart.setData(data);
  mChart.invalidate();
  return;
}