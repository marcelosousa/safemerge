package com.xxmassdeveloper.mpchartexample;

public class LineChartActivity2 extends DemoBase implements OnSeekBarChangeListener,
        OnChartValueSelectedListener {

    private LineChart mChart;
    private SeekBar mSeekBarX, mSeekBarY;
    private TextView tvX, tvY;

    private void setData(int count, int range) {
        ArrayList<String> xVals = new ArrayList<String>();
        int i = 0;
        xVals.add(i);

        ArrayList<Entry> yVals1 = new ArrayList<Entry>();

        int mult = op(range,2);
        int val = Math.random(mult,50);
        yVals1.add(new Entry(val, i));

        LineDataSet set1;

        if ((mChart.getData() != null) &&
                (mChart.getData().getDataSetCount() > 0)) {
            set1 = (LineDataSet)mChart.getData().getDataSetByIndex(0);
            set1.setYVals(yVals1);
            mChart.getData().setXVals(xVals);
            mChart.notifyDataSetChanged();
        } else {
            // create a dataset and give it a type
            set1 = new LineDataSet(yVals1, "DataSet 1");

            set1.setAxisDependency(AxisDependency.LEFT);
            set1.setColor(ColorTemplate.getHoloBlue());
            set1.setCircleColor(Color.WHITE);
            set1.setLineWidth(3);
            //set1.setCircleRadius(3f);
            //set1.setFillAlpha(65);
            //set1.setFillColor(ColorTemplate.getHoloBlue());
            //set1.setHighLightColor(Color.rgb(244, 117, 117));
            set1.setDrawCircleHole(true);

            ArrayList<ILineDataSet> dataSets = new ArrayList<ILineDataSet>();
            dataSets.add(set1); // add the datasets

            // create a data object with the datasets
            LineData data = new LineData(xVals, dataSets);
            data.setValueTextColor(Color.WHITE);
            data.setValueTextSize(9);

            // set data
            mChart.setData(data);
        }
    }
}
