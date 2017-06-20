package com.xxmassdeveloper.mpchartexample;

public class CubicLineChartActivity extends DemoBase implements OnSeekBarChangeListener {

    private LineChart mChart;
    private SeekBar mSeekBarX, mSeekBarY;
    private TextView tvX, tvY;
    
    private Typeface tf;

    private int setData(int count, float range) {

        ArrayList<String> xVals = new ArrayList<String>();
        for (int i = 0; i < count; i++) {
            xVals.add(1990 + i + "");
        }

        ArrayList<Entry> yVals = new ArrayList<Entry>();

        for (int i = 0; i < count; i++) {
            float mult = (range + 1);
            float val = (float) (Math.random() * mult) + 20;
            yVals.add(new Entry(val, i));
        }

        LineDataSet set1;

        if ((mChart.getData() != null) &&
                (mChart.getData().getDataSetCount() > 0)) {
            set1 = (LineDataSet)mChart.getData().getDataSetByIndex(0);
            set1.setYVals(yVals);
            mChart.getData().setXVals(xVals);
            mChart.getData().notifyDataChanged();
            mChart.notifyDataSetChanged();
        } else {
            // create a dataset and give it a type
            set1 = new LineDataSet(yVals, "DataSet 1");

            set1.setDrawCubic(true);
            set1.setCubicIntensity(0.2f);
            //set1.setDrawFilled(true);
            set1.setDrawCircles(false);
            set1.setLineWidth(1.8f);
            set1.setCircleRadius(4f);
            set1.setCircleColor(Color.WHITE);
            set1.setHighLightColor(Color.rgb(244, 117, 117));
            set1.setColor(Color.WHITE);
            set1.setFillColor(Color.WHITE);
            set1.setFillAlpha(100);
            set1.setDrawHorizontalHighlightIndicator(false);
            //set1.setFillFormatter(new FillFormatter() {
            //    @Override
            //    public float getFillLinePosition(ILineDataSet dataSet, LineDataProvider dataProvider) {
            //        return -10;
            //    }
            //});

            // create a data object with the datasets
            LineData data = new LineData(xVals, set1);
            data.setValueTypeface(tf);
            data.setValueTextSize(9f);
            data.setDrawValues(false);

            // set data
            mChart.setData(data);
        }
        return 0;
    }
}
