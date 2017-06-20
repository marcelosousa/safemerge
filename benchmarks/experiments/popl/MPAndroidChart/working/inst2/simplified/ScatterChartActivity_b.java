
package com.xxmassdeveloper.mpchartexample;

public class ScatterChartActivity extends DemoBase implements OnSeekBarChangeListener,
        OnChartValueSelectedListener {

    private ScatterChart mChart;
    private SeekBar mSeekBarX, mSeekBarY;
    private TextView tvX, tvY;

    private Typeface tf;
    
    @Override
    public void onProgressChanged(SeekBar seekBar, int progress, boolean fromUser) {

        tvX.setText("" + (mSeekBarX.getProgress() + 1));
        tvY.setText("" + (mSeekBarY.getProgress()));

        ArrayList<String> xVals = new ArrayList<String>();
        for (int i = 0; i < (mSeekBarX.getProgress() + 1); i++) {
            xVals.add(i + "");
        }

        ArrayList<Entry> yVals1 = new ArrayList<Entry>();
        ArrayList<Entry> yVals2 = new ArrayList<Entry>();
        ArrayList<Entry> yVals3 = new ArrayList<Entry>();

        for (int i = 0; i < mSeekBarX.getProgress(); i++) {
            float val = (float) (Math.random() * mSeekBarY.getProgress()) + 3;
            yVals1.add(new Entry(val, i));
        }

        for (int i = 0; i < mSeekBarX.getProgress(); i++) {
            float val = (float) (Math.random() * mSeekBarY.getProgress()) + 3;
            yVals2.add(new Entry(val, i));
        }

        for (int i = 0; i < mSeekBarX.getProgress(); i++) {
            float val = (float) (Math.random() * mSeekBarY.getProgress()) + 3;
            yVals3.add(new Entry(val, i));
        }

        // create a dataset and give it a type
        ScatterDataSet set1 = new ScatterDataSet(yVals1, "DS 1");
        set1.setScatterShape(ScatterChart.ScatterShape.SQUARE);
        set1.setColor(ColorTemplate.COLORFUL_COLORS_0);
        ScatterDataSet set2 = new ScatterDataSet(yVals2, "DS 2");
        set2.setScatterShape(ScatterChart.ScatterShape.CIRCLE);
        set2.setScatterShapeHoleColor(ColorTemplate.COLORFUL_COLORS_3);
        set2.setScatterShapeHoleRadius(4f);
        set2.setColor(ColorTemplate.COLORFUL_COLORS_1);
        ScatterDataSet set3 = new ScatterDataSet(yVals3, "DS 3");
        set3.setScatterShape(CustomScatterShapeRenderer.IDENTIFIER);
        set3.setColor(ColorTemplate.COLORFUL_COLORS_2);

        set1.setScatterShapeSize(8f);
        set2.setScatterShapeSize(8f);
        set3.setScatterShapeSize(8f);

        ArrayList<IScatterDataSet> dataSets = new ArrayList<IScatterDataSet>();
        dataSets.add(set1); // add the datasets
        dataSets.add(set2);
        dataSets.add(set3);

        // create a data object with the datasets
        ScatterData data = new ScatterData(xVals, dataSets);
        data.setValueTypeface(tf);

        mChart.setData(data);
        mChart.invalidate();
    }

}
