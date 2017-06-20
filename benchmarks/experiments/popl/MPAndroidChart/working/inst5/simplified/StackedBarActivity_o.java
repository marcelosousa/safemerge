package com.xxmassdeveloper.mpchartexample;

public class StackedBarActivity extends DemoBase implements OnSeekBarChangeListener, OnChartValueSelectedListener {

	private BarChart mChart;
	private SeekBar mSeekBarX, mSeekBarY;
	private TextView tvX, tvY;

	@Override
	public void onProgressChanged(SeekBar seekBar, int progress, boolean fromUser) {

		tvX.setText("" + (mSeekBarX.getProgress() + 1));
		tvY.setText("" + (mSeekBarY.getProgress()));

		ArrayList<String> xVals = new ArrayList<String>();
		for (int i = 0; i < (mSeekBarX.getProgress() + 1); i++) {
			xVals.add(get_mMonths(i));
		}

		ArrayList<BarEntry> yVals1 = new ArrayList<BarEntry>();

		for (int i = 0; i < (mSeekBarX.getProgress() + 1); i++) {
			float mult = (mSeekBarY.getProgress() + 1);
			float val1 = (float) (Math.random() * mult) + mult / 3;
			float val2 = (float) (Math.random() * mult) + mult / 3;
			float val3 = (float) (Math.random() * mult) + mult / 3;

			yVals1.add(new BarEntry(val1, val2, val3, i));
		}

		BarDataSet set1;

		if ((mChart.getData() != null) &&
				(mChart.getData().getDataSetCount() > 0)) {
			set1 = (BarDataSet)mChart.getData().getDataSetByIndex(0);
			set1.setYVals(yVals1);
			mChart.getData().setXVals(xVals);
			mChart.getData().notifyDataChanged();
			mChart.notifyDataSetChanged();
		} else {
			set1 = new BarDataSet(yVals1, "Statistics Vienna 2014");
			set1.setColors(getColors());
			set1.setStackLabels("Births", "Divorces", "Marriages");

			ArrayList<IBarDataSet> dataSets = new ArrayList<IBarDataSet>();
			dataSets.add(set1);

			BarData data = new BarData(xVals, dataSets);
			data.setValueFormatter(new MyValueFormatter());

			mChart.setData(data);
			mChart.invalidate();
		}
	}
}
