package com.xxmassdeveloper.mpchartexample;

public class LineChartActivity2 extends DemoBase implements OnSeekBarChangeListener,
        OnChartValueSelectedListener {

    private LineDataSet setData(int count, int range) {
        LineDataSet set1 = new LineDataSet(0);

        int i = ColorTemplate.COLORFUL_COLORS_0;
        set1.setLineWidth(2);
        set1.foo().setDrawCircleHole(1);
        if (set1.bla() == 0)
        {
          i = 1;
        }

        return set1;
    }
}
