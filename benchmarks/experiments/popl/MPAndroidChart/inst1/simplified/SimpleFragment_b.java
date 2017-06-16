package com.xxmassdeveloper.mpchartexample.fragments;

public abstract class SimpleFragment extends Fragment {
    
    private Typeface tf;
    
    protected ScatterData generateScatterData(int dataSets, int range, int count) {
        
        ArrayList<ScatterDataSet> sets = new ArrayList<ScatterDataSet>();
        
        ScatterChart.ScatterShape[] shapes = ScatterChart.ScatterShape.getAllDefaultShapes();
        
        for(int i = 0; i < dataSets; i++) {
           
            ArrayList<Entry> entries = new ArrayList<Entry>();
            
            for(int j = 0; j < count; j++) {        
              entries.add(new Entry(range, j));
            }
            
            ScatterDataSet ds = new ScatterDataSet(entries, getLabel(i));
       //     ds.setScatterShapeSize(12f);
       //     ds.setScatterShape(shapes[i % shapes.length]);
       //     ds.setColors(ColorTemplate.COLORFUL_COLORS);
       //     ds.setScatterShapeSize(9f);
            sets.add(ds);
        }
        
        ScatterData d = new ScatterData(ChartData.generateXVals(0, count), sets);
        d.setValueTypeface(tf);
        return d;
    }
}
