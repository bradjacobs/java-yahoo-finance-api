package com.github.bradjacobs.yahoofinance.request.builder;

import com.github.bradjacobs.yahoofinance.types.Interval;
import com.github.bradjacobs.yahoofinance.types.Range;
import com.github.bradjacobs.yahoofinance.types.YahooEndpoint;

import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;

import static com.github.bradjacobs.yahoofinance.request.builder.ChartBuilder.IndicatorFieldSelection.ADJ_CLOSE_ONLY;
import static com.github.bradjacobs.yahoofinance.request.builder.ChartBuilder.IndicatorFieldSelection.ALL;
import static com.github.bradjacobs.yahoofinance.request.builder.ChartBuilder.IndicatorFieldSelection.CLOSE_ADJCLOSE;
import static com.github.bradjacobs.yahoofinance.request.builder.ChartBuilder.IndicatorFieldSelection.CLOSE_ONLY;

public class ChartBuilder extends BasePeriodRequestBuilder<ChartBuilder>
{
    private static final String EVENT_VALUE_DIV = "div";
    private static final String EVENT_VALUE_SPLITS = "split";

    private static final String INDICATOR_VALUE_CLOSE = "close";
    private static final String INDICATOR_VALUE_ADJ_CLOSE = "adjclose";

    private String ticker;
    private Range range;
    private Interval interval = Interval.ONE_DAY;  // default
    private Boolean formatted; // this seems only applicable when response includes div or splits
    private final Set<String> eventValues = new LinkedHashSet<>();
    private Boolean includeAdjustedClose = Boolean.TRUE;
    private IndicatorFieldSelection indicatorFieldSelection;
    private Boolean includeTimestamps;
    private Boolean includePrePost;
    private Integer numberOfPoints;

    enum IndicatorFieldSelection
    {
        ALL,
        CLOSE_ADJCLOSE,
        CLOSE_ONLY,
        ADJ_CLOSE_ONLY
    }



    public ChartBuilder withTicker(String ticker) {
        this.ticker = ticker;
        return this;
    }
    public ChartBuilder withRange(Range range) {
        this.range = range;
        return this;
    }

    public ChartBuilder withInterval(Interval inverval) {
        this.interval = inverval;
        return this;
    }
    public ChartBuilder withFormatted(Boolean formatted) {
        this.formatted = formatted;
        return this;
    }
    public ChartBuilder withDividends(Boolean dividends) {
        if (Boolean.TRUE.equals(dividends)) {
            this.eventValues.add(EVENT_VALUE_DIV);
        }
        else {
            this.eventValues.remove(EVENT_VALUE_DIV);
        }
        return this;
    }
    public ChartBuilder withSplits(Boolean splits) {
        if (Boolean.TRUE.equals(splits)) {
            this.eventValues.add(EVENT_VALUE_SPLITS);
        }
        else {
            this.eventValues.remove(EVENT_VALUE_SPLITS);
        }
        return this;
    }
    public ChartBuilder withAdjClose(Boolean adjClose) {
        this.includeAdjustedClose = adjClose;
        return this;
    }


    //  convenience methods for when response is to only return certain fields
    //   (b/c not very intuitive)
    public ChartBuilder withIndicatorCloseAdjCloseOnly() {
        this.indicatorFieldSelection = CLOSE_ADJCLOSE;
        return this;
    }
    public ChartBuilder withIndicatorCloseOnly() {
        this.indicatorFieldSelection = CLOSE_ONLY;
        return this;
    }
    public ChartBuilder withIndicatorAdjCloseOnly() {
        this.indicatorFieldSelection = ADJ_CLOSE_ONLY;
        return this;
    }
    public ChartBuilder withIndicatorAllFields() {
        this.indicatorFieldSelection = ALL;
        return this;
    }

    public ChartBuilder withTimestamps(Boolean includeTimestamps) {
        this.includeTimestamps = includeTimestamps;
        return this;
    }

    public ChartBuilder withPrePost(Boolean includePrePost) {
        this.includePrePost = includePrePost;
        return this;
    }
    public ChartBuilder withNumberOfPoints(Integer numberOfPoints) {
        this.numberOfPoints = numberOfPoints;
        return this;
    }


    @Override
    protected YahooEndpoint _getRequestEndpoiint()
    {
        return YahooEndpoint.CHART;
    }

    @Override
    protected String _getRequestTicker()
    {
        return this.ticker;
    }

    @Override
    protected Map<String, String> _buildParamMap() {
        Map<String,String> map = new LinkedHashMap<>();

        // if startPeriod is set, then it takes precedence over the 'range' parameter.
        if (this.startPeriod != null)
        {
            map.put(ParamKeys.PERIOD1, this.startPeriod.toString());
            if (this.endPeriod != null) {
                map.put(ParamKeys.PERIOD2, this.endPeriod.toString());
            }
        }
        else if (this.range != null) {
            map.put(ParamKeys.RANGE, this.range.getValue());
        }

        if (this.interval != null) {
            map.put(ParamKeys.INTERVAL, this.interval.getValue());
        }
        if (this.formatted != null) {
            map.put(ParamKeys.FORMATTED, this.formatted.toString());
        }

        if (this.eventValues.size() > 0) {
            // delim can be ',' or '|'
            String eventValueString = String.join(",", eventValues);
            map.put(ParamKeys.EVENTS, eventValueString);
        }

        boolean includeAdjCloseValue = true;
        String indicatorValue = null;
        if (this.indicatorFieldSelection != null)
        {
            includeAdjCloseValue = !this.indicatorFieldSelection.equals(CLOSE_ONLY);

            if (this.indicatorFieldSelection.equals(CLOSE_ADJCLOSE) || this.indicatorFieldSelection.equals(CLOSE_ONLY)) {
                indicatorValue = INDICATOR_VALUE_CLOSE;
            }
            else if (this.indicatorFieldSelection.equals(ADJ_CLOSE_ONLY)) {
                indicatorValue = INDICATOR_VALUE_ADJ_CLOSE;
            }
        }
        else if (this.includeAdjustedClose != null) {
            includeAdjCloseValue = this.includeAdjustedClose;
        }

        map.put(ParamKeys.INCLUDE_ADJ_CLOSE, Boolean.toString(includeAdjCloseValue));
        if (indicatorValue != null) {
            map.put(ParamKeys.INDICATORS, indicatorValue);
        }
        if (this.includeTimestamps != null) {
            map.put(ParamKeys.INCLUDE_TIMESTAMPS, this.includeTimestamps.toString());
        }
        if (this.includePrePost != null) {
            map.put(ParamKeys.INCLUDE_PRE_POST, this.includePrePost.toString());
        }

        if (this.numberOfPoints != null) {
            map.put(ParamKeys.KEY_NUMBER_OF_POINTS, this.numberOfPoints.toString());
        }

        return map;
    }

    @Override
    protected ChartBuilder getThis() {
        return this;
    }

}
