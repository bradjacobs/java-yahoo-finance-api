package com.github.bradjacobs.yahoofinance.request.builder;

import com.github.bradjacobs.yahoofinance.request.builder.helper.MultiTickerParamSet;
import com.github.bradjacobs.yahoofinance.types.Interval;
import com.github.bradjacobs.yahoofinance.types.Range;
import com.github.bradjacobs.yahoofinance.types.YahooEndpoint;

import java.util.*;

public class SparkBuilder extends BaseRequestBuilder<SparkBuilder>
{
    private final MultiTickerParamSet tickerSet = new MultiTickerParamSet();
    private Range range;
    private Interval interval = Interval.ONE_DAY;  // default


    //these should be allowed, but haven't seem them work (thus far)
    //  private Boolean includeTimestamps;
    //  private Boolean includePrePost;


    public SparkBuilder withTicker(String... tickers) {
        tickerSet.updateTickers(tickers);
        return this;
    }

    public SparkBuilder withRange(Range range) {
        this.range = range;
        return this;
    }

    public SparkBuilder withInterval(Interval interval) {
        this.interval = interval;
        return this;
    }

    @Override
    protected YahooEndpoint _getRequestEndpoint()
    {
        return YahooEndpoint.SPARK;
    }

    @Override
    protected String _getRequestTicker()
    {
        return generateTickerString();
    }

    @Override
    protected Map<String, String> _buildParamMap() {
        Map<String,String> requestParamMap = new LinkedHashMap<>();

        requestParamMap.put(ParamKeys.SYMBOLS, generateTickerString());

        if (this.range != null) {
            requestParamMap.put(ParamKeys.RANGE, this.range.getValue());
        }
        if (this.interval != null) {
            requestParamMap.put(ParamKeys.INTERVAL, this.interval.getValue());
        }

        return requestParamMap;
    }

    @Override
    protected SparkBuilder getThis() {
        return this;
    }


    private String generateTickerString() {
        return this.tickerSet.generateTickerString();
    }

}
