package com.github.bradjacobs.yahoofinance.request.builder;

import com.github.bradjacobs.yahoofinance.types.Interval;
import com.github.bradjacobs.yahoofinance.types.Range;
import com.github.bradjacobs.yahoofinance.types.YahooEndpoint;

import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class SparkBuilder extends BaseRequestBuilder<SparkBuilder>
{
    private Set<String> tickerSet = new LinkedHashSet<>();  // preserve insertion order
    private Range range;
    private Interval interval = Interval.ONE_DAY;  // default


    //these should be allowed, but haven't seem them work (thus far)
    //  private Boolean includeTimestamps;
    //  private Boolean includePrePost;


    public SparkBuilder withTicker(String... tickers) {
        if (tickers != null && tickers.length > 0) {
            List<String> tickerList = Arrays.asList(tickers);
            tickerList.replaceAll(String::toUpperCase);  // make them all UPPERCASE
            this.tickerSet.addAll(tickerList);
        }
        else {
            tickerSet.clear();
        }
        return this;
    }

    public SparkBuilder withRange(Range range) {
        this.range = range;
        return this;
    }

    public SparkBuilder withInterval(Interval inverval) {
        this.interval = inverval;
        return this;
    }

    @Override
    protected YahooEndpoint _getRequestEndpoiint()
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
        if (this.tickerSet.size() > 0) {
            return String.join(",", this.tickerSet);
        }
        return "";
    }

}
