package com.github.bradjacobs.yahoofinance.request.builder;

import com.github.bradjacobs.yahoofinance.request.YahooRequest;
import com.github.bradjacobs.yahoofinance.request.batch.ParamMapSymbolBatchUpdater;
import com.github.bradjacobs.yahoofinance.request.batch.YahooBatchRequest;
import com.github.bradjacobs.yahoofinance.request.builder.helper.MultiTickerParamSet;
import com.github.bradjacobs.yahoofinance.types.Interval;
import com.github.bradjacobs.yahoofinance.types.Range;
import com.github.bradjacobs.yahoofinance.types.YahooEndpoint;

import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.Map;

public class SparkRequestBuilder extends BaseRequestBuilder<SparkRequestBuilder>
{
    // the batchSize limits the number of tickers on a single request to avoid
    // the GET url from becoming "too big"
    private static final int BATCH_SIZE = 200;

    private final MultiTickerParamSet tickerSet = new MultiTickerParamSet();
    private Range range;
    private Interval interval = Interval.ONE_DAY;  // default

    //these should be allowed, but haven't seem them work (thus far)
    //private Boolean includeTimestamps;
    //private Boolean includePrePost;

    public SparkRequestBuilder withTicker(String... tickers) {
        tickerSet.updateTickers(tickers);
        return this;
    }
    public SparkRequestBuilder withTicker(Collection<String> tickers) {
        if (tickers != null) {
            withTicker(tickers.toArray(new String[0]));
        }
        return this;
    }

    public SparkRequestBuilder withRange(Range range) {
        this.range = range;
        return this;
    }

    public SparkRequestBuilder withInterval(Interval interval) {
        this.interval = interval;
        return this;
    }

    @Override
    protected YahooEndpoint getEndpoint()
    {
        return YahooEndpoint.SPARK;
    }

    @Override
    protected String getRequestTicker()
    {
        return generateTickerString();
    }

    @Override
    protected Map<String, String> buildEndpointParamMap() {
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
    protected YahooRequest generateRequest(
            YahooEndpoint endpoint, String ticker,
            Map<String, String> paramMap, Object postBody, Map<String,String> headerMap)
    {
        YahooRequest req = super.generateRequest(endpoint, ticker, paramMap, postBody, headerMap);

        int tickerCount = this.tickerSet.size();
        if (tickerCount > BATCH_SIZE) {
            // todo - fix -- prob use more builder
            ParamMapSymbolBatchUpdater paramMapBatchUpdater = new ParamMapSymbolBatchUpdater(ParamKeys.SYMBOLS, BATCH_SIZE);
            req = new YahooBatchRequest(req, paramMapBatchUpdater, null, BATCH_SIZE, tickerCount);
        }

        return req;
    }

    @Override
    protected SparkRequestBuilder getThis() {
        return this;
    }

    private String generateTickerString() {
        return this.tickerSet.generateTickerString();
    }
}
