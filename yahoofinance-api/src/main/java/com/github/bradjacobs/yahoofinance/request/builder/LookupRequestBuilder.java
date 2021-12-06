package com.github.bradjacobs.yahoofinance.request.builder;

import com.github.bradjacobs.yahoofinance.request.YahooRequest;
import com.github.bradjacobs.yahoofinance.request.batch.ParamMapBatchOffsetUpdater;
import com.github.bradjacobs.yahoofinance.request.batch.ParamMapBatchUpdater;
import com.github.bradjacobs.yahoofinance.request.batch.YahooBatchRequest;
import com.github.bradjacobs.yahoofinance.types.Type;
import com.github.bradjacobs.yahoofinance.types.YahooEndpoint;

import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

// todo - come back and fix method naming
// todo - validation on missing query
public class LookupRequestBuilder extends BaseRequestBuilder<LookupRequestBuilder>
{
    private static final int MAX_BATCH_SIZE = 1000;
    private static final int DEFAULT_START = 0;
    private static final int DEFAULT_MAX_RESULTS = MAX_BATCH_SIZE;

    private String query;
    private Boolean formatted;
    private Type type;
    private int start = DEFAULT_START;
    private int maxResults = DEFAULT_MAX_RESULTS;

    private boolean includeTotalsOnly = false;

    @Override
    protected List<String> getRequiredParameters() {
        return Collections.singletonList(ParamKeys.QUERY);
    }

    public LookupRequestBuilder withQuery(String query) {
        this.query = query;
        return this;
    }
    public LookupRequestBuilder withTotalsOnly(boolean includeTotalsOnly) {
        this.includeTotalsOnly = includeTotalsOnly;
        return this;
    }
    public LookupRequestBuilder withFormatted(Boolean formatted) {
        this.formatted = formatted;
        return this;
    }
    public LookupRequestBuilder withType(Type type) {
        this.type = type;
        return this;
    }

    public LookupRequestBuilder withStart(int start) {
        this.start = Math.max(start, 0); // no negative allowed
        return this;
    }

    public LookupRequestBuilder setMaxResults(int maxResults) {
        this.maxResults = Math.max(maxResults, 0); // no negative allowed
        return this;
    }

    @Override
    public YahooEndpoint getEndpoint()
    {
        if (includeTotalsOnly) {
            return YahooEndpoint.LOOKUP_TOTALS;
        }
        return YahooEndpoint.LOOKUP;
    }

    @Override
    protected String getRequestTicker()
    {
        return "";
    }

    @Override
    protected Map<String, String> buildEndpointParamMap()
    {
        Map<String,String> map = new LinkedHashMap<>();
        if (this.query != null) {
            map.put(ParamKeys.QUERY, query.trim());
        }
        if (! this.includeTotalsOnly)
        {
            if (this.type != null) {
                map.put(ParamKeys.TYPE, type.toString().toLowerCase());
            }
            if (this.formatted != null) {
                map.put(ParamKeys.FORMATTED, formatted.toString());
            }

            map.put(ParamKeys.COUNT, String.valueOf(calculateRequestBatchSize()));
            map.put(ParamKeys.START, String.valueOf(start));
        }
        return map;
    }

    private int calculateRequestBatchSize() {
        return Math.min(MAX_BATCH_SIZE, this.maxResults);
    }


    @Override
    protected LookupRequestBuilder getThis() {
        return this;
    }

    @Override
    protected YahooRequest generateRequest(
            YahooEndpoint endpoint, String ticker,
            Map<String, String> paramMap, Object postBody, Map<String,String> headerMap)
    {
        YahooRequest req = super.generateRequest(endpoint, ticker, paramMap, postBody, headerMap);
        int batchSize = calculateRequestBatchSize();

        if (!includeTotalsOnly && this.maxResults > batchSize) {

            // todo - fix -- prob use more builder
            ParamMapBatchUpdater paramMapBatchUpdater = new ParamMapBatchOffsetUpdater(ParamKeys.START, batchSize);
            req = new YahooBatchRequest(req, paramMapBatchUpdater, null, batchSize, this.maxResults);
        }
        return req;
    }
}
