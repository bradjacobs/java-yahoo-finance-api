package com.github.bradjacobs.yahoofinance.request.builder;

import com.github.bradjacobs.yahoofinance.types.Type;
import com.github.bradjacobs.yahoofinance.types.YahooEndpoint;

import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

public class LookupRequestBuilder extends BaseRequestBuilder<LookupRequestBuilder> implements BatchableRequestBuilder
{
    private static final int DEFAULT_COUNT = 100;
    private static final int DEFAULT_START = 0;
    private static final int MIN_BATCHABLE_SIZE = 10;

    private String query;
    private Boolean formatted;
    private Type type;
    private int count = DEFAULT_COUNT;
    private int start = DEFAULT_START;

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
    public LookupRequestBuilder withCount(int count) {
        this.count = Math.max(count, 0); // no negative allowed
        return this;
    }
    public LookupRequestBuilder withStart(int start) {
        this.start = Math.max(start, 0); // no negative allowed
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

            map.put(ParamKeys.COUNT, String.valueOf(count));
            map.put(ParamKeys.START, String.valueOf(start));
        }
        return map;
    }

    @Override
    protected LookupRequestBuilder getThis() {
        return this;
    }

    @Override
    public int getBatchSize() {
        return this.count;
    }

    @Override
    public int getBatchOffset() {
        return this.start;
    }

    @Override
    public void setBatchOffset(int offset)
    {
        this.withStart(offset);
    }

    @Override
    protected BatchableRequestBuilder getAdditionalBatchableRequestBuilder() {
        if (!includeTotalsOnly && count >= MIN_BATCHABLE_SIZE) {
            return this;
        }
        return null;
    }
}
