package com.github.bradjacobs.yahoofinance.request.builder;

import com.github.bradjacobs.yahoofinance.request.YahooRequest;
import com.github.bradjacobs.yahoofinance.request.batch.ParamMapSymbolBatchUpdater;
import com.github.bradjacobs.yahoofinance.request.batch.YahooBatchRequest;
import com.github.bradjacobs.yahoofinance.request.builder.helper.MultiTickerParamSet;
import com.github.bradjacobs.yahoofinance.types.Type;
import com.github.bradjacobs.yahoofinance.types.YahooEndpoint;

import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

public class QuoteRequestBuilder extends BaseRequestBuilder<QuoteRequestBuilder>
{
    // the batchSize limits the number of tickers on a single request to avoid
    // the GET url from becoming "too big"
    private static final int BATCH_SIZE = 200;

    // use collection to allow for case where some endpoints allow multiple ticker values
    private final MultiTickerParamSet tickerSet = new MultiTickerParamSet();

    // todo - add batching logic if the ticker list is 'too big'
    public QuoteRequestBuilder withTicker(Collection<String> tickers) {
        if (tickers != null) {
            withTicker(tickers.toArray(new String[0]));
        }
        return this;
    }
    public QuoteRequestBuilder withTicker(String... tickers) {
        this.tickerSet.updateTickers(tickers);
        return this;
    }

    @Override
    protected YahooEndpoint getEndpoint()
    {
        return YahooEndpoint.QUOTE;
    }

    @Override
    protected String getRequestTicker()
    {
        return generateTickerString();
    }

    @Override
    protected Map<String, String> buildEndpointParamMap()
    {
        Map<String,String> requestParamMap = new LinkedHashMap<>();
        requestParamMap.put(ParamKeys.SYMBOLS, generateTickerString());

        // todo: right now only focusing on equities.  obviously this needs to be fixed when considering other types.
        List<String> fields = QuoteRequestFieldFactory.getQuoteFields(Type.EQUITY);
        String fieldValueString = String.join(",", fields);
        requestParamMap.put(ParamKeys.FIELDS, fieldValueString);

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
    protected QuoteRequestBuilder getThis()
    {
        return this;
    }

    private String generateTickerString() {
        return this.tickerSet.generateTickerString();
    }
}
