package com.github.bradjacobs.yahoofinance.demo.lookup;

import com.github.bradjacobs.yahoofinance.request.YahooFinanceRequest;
import com.github.bradjacobs.yahoofinance.request.builder.YahooRequestBuilder;
import com.github.bradjacobs.yahoofinance.types.Type;

/**
 * Example of LookUp requests
 *   Basically give it a lookup string.
 *
 *   _NOTE_ there is probably special syntax supported (regex?) but unable to find any documentation.
 */
public class LookupRequestDemoFactory
{
    private LookupRequestDemoFactory() {}

    // note: the numeric id value has no real meaning
    public static YahooFinanceRequest getRequest(int sampleRequestId)
    {
        switch (sampleRequestId) {
            case 1: return SIMPLE;
            default: return SIMPLE;
        }
    }

    /**
     * Query for EQUITIES
     *    with a search string: "starts with 'AA'"
     */
    private static final YahooFinanceRequest SIMPLE =
        YahooRequestBuilder.api()
            .lookup()
            .withQuery("AA*")
            .withType(Type.EQUITY)
            .build();




}
