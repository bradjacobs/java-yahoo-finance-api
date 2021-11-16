package com.github.bradjacobs.yahoofinance.demo.lookup;

import com.github.bradjacobs.yahoofinance.demo.RequestExample;
import com.github.bradjacobs.yahoofinance.request.YahooFinanceRequest;
import com.github.bradjacobs.yahoofinance.request.builder.YahooRequestBuilder;
import com.github.bradjacobs.yahoofinance.types.Type;

public enum LookupRequestExample implements RequestExample
{
    SIMPLE {
        @Override
        public YahooFinanceRequest getRequest() {
            return YahooRequestBuilder.api()
                    .lookup()
                    .withQuery("AA*")
                    .withType(Type.EQUITY)
                    .build();
        }
    };

}
