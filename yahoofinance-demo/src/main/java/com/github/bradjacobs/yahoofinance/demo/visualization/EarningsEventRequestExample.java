package com.github.bradjacobs.yahoofinance.demo.visualization;

import com.github.bradjacobs.yahoofinance.demo.RequestExample;
import com.github.bradjacobs.yahoofinance.request.YahooFinanceRequest;
import com.github.bradjacobs.yahoofinance.request.builder.YahooRequestBuilder;
import com.github.bradjacobs.yahoofinance.types.Type;

public enum EarningsEventRequestExample implements RequestExample
{
    SIMPLE {
        @Override
        public YahooFinanceRequest getRequest() {
            return YahooRequestBuilder.api()
                    .earningsEvent()
                    .setStart("2021-11-01")
                    .build();
        }
    };

}
