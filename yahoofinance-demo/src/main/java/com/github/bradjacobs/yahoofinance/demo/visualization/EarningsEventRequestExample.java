package com.github.bradjacobs.yahoofinance.demo.visualization;

import com.github.bradjacobs.yahoofinance.demo.RequestExample;
import com.github.bradjacobs.yahoofinance.request.YahooRequest;
import com.github.bradjacobs.yahoofinance.request.builder.YahooRequestBuilder;

public enum EarningsEventRequestExample implements RequestExample
{
    SIMPLE {
        @Override
        public YahooRequest getRequest() {
            return YahooRequestBuilder.api()
                    .earningsEvent()
                    .setStart("2021-12-06")
                    .setEnd("2021-12-07")
                    .build();
        }
    };

}
