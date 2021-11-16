package com.github.bradjacobs.yahoofinance.demo.visualization;

import com.github.bradjacobs.yahoofinance.demo.RequestExample;
import com.github.bradjacobs.yahoofinance.request.YahooFinanceRequest;
import com.github.bradjacobs.yahoofinance.request.builder.YahooRequestBuilder;

public enum IpoEventRequestExample implements RequestExample
{
    SIMPLE {
        @Override
        public YahooFinanceRequest getRequest() {
            return YahooRequestBuilder.api()
                    .ipoEvent()
                    .setStart("2021-11-02")
                    .build();
        }
    };

}
