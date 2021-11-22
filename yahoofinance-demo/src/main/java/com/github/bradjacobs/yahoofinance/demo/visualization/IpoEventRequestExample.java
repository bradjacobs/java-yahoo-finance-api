package com.github.bradjacobs.yahoofinance.demo.visualization;

import com.github.bradjacobs.yahoofinance.demo.RequestExample;
import com.github.bradjacobs.yahoofinance.request.YahooRequest;
import com.github.bradjacobs.yahoofinance.request.builder.YahooRequestBuilder;

public enum IpoEventRequestExample implements RequestExample
{
    SIMPLE {
        @Override
        public YahooRequest getRequest() {
            return YahooRequestBuilder.api()
                    .ipoEvent()
                    .setStart("2021-11-02")
                    .build();
        }
    };

}
