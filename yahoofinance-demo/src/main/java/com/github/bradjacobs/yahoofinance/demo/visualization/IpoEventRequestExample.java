package com.github.bradjacobs.yahoofinance.demo.visualization;

import com.github.bradjacobs.yahoofinance.demo.RequestExample;
import com.github.bradjacobs.yahoofinance.request.YahooRequest;
import com.github.bradjacobs.yahoofinance.request.builder.YahooRequestBuilder;

public enum IpoEventRequestExample implements RequestExample
{
    // todo - need to review example, sometimes can get 'unexpected' response based on criteria
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
