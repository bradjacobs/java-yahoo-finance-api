package com.github.bradjacobs.yahoofinance.response;

import com.github.bradjacobs.yahoofinance.types.YahooEndpoint;

public class ResponseConverterFactory
{

    // when true the top level map keys of timeseries will be 'date'
    //      false will be the 'attribute'
    private static final boolean DATE_TIMESERIES_MAP_KEY = true;


    private ResponseConverterFactory() { }

    public static YahooResponseConverter getResponseConverter(YahooEndpoint endpoint)
    {
        if (endpoint == null) {
            throw new IllegalArgumentException("Must provide an enpoint");
        }

        switch (endpoint)
        {
            case CHART:
                return new ChartResponseConverter();
            case SCREENER:
                return new ScreenerResponseConverter();
            case QUOTE_SUMMARY:
                return new QuoteSummaryResponseConverter();
            case QUOTE:
                return new QuoteResponseConverter();
            case LOOKUP:
                return new LookupResponseConverter();
            case SPARK:
                return new SparkResponseConverter();
            case TIMESERIES:
                return new TimeSeriesResponseConverter(DATE_TIMESERIES_MAP_KEY); // todo: come back to the future of the boolean param
            default:
                return new DefaultResponseConverter();
        }
    }
}
