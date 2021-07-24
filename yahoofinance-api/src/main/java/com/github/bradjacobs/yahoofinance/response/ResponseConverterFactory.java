package com.github.bradjacobs.yahoofinance.response;

import com.github.bradjacobs.yahoofinance.types.YahooEndpoint;

public class ResponseConverterFactory
{
    // when true the top level map keys of timeseries will be 'date'
    //      false will be the 'attribute'
    private static final boolean DEFAULT_TIMESERIES_USE_DATE_MAP_KEY = true;

    // when true the top level map keys of spark will be 'date'
    //      false will be the 'ticker'
    private static final boolean DEFAULT_SPARK_USE_DATE_MAP_KEY = true;


    private ResponseConverterFactory() { }


    /**
     * Get response converter based on endpoint
     * @param endpoint endpoint
     * @return
     */
    public static YahooResponseConverter getResponseConverter(YahooEndpoint endpoint)
    {
        return getResponseConverter(endpoint, null);
    }


    /**
     * Get response converter based on endpoint
     * @param endpoint endpoint
     * @param useDateAsKey (optional) specify if you use 'date' as a map key (instead of attribute)...  ONLY applicable to certain endpoints.
     * @return
     */
    public static YahooResponseConverter getResponseConverter(YahooEndpoint endpoint, Boolean useDateAsKey)
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
                return new SparkResponseConverter(useDateAsKey != null ? useDateAsKey : DEFAULT_SPARK_USE_DATE_MAP_KEY);  // todo: come back to the future of the boolean param
            case TIMESERIES:
                return new TimeSeriesResponseConverter(useDateAsKey != null ? useDateAsKey : DEFAULT_TIMESERIES_USE_DATE_MAP_KEY); // todo: come back to the future of the boolean param
            default:
                return new DefaultResponseConverter();
        }
    }
}
