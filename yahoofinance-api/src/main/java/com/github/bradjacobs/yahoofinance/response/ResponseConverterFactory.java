package com.github.bradjacobs.yahoofinance.response;

import com.github.bradjacobs.yahoofinance.response.converter.*;
import com.github.bradjacobs.yahoofinance.types.YahooEndpoint;

public class ResponseConverterFactory
{
    // when true the top level map keys of timeseries will be 'date'
    //      false will be the 'attribute'
    private static final boolean DEFAULT_TIMESERIES_USE_DATE_MAP_KEY = true;

    // when true the top level map keys of spark will be 'date'
    //      false will be the 'ticker'
    private static final boolean DEFAULT_SPARK_USE_DATE_MAP_KEY = true;


    private static final ResponseConverterConfig DEFAULT_CONFIG = ResponseConverterConfig.DEFAULT_INSTANCE;


    private ResponseConverterFactory() { }


    /**
     * Get response converter based on endpoint
     * @param endpoint endpoint
     * @return
     */
    public static YahooResponseConverter getResponseConverter(YahooEndpoint endpoint)
    {
        return getResponseConverter(endpoint, DEFAULT_CONFIG);
    }


    /**
     * Get response converter based on endpoint
     * @param endpoint endpoint
     * @param config (optional) specific 'date' conversion settings.   Only used for certain converters
     * @return
     */
    public static YahooResponseConverter getResponseConverter(YahooEndpoint endpoint, ResponseConverterConfig config)
    {
        if (endpoint == null) {
            throw new IllegalArgumentException("Must provide an enpoint");
        }

        if (config == null) {
            config = DEFAULT_CONFIG;
        }

        switch (endpoint)
        {
            case CHART:
                return new ChartResponseConverter(config);
            case SCREENER:
                return new ScreenerResponseConverter();
            case QUOTE_SUMMARY:
                return new QuoteSummaryResponseConverter();
            case QUOTE:
                return new QuoteResponseConverter();
            case LOOKUP:
                return new LookupResponseConverter();
            case SPARK:
                return new SparkResponseConverter(config);  // todo: come back to the future of the config param
            case TIMESERIES:
                return new TimeSeriesResponseConverter(config); // todo: come back to the future of the config param
            default:
                return new DefaultResponseConverter();
        }
    }
}
