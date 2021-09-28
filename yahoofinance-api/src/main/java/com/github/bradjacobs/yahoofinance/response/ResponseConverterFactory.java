package com.github.bradjacobs.yahoofinance.response;

import com.github.bradjacobs.yahoofinance.response.converter.*;
import com.github.bradjacobs.yahoofinance.types.YahooEndpoint;

public class ResponseConverterFactory
{
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

        ResponseConverter baseResponseConverter = null;

        switch (endpoint)
        {
            case CHART:
                baseResponseConverter = new ChartResponseConverter(config);
                break;
            case SCREENER:
                baseResponseConverter = new ScreenerResponseConverter();
                break;
            case QUOTE_SUMMARY:
                baseResponseConverter = new QuoteSummaryResponseConverter();
                break;
            case QUOTE:
                baseResponseConverter = new QuoteResponseConverter();
                break;
            case LOOKUP:
                baseResponseConverter = new LookupResponseConverter();
                break;
            case SPARK:
                baseResponseConverter = new SparkResponseConverter(config);  // todo: come back to the future of the config param
                break;
            case TIMESERIES:
            case PREMIUM_TIMESERIES:
                baseResponseConverter = new TimeSeriesResponseConverter(config); // todo: come back to the future of the config param
                break;
            default:
                baseResponseConverter = new DefaultResponseConverter();
        }

        ResponsePojoConverter pojoResponseConverter = new DefaultResponsePojoConverter(baseResponseConverter);

        return new YahooResponseConverter(baseResponseConverter, pojoResponseConverter);
    }
}
