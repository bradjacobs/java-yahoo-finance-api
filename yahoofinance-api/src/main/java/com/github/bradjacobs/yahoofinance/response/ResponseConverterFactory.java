package com.github.bradjacobs.yahoofinance.response;

import com.github.bradjacobs.yahoofinance.response.converter.ChartResponseConverter;
import com.github.bradjacobs.yahoofinance.response.converter.DefaultResponseConverter;
import com.github.bradjacobs.yahoofinance.response.converter.DefaultResponsePojoConverter;
import com.github.bradjacobs.yahoofinance.response.converter.LookupResponseConverter;
import com.github.bradjacobs.yahoofinance.response.converter.QuoteResponseConverter;
import com.github.bradjacobs.yahoofinance.response.converter.QuoteSummaryResponseConverter;
import com.github.bradjacobs.yahoofinance.response.converter.ResponseConverter;
import com.github.bradjacobs.yahoofinance.response.converter.ResponsePojoConverter;
import com.github.bradjacobs.yahoofinance.response.converter.ScreenerResponseConverter;
import com.github.bradjacobs.yahoofinance.response.converter.SparkResponseConverter;
import com.github.bradjacobs.yahoofinance.response.converter.TimeSeriesResponseConverter;
import com.github.bradjacobs.yahoofinance.response.converter.VisualizationResponseConverter;
import com.github.bradjacobs.yahoofinance.response.converter.YahooResponseConverter;
import com.github.bradjacobs.yahoofinance.types.YahooEndpoint;

public class ResponseConverterFactory
{
    private ResponseConverterFactory() { }


    /**
     * Get response converter based on endpoint
     * @param endpoint endpoint
     * @return converter
     */
    public static YahooResponseConverter getResponseConverter(YahooEndpoint endpoint)
    {
        if (endpoint == null) {
            throw new IllegalArgumentException("Must provide an endpoint");
        }

        ResponseConverter baseResponseConverter;

        switch (endpoint)
        {
            case CHART:
                baseResponseConverter = new ChartResponseConverter();
                break;
            case SCREENER:
            case PREMIUM_SCREENER:
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
                baseResponseConverter = new SparkResponseConverter();
                break;
            case TIMESERIES:
            case PREMIUM_TIMESERIES:
                baseResponseConverter = new TimeSeriesResponseConverter();
                break;
            case VISUALIZATION:
                baseResponseConverter = new VisualizationResponseConverter();
                break;
            default:
                baseResponseConverter = new DefaultResponseConverter();
        }

        ResponsePojoConverter pojoResponseConverter = new DefaultResponsePojoConverter(baseResponseConverter);

        return new YahooResponseConverter(baseResponseConverter, pojoResponseConverter);
    }
}
