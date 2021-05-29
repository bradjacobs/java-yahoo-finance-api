package bwj.yahoofinance.request.builder;

import bwj.yahoofinance.types.YahooEndpoint;
import org.apache.commons.lang3.NotImplementedException;

import java.util.LinkedHashMap;
import java.util.Map;

public class TimeSeriesBuilder extends BaseRequestBuilder<TimeSeriesBuilder>
{

    public TimeSeriesBuilder()
    {
        throw new NotImplementedException("TimeSeriesBuilder query is not yet implemented.");
    }

    @Override
    protected String _getRequestTicker()
    {
        return "";
    }

    @Override
    protected TimeSeriesBuilder getThis()
    {
        return this;
    }

    @Override
    protected Map<String, String> _buildParamMap()
    {
        Map<String,String> requestParamMap = new LinkedHashMap<>();
        // todo
        return requestParamMap;

    }

    @Override
    protected YahooEndpoint _getRequestEndpoiint()
    {
        return YahooEndpoint.TIMESERIES;
    }

}
