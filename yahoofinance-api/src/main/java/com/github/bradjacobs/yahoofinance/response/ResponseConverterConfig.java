package com.github.bradjacobs.yahoofinance.response;

public class ResponseConverterConfig
{
    private static final boolean DEFAULT_USE_DATE_MAP_KEY = true;
    private static final boolean DEFAULT_USE_DATE_TIME = false;

    private final boolean useDateAsMapKey;
    private final boolean useDateTime;

    public ResponseConverterConfig()
    {
        this(DEFAULT_USE_DATE_MAP_KEY, DEFAULT_USE_DATE_TIME);
    }

    public ResponseConverterConfig(boolean useDateAsMapKey, boolean useDateTime)
    {
        this.useDateAsMapKey = useDateAsMapKey;
        this.useDateTime = useDateTime;
    }

    public boolean isUseDateAsMapKey()
    {
        return useDateAsMapKey;
    }

    public boolean isUseDateTime()
    {
        return useDateTime;
    }
}
