package com.github.bradjacobs.yahoofinance.response;

public class ResponseConverterConfig
{
    private static final boolean DEFAULT_USE_DATE_MAP_KEY = true;

    private static final boolean DEFAULT_AUTO_DETECT_DATE_TIME = true;

    private final boolean useDateAsMapKey;
    private final boolean autoDetechDateTime;


    public static final ResponseConverterConfig DEFAULT_INSTANCE = new ResponseConverterConfig();


    public ResponseConverterConfig()
    {
        this(DEFAULT_USE_DATE_MAP_KEY, DEFAULT_AUTO_DETECT_DATE_TIME);
    }

    public ResponseConverterConfig(boolean useDateAsMapKey, boolean autoDetechDateTime)
    {
        this.useDateAsMapKey = useDateAsMapKey;
        this.autoDetechDateTime = autoDetechDateTime;
    }

    public boolean isUseDateAsMapKey()
    {
        return useDateAsMapKey;
    }

    public boolean isAutoDetechDateTime()
    {
        return autoDetechDateTime;
    }
}
