package com.github.bradjacobs.yahoofinance.response;

public class ResponseConverterConfig
{
    private static final boolean DEFAULT_USE_DATE_MAP_KEY = true;

    private static final boolean DEFAULT_AUTO_DETECT_DATE_TIME = true;

    private final boolean useDateAsMapKey;
    private final boolean autoDetectDateTime;


    public static final ResponseConverterConfig DEFAULT_INSTANCE = new ResponseConverterConfig();


    public ResponseConverterConfig()
    {
        this(DEFAULT_USE_DATE_MAP_KEY, DEFAULT_AUTO_DETECT_DATE_TIME);
    }

    public ResponseConverterConfig(boolean useDateAsMapKey, boolean autoDetectDateTime)
    {
        this.useDateAsMapKey = useDateAsMapKey;
        this.autoDetectDateTime = autoDetectDateTime;
    }

    public boolean isUseDateAsMapKey()
    {
        return useDateAsMapKey;
    }

    public boolean isAutoDetectDateTime()
    {
        return autoDetectDateTime;
    }
}
