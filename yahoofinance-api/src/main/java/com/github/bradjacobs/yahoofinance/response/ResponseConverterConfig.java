package com.github.bradjacobs.yahoofinance.response;

// todo - come back and revisit all of this.
public class ResponseConverterConfig
{
    private static final boolean DEFAULT_USE_DATE_MAP_KEY = true;
    private static final boolean DEFAULT_AUTO_DETECT_DATE_TIME = true;
    private static final boolean DEFAULT_USE_BIG_DECIMALS = false;

    private boolean useDateAsMapKey = DEFAULT_USE_DATE_MAP_KEY;
    private boolean autoDetectDateTime = DEFAULT_AUTO_DETECT_DATE_TIME;
    private boolean useBigDecimals = DEFAULT_USE_BIG_DECIMALS;


    public static final ResponseConverterConfig DEFAULT_INSTANCE = builder().build();

    private ResponseConverterConfig(Builder builder)
    {
        this.useDateAsMapKey = builder.useDateAsMapKey;
        this.autoDetectDateTime = builder.autoDetectDateTime;
        this.useBigDecimals = builder.useBigDecimals;
    }

    public boolean isUseDateAsMapKey()
    {
        return useDateAsMapKey;
    }

    public boolean isAutoDetectDateTime()
    {
        return autoDetectDateTime;
    }

    public boolean isUseBigDecimals() {
        return useBigDecimals;
    }

    public static Builder builder() {
        return new Builder();
    }

    public static class Builder
    {
        private Builder() { }

        private boolean useDateAsMapKey = DEFAULT_USE_DATE_MAP_KEY;
        private boolean autoDetectDateTime = DEFAULT_AUTO_DETECT_DATE_TIME;
        private boolean useBigDecimals = DEFAULT_USE_BIG_DECIMALS;


        public Builder useDateAsMapKey(boolean useDateAsMapKey) {
            this.useDateAsMapKey = useDateAsMapKey;
            return this;
        }
        public Builder autoDetectDateTime(boolean autoDetectDateTime) {
            this.autoDetectDateTime = autoDetectDateTime;
            return this;
        }
        public Builder useBigDecimals(boolean useBigDecimals) {
            this.useBigDecimals = useBigDecimals;
            return this;
        }

        public ResponseConverterConfig build() {
            return new ResponseConverterConfig(this);
        }
    }
}
