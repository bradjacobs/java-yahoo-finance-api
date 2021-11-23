package com.github.bradjacobs.yahoofinance.response;

// todo - come back and revisit all of this.
public class ResponseConverterConfig
{
    private static final boolean DEFAULT_USE_DATE_MAP_KEY = true;
    private static final boolean DEFAULT_AUTO_DETECT_DATE_TIME = true;

    private boolean useDateAsMapKey = DEFAULT_USE_DATE_MAP_KEY;
    private boolean autoDetectDateTime = DEFAULT_AUTO_DETECT_DATE_TIME;


    public static final ResponseConverterConfig DEFAULT_INSTANCE = builder().build();

    private ResponseConverterConfig(Builder builder)
    {
        this.useDateAsMapKey = builder.useDateAsMapKey;
        this.autoDetectDateTime = builder.autoDetectDateTime;
    }

    public boolean isUseDateAsMapKey()
    {
        return useDateAsMapKey;
    }

    public boolean isAutoDetectDateTime()
    {
        return autoDetectDateTime;
    }

    public static Builder builder() {
        return new Builder();
    }

    public static class Builder
    {
        private Builder() { }

        private boolean useDateAsMapKey = DEFAULT_USE_DATE_MAP_KEY;
        private boolean autoDetectDateTime = DEFAULT_AUTO_DETECT_DATE_TIME;


        public Builder useDateAsMapKey(boolean useDateAsMapKey) {
            this.useDateAsMapKey = useDateAsMapKey;
            return this;
        }
        public Builder autoDetectDateTime(boolean autoDetectDateTime) {
            this.autoDetectDateTime = autoDetectDateTime;
            return this;
        }

        public ResponseConverterConfig build() {
            return new ResponseConverterConfig(this);
        }
    }
}
