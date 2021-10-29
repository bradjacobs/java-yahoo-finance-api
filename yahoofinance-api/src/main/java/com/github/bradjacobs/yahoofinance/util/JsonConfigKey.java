package com.github.bradjacobs.yahoofinance.util;

import java.util.Objects;

/**
 * Used as a VERY limited config for JsonMapper.
 */
//  note:  is package-protected on purpose.
class JsonConfigKey {
    private final boolean makePretty;
    private final boolean useBigDecimal;

    public static final JsonConfigKey DEFAULT_CONFIG_KEY = new JsonConfigKey(false, false);

    public static JsonConfigKey getDefaultConfigKey() {
        return DEFAULT_CONFIG_KEY;
    }

    public JsonConfigKey(boolean makePretty, boolean useBigDecimal) {
        this.makePretty = makePretty;
        this.useBigDecimal = useBigDecimal;
    }

    public boolean isMakePretty() {
        return makePretty;
    }

    public boolean isUseBigDecimal() {
        return useBigDecimal;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        JsonConfigKey that = (JsonConfigKey) o;
        return makePretty == that.makePretty && useBigDecimal == that.useBigDecimal;
    }

    @Override
    public int hashCode() {
        return Objects.hash(makePretty, useBigDecimal);
    }
}
