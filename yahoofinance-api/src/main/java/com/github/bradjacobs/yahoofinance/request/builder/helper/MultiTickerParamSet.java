package com.github.bradjacobs.yahoofinance.request.builder.helper;

import java.util.Arrays;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

public class MultiTickerParamSet
{
    private final Set<String> tickerSet = new LinkedHashSet<>();  // preserve insertion order

    public void updateTickers(String... tickers) {
        if (tickers != null && tickers.length > 0) {
            addTickers(tickers);
        }
        else {
            clearAllTickers();
        }
    }

    public void addTickers(String... tickers) {
        if (tickers != null && tickers.length > 0) {
            List<String> tickerList = Arrays.asList(tickers);
            tickerList.replaceAll(String::toUpperCase);  // make them all UPPERCASE
            this.tickerSet.addAll(tickerList);
        }
    }

    public void clearAllTickers() {
        tickerSet.clear();
    }

    public String generateTickerString() {
        if (this.tickerSet.size() > 0) {
            return String.join(",", this.tickerSet);
        }
        return "";
    }

}
