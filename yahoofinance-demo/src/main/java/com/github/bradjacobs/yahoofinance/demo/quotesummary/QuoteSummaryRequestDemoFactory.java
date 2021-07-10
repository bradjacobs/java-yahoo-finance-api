package com.github.bradjacobs.yahoofinance.demo.quotesummary;

import com.github.bradjacobs.yahoofinance.request.YahooFinanceRequest;
import com.github.bradjacobs.yahoofinance.request.builder.YahooRequestBuilder;

import static com.github.bradjacobs.yahoofinance.types.YahooModule.ASSET_PROFILE;
import static com.github.bradjacobs.yahoofinance.types.YahooModule.BALANCE_SHEET_HISTORY;
import static com.github.bradjacobs.yahoofinance.types.YahooModule.CASH_FLOW_STMT_HISTORY;
import static com.github.bradjacobs.yahoofinance.types.YahooModule.FINANCIAL_DATA;
import static com.github.bradjacobs.yahoofinance.types.YahooModule.INCOME_STMT_HISTORY;

public class QuoteSummaryRequestDemoFactory
{
    private QuoteSummaryRequestDemoFactory() {}

    // note: the numeric id value has no real meaning
    public static YahooFinanceRequest getRequest(int sampleRequestId)
    {
        switch (sampleRequestId) {
            case 1: return SIMPLE;
            case 2: return MULTI_MODULES;
            case 3: return MULTI_MODULES_ALTERNATIVE;
            case 4: return BAD_TICKER;
            default: return SIMPLE;
        }
    }



    /**
     * Get the assetProfile Module for AAPL
     */
    private static final YahooFinanceRequest SIMPLE =
        YahooRequestBuilder.api()
            .quoteSummary()
            .withModules(ASSET_PROFILE)
            .withTicker("AAPL")
            .build();

    /**
     * Get multiple module financial data for MSFT
     */
    private static final YahooFinanceRequest MULTI_MODULES =
        YahooRequestBuilder.api()
            .quoteSummary()
            .withModules(FINANCIAL_DATA)
            .withModules(BALANCE_SHEET_HISTORY)
            .withModules(INCOME_STMT_HISTORY)
            .withModules(CASH_FLOW_STMT_HISTORY)
            .withTicker("MSFT")
            .build();

    /**
     * Get multiple module financial data for MSFT  (different form)
     */
    private static final YahooFinanceRequest MULTI_MODULES_ALTERNATIVE =
        YahooRequestBuilder.api()
            .quoteSummary()
            .withModules(FINANCIAL_DATA, BALANCE_SHEET_HISTORY, INCOME_STMT_HISTORY, CASH_FLOW_STMT_HISTORY)
            .withTicker("MSFT")
            .build();



    /**
     * Request with invalid (non-existent) ticker, will result in a 404 response.
     */
    private static final YahooFinanceRequest BAD_TICKER =
        YahooRequestBuilder.api()
                .quoteSummary()
                .withModules(ASSET_PROFILE)
                .withTicker("FAKETICKERVALUE")
                .build();


}
