/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package bwj.yahoofinance.request;

import bwj.yahoofinance.enums.YahooEndpoint;
import bwj.yahoofinance.enums.Interval;
import bwj.yahoofinance.enums.Range;

import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;

import static bwj.yahoofinance.request.YahooPriceHistoryRequest.IndicatorFieldSelection.*;

public class YahooPriceHistoryRequest extends YahooFinanceRequest
{
    private static final String EVENT_VALUE_DIV = "div";
    private static final String EVENT_VALUE_SPLITS = "split";

    private static final String INDICATOR_VALUE_CLOSE = "close";
    private static final String INDICATOR_VALUE_ADJ_CLOSE = "adjclose";


    protected YahooPriceHistoryRequest(Builder builder)
    {
        super(builder.getEndpoint(), builder.getTicker(), builder.generateParamMap());
    }


    public static class Builder extends PeriodRangeRequestBuilder<Builder>
    {
        private String ticker;
        private final YahooEndpoint endpoint = YahooEndpoint.CHART;
        private Range range;
        private Interval interval;
        private Boolean formatted; // this seems only applicable when response includes div or splits
        private Set<String> eventValues = new LinkedHashSet<>();
        private Boolean includeAdjustedClose = Boolean.TRUE;
        private IndicatorFieldSelection indicatorFieldSelection;
        private Boolean includeTimestamps;
        private Boolean includePrePost;

        // for any other misc params
        private Map<String,String> paramMap = new LinkedHashMap<>();

        @Override
        protected Builder getThis() {
            return this;
        }

        public Builder withTicker(String ticker) {
            this.ticker = ticker;
            return this;
        }
        public Builder withRange(Range range) {
            this.range = range;
            return this;
        }

        public Builder withInterval(Interval inverval) {
            this.interval = inverval;
            return this;
        }
        public Builder withFormatted(Boolean formatted) {
            this.formatted = formatted;
            return this;
        }
        public Builder withDividends(Boolean dividends) {
            if (Boolean.TRUE.equals(dividends)) {
                this.eventValues.add(EVENT_VALUE_DIV);
            }
            else {
                this.eventValues.remove(EVENT_VALUE_DIV);
            }
            return this;
        }
        public Builder withSplits(Boolean splits) {
            if (Boolean.TRUE.equals(splits)) {
                this.eventValues.add(EVENT_VALUE_SPLITS);
            }
            else {
                this.eventValues.remove(EVENT_VALUE_SPLITS);
            }
            return this;
        }
        public Builder withAdjClose(Boolean adjClose) {
            this.includeAdjustedClose = adjClose;
            return this;
        }

        //  convenience methods for when response is to only return certain fields
        //   (b/c not very intuitive)
        public Builder withIndicatorCloseAdjCloseOnly() {
            this.indicatorFieldSelection = CLOSE_ADJCLOSE;
            return this;
        }
        public Builder withIndicatorCloseOnly() {
            this.indicatorFieldSelection = CLOSE_ONLY;
            return this;
        }
        public Builder withIndicatorAdjCloseOnly() {
            this.indicatorFieldSelection = ADJ_CLOSE_ONLY;
            return this;
        }
        public Builder withIndicatorAllFields() {
            this.indicatorFieldSelection = ALL;
            return this;
        }

        public Builder withTimestamps(Boolean includeTimestamps) {
            this.includeTimestamps = includeTimestamps;
            return this;
        }

        public Builder withPrePost(Boolean includePrePost) {
            this.includePrePost = includePrePost;
            return this;
        }

        public Builder withParam(String key, String value) {
            if (key != null && value != null) {
                this.paramMap.put(key.trim(), value.trim());
            }
            return this;
        }

        public YahooEndpoint getEndpoint() {
            return endpoint;
        }

        public String getTicker() {
            return ticker;
        }

        public Map<String,String> generateParamMap() {

            Map<String,String> map = new LinkedHashMap<>();

            // if startPeriod is set, then it takes precedence over the 'range' parameter.
            if (this.startPeriod != null)
            {
                map.put(ParamKeys.PERIOD1, this.startPeriod.toString());
                if (this.endPeriod != null) {
                    map.put(ParamKeys.PERIOD2, this.endPeriod.toString());
                }
            }
            else if (this.range != null) {
                map.put(ParamKeys.RANGE, this.range.getValue());
            }

            if (this.interval != null) {
                map.put(ParamKeys.INTERVAL, this.interval.getValue());
            }
            if (this.formatted != null) {
                map.put(ParamKeys.FORMATTED, this.formatted.toString().toLowerCase());
            }

            if (this.eventValues.size() > 0) {
                // delim can be ',' or '|'
                String eventValueString = String.join(",", eventValues);
                map.put(ParamKeys.EVENTS, eventValueString);
            }

            boolean includeAdjCloseValue = true;
            String indicatorValue = null;
            if (this.indicatorFieldSelection != null)
            {
                includeAdjCloseValue = !this.indicatorFieldSelection.equals(CLOSE_ONLY);

                if (this.indicatorFieldSelection.equals(CLOSE_ADJCLOSE) || this.indicatorFieldSelection.equals(CLOSE_ONLY)) {
                    indicatorValue = INDICATOR_VALUE_CLOSE;
                }
                else if (this.indicatorFieldSelection.equals(ADJ_CLOSE_ONLY)) {
                    indicatorValue = INDICATOR_VALUE_ADJ_CLOSE;
                }
            }
            else if (this.includeAdjustedClose != null) {
                includeAdjCloseValue = this.includeAdjustedClose;
            }

            map.put(ParamKeys.INCLUDE_ADJ_CLOSE, Boolean.toString(includeAdjCloseValue).toLowerCase());
            if (indicatorValue != null) {
                map.put(ParamKeys.INDICATORS, indicatorValue);
            }
            if (this.includeTimestamps != null) {
                map.put(ParamKeys.INCLUDE_TIMESTAMPS, this.includeTimestamps.toString().toLowerCase());
            }
            if (this.includePrePost != null) {
                map.put(ParamKeys.INCLUDE_PRE_POST, this.includePrePost.toString().toLowerCase());
            }

            map.putAll(paramMap);
            return map;
        }

        public YahooPriceHistoryRequest build() {

            YahooPriceHistoryRequest req = new YahooPriceHistoryRequest(this);
            return req;
        }

    }


    enum IndicatorFieldSelection
    {
        ALL,
        CLOSE_ADJCLOSE,
        CLOSE_ONLY,
        ADJ_CLOSE_ONLY
    }
}
