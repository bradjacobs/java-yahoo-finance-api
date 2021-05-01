package bwj.yahoofinance.model.request;

import bwj.yahoofinance.YahooEndpoint;
import bwj.yahoofinance.model.params.Interval;
import bwj.yahoofinance.model.params.Range;

import java.time.Instant;
import java.util.Date;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.TimeUnit;

import static bwj.yahoofinance.model.request.YahooPriceHistoryRequest.IndicatorFieldSelection.*;

public class YahooPriceHistoryRequest extends YahooFinanceRequest
{
    private static final String KEY_RANGE = "range";
    private static final String KEY_START = "period1";
    private static final String KEY_END = "period2";
    private static final String KEY_INTERVAL = "interval";
    private static final String KEY_INCLUDE_ADJ_CLOSE = "includeAdjustedClose";
    private static final String KEY_EVENTS = "events";
    private static final String KEY_INDICATORS = "indicators";
    private static final String KEY_FORMATTED = "formatted"; // only seems applicable when response includes div or splits
    private static final String KEY_INCLUDE_TIMESTAMPS = "includeTimestamps"; // default is true when unset
    //private static final String KEY_NUMBER_OF_POINTS = "numberOfPoints";
    //private static final String KEY_USE_YFID = "useYfid";

    private static final String EVENT_VALUE_DIV = "div";
    private static final String EVENT_VALUE_SPLITS = "split";


    public YahooPriceHistoryRequest() {
        super("", YahooEndpoint.CHART);
    }

    public YahooPriceHistoryRequest(String ticker) {
        super(ticker, YahooEndpoint.CHART);
    }


    @Override
    public void addEndpoint(YahooEndpoint... endpoints) {
        // ignore
    }


    public static class Builder
    {
        private String ticker;
        private Range range;
        private Long startPeriod;
        private Long endPeriod;
        private Interval interval;
        private Boolean formatted; // this seems only applicable when response includes div or splits
        private Set<String> eventValues = new LinkedHashSet<>();
        private Boolean includeAdjustedClose = Boolean.TRUE;
        private IndicatorFieldSelection indicatorFieldSelection;
        private Boolean includeTimestamps;

        // if the input epoch time is greater than this value, then assume MILLISECONDS
        //   otherwise assume SECONDS
        private static final long EPOCH_MILLI_THRESHOLD = 4000000000L;
        // todo: fix - put const above in single location

        // for any other misc params
        private Map<String,String> paramMap = new LinkedHashMap<>();

        public Builder withTicker(String ticker) {
            this.ticker = ticker;
            return this;
        }
        public Builder withRange(Range range) {
            this.range = range;
            return this;
        }

        public Builder withTimeRange(Long start, Long end) {
            return withStart(start).withEnd(end);
        }
        public Builder withTimeRange(Date start, Date end) {
            return withStart(start).withEnd(end);
        }
        public Builder withTimeRange(Instant start, Instant end) {
            return withStart(start).withEnd(end);
        }

        public Builder withStart(Instant start) {
            if (start != null) {
                this.startPeriod = start.toEpochMilli();
            }
            else {
                this.startPeriod = null;
            }
            return this;
        }
        public Builder withStart(Date start) {
            if (start != null) {
                this.startPeriod = start.getTime();
            }
            else {
                this.startPeriod = null;
            }
            return this;
        }
        public Builder withStart(Long start) {
            this.startPeriod = start;
            return this;
        }

        public Builder withEnd(Instant end) {
            if (end != null) {
                this.endPeriod = end.toEpochMilli();
            }
            else {
                this.endPeriod = null;
            }
            return this;
        }
        public Builder withEnd(Date end) {
            if (end != null) {
                this.endPeriod = end.getTime();
            }
            else {
                this.endPeriod = null;
            }
            return this;
        }
        public Builder withEnd(Long end) {
            this.endPeriod = end;
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

        //  convenience methods to have response only return certain fields
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
        public Builder withIncludeTimestamps(Boolean includeTimestamps) {
            this.includeTimestamps = includeTimestamps;
            return this;
        }

        public Builder withParam(String key, String value) {
            if (key != null && value != null) {
                this.paramMap.put(key.trim(), value.trim());
            }
            return this;
        }

        public YahooPriceHistoryRequest build() {
            YahooPriceHistoryRequest req = new YahooPriceHistoryRequest();
            req.setTicker(this.ticker);

            if (this.startPeriod != null)
            {
                Long start = this.startPeriod;
                if (start > EPOCH_MILLI_THRESHOLD) {
                    start /= 1000;
                }

                req.addParam(KEY_START, start.toString());
                if (this.endPeriod != null) {
                    Long end = this.endPeriod;
                    if (end > EPOCH_MILLI_THRESHOLD) {
                        end /= 1000;
                    }

                    req.addParam(KEY_END, end.toString());
                }
            }
            else if (this.range != null) {
                req.addParam(KEY_RANGE, this.range.getValue());
            }

            if (this.interval != null) {
                req.addParam(KEY_INTERVAL, this.interval.getValue());
            }
            if (this.formatted != null) {
                req.addParam(KEY_FORMATTED, this.formatted.toString().toLowerCase());
            }

            if (this.eventValues.size() > 0) {
                // delim can be ',' or '|'
                String eventValueString = String.join(",", eventValues);
                req.addParam(KEY_EVENTS, eventValueString);
            }

            boolean includeAdjCloseValue = true;
            String indicatorValue = null;
            if (this.indicatorFieldSelection != null)
            {
                includeAdjCloseValue = !this.indicatorFieldSelection.equals(CLOSE_ONLY);

                if (this.indicatorFieldSelection.equals(CLOSE_ADJCLOSE) || this.indicatorFieldSelection.equals(CLOSE_ONLY)) {
                    indicatorValue = "close";
                }
                else if (this.indicatorFieldSelection.equals(ADJ_CLOSE_ONLY)) {
                    indicatorValue = "adjclose";
                }
            }
            else if (this.includeAdjustedClose != null) {
                includeAdjCloseValue = this.includeAdjustedClose;
            }

            req.addParam(KEY_INCLUDE_ADJ_CLOSE, Boolean.toString(includeAdjCloseValue).toLowerCase());
            if (indicatorValue != null) {
                req.addParam(KEY_INDICATORS, indicatorValue);
            }
            if (this.includeTimestamps != null) {
                req.addParam(KEY_INCLUDE_TIMESTAMPS, this.includeTimestamps.toString().toLowerCase());
            }

            req.addParams(paramMap);
            return req;
        }
    }


    static enum IndicatorFieldSelection
    {
        ALL,
        CLOSE_ADJCLOSE,
        CLOSE_ONLY,
        ADJ_CLOSE_ONLY
    }
}
