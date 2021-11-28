package com.github.bradjacobs.yahoofinance.request.builder;

import com.github.bradjacobs.yahoofinance.converter.datetime.MetaEpochSecondsConverter;
import com.github.bradjacobs.yahoofinance.request.builder.helper.QueryBuilder;
import com.github.bradjacobs.yahoofinance.types.YahooEndpoint;
import com.github.bradjacobs.yahoofinance.types.criteria.CriteriaKey;
import com.github.bradjacobs.yahoofinance.types.criteria.Query;
import com.github.bradjacobs.yahoofinance.types.criteria.VisualizationCriteria;
import org.apache.commons.lang3.StringUtils;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * General Abstract class for different kinds of requests against the "visualization" endpoint.
 * @param <T>
 */
abstract public class AbstractVisualizationRequestBuilder<T extends AbstractVisualizationRequestBuilder<T>>
        extends BasePeriodRequestBuilder<T>
        implements BatchableRequestBuilder
{
    private int size = 100;
    private int offset = 0;

    // todo -- better home for this.
    private enum VisualizationCriteriaField implements CriteriaKey
    {
        START_DATE_TIME("startdatetime"),
        REGION("region");

        private final String key;
        VisualizationCriteriaField(String key) {
            this.key = key;
        }

        @Override
        public String getKeyName() {
            return key;
        }
    }

    private boolean isAggregate = false;  // todo - always false for now
    private boolean isSortDescending = false;  // todo - always false for now

    private static final long ONE_DAY_SECONDS = 24 * 60 * 60;
    private static final int MIN_BATCHABLE_SIZE = 10;

    abstract protected String getEntityType();
    abstract protected String getSortField();
    abstract protected List<String> getIncludeFields();

    @Override
    public YahooEndpoint getEndpoint()
    {
        return YahooEndpoint.VISUALIZATION;
    }

    @Override
    protected String getRequestTicker() {
        return "";
    }

    @Override
    protected Map<String, String> buildEndpointParamMap() {
        return new LinkedHashMap<>();
    }

    @Override
    protected Object buildRequestPostBody()
    {
        VisualizationCriteria criteria = new VisualizationCriteria();
        criteria.setSize(size);
        criteria.setOffset(offset);
        criteria.setSortField(getSortField());
        criteria.setIsSortDescending(isSortDescending);
        criteria.setEntityIdType(getEntityType());
        criteria.setIncludeFields(getIncludeFields());

        QueryBuilder queryBuilder = new QueryBuilder();
        MetaEpochSecondsConverter metaEpochSecondsConverter = MetaEpochSecondsConverter.getInstance();
        if (this.startPeriod != null) {
            String startPeriodString = metaEpochSecondsConverter.toDateString(this.startPeriod);
            if (this.endPeriod != null) {
                String endPeriodString = metaEpochSecondsConverter.toDateString(this.endPeriod);
                queryBuilder.between(VisualizationCriteriaField.START_DATE_TIME, startPeriodString, endPeriodString);
            }
            else {
                queryBuilder.eq(VisualizationCriteriaField.START_DATE_TIME, startPeriodString);
            }
        }

        // note: the aggregation can have a
        //  "operator": "eq",
        //        "operands": [
        //          "interval",
        //          "1d"
        //        ]

        String region = getRegion();
        if (! StringUtils.isEmpty(region)) {
            queryBuilder.eq(VisualizationCriteriaField.REGION, region.toLowerCase());
        }

        Query query = queryBuilder.build();
        criteria.setQuery(query);
        return criteria;
    }

    @Override
    protected BatchableRequestBuilder getAdditionalBatchableRequestBuilder() {
        if (!isAggregate && size >= MIN_BATCHABLE_SIZE) {
            return this;
        }
        return null;
    }

    @Override
    public int getBatchSize() {
        return size;
    }

    @Override
    public int getBatchOffset() {
        return offset;
    }

    @Override
    public void setBatchOffset(int offset) {
        this.offset = offset;
    }

}
