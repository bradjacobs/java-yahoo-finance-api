/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package com.github.bradjacobs.yahoofinance.request.builder;

import com.github.bradjacobs.yahoofinance.converter.datetime.EpochSecondsConverter;
import com.github.bradjacobs.yahoofinance.request.YahooRequest;
import com.github.bradjacobs.yahoofinance.request.batch.CriteriaPostBodyBatchUpdater;
import com.github.bradjacobs.yahoofinance.request.batch.PostBodyBatchUpdater;
import com.github.bradjacobs.yahoofinance.request.batch.YahooBatchRequest;
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
{
    private static final int MAX_BATCH_SIZE = 1000;
    private static final EpochSecondsConverter EPOCH_SECONDS_CONVERTER = new EpochSecondsConverter();

    private int offset = 0;
    private int maxResults = 100;

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


    abstract protected String getEntityType();
    abstract protected String getSortField();
    abstract protected List<String> getIncludeFields();

    public T setMaxResults(int maxResults) {
        this.maxResults = Math.max(maxResults, 0); // no negative allowed
        return getThis();
    }

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
        criteria.setSize(calculateRequestBatchSize());
        criteria.setOffset(offset);
        criteria.setSortField(getSortField());
        criteria.setIsSortDescending(isSortDescending);
        criteria.setEntityIdType(getEntityType());
        criteria.setIncludeFields(getIncludeFields());

        QueryBuilder queryBuilder = new QueryBuilder();
        if (this.startPeriod != null) {
            String startPeriodString = EPOCH_SECONDS_CONVERTER.toString(this.startPeriod);
            if (this.endPeriod != null) {
                String endPeriodString = EPOCH_SECONDS_CONVERTER.toString(this.endPeriod);
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

    private int calculateRequestBatchSize() {
        return Math.min(MAX_BATCH_SIZE, this.maxResults);
    }

    @Override
    protected YahooRequest generateRequest(
            YahooEndpoint endpoint, String ticker,
            Map<String, String> paramMap, Object postBody, Map<String,String> headerMap)
    {
        YahooRequest req = super.generateRequest(endpoint, ticker, paramMap, postBody, headerMap);
        if (!isAggregate && maxResults > MAX_BATCH_SIZE) {

            // todo - fix -- prob use more builder
            int batchSize = calculateRequestBatchSize();
            PostBodyBatchUpdater postBodyBatchUpdater = new CriteriaPostBodyBatchUpdater(batchSize);
            req = new YahooBatchRequest(req, null, postBodyBatchUpdater, batchSize, maxResults);
        }
        return req;
    }

}
