package com.github.bradjacobs.yahoofinance.request.builder;

import com.github.bradjacobs.yahoofinance.converter.datetime.MetaEpochSecondsConverter;
import com.github.bradjacobs.yahoofinance.request.YahooFinanceBatchRequest;
import com.github.bradjacobs.yahoofinance.request.YahooFinanceRequest;
import com.github.bradjacobs.yahoofinance.types.YahooEndpoint;
import com.github.bradjacobs.yahoofinance.types.screener.Operand;
import com.github.bradjacobs.yahoofinance.types.screener.Operator;
import com.github.bradjacobs.yahoofinance.types.screener.Query;
import com.github.bradjacobs.yahoofinance.types.screener.VisualizationCriteria;
import org.apache.commons.lang3.StringUtils;

import java.util.ArrayList;
import java.util.Arrays;
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

    private boolean isAggregate = false;  // todo - always false for now
    private static final String SORT_TYPE = "ASC";      // for now sort order is const

    private static final String DATE_CRITERIA_FIELD_NAME = "startdatetime";

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
    protected String _getRequestTicker() {
        return "";
    }

    @Override
    protected Map<String, String> _buildParamMap() {
        return new LinkedHashMap<>();
    }

    @Override
    protected Object _buildRequestPostBody()
    {
        VisualizationCriteria criteria = new VisualizationCriteria();
        criteria.setSize(size);
        criteria.setOffset(offset);
        criteria.setSortField(getSortField());
        criteria.setSortType(SORT_TYPE);
        criteria.setEntityIdType(getEntityType());
        criteria.setIncludeFields(getIncludeFields());

        Query query = new Query();
        query.setOperator(Operator.AND.getValue()); // unchangeable for now

        List<Operand> operandList = new ArrayList<>();

        MetaEpochSecondsConverter metaEpochSecondsConverter = MetaEpochSecondsConverter.getInstance();
        if (this.startPeriod != null)
        {
            String startPeriodString = metaEpochSecondsConverter.toDateString(this.startPeriod);
            if (this.endPeriod != null) {
                String endPeriodString = metaEpochSecondsConverter.toDateString(this.endPeriod);
                operandList.add(generateRestriction(DATE_CRITERIA_FIELD_NAME, Operator.BETWEEN, startPeriodString, endPeriodString));
            }
            else {
                // todo == might be able to do a simple EQ
                String endPeriodString = metaEpochSecondsConverter.toDateString(this.startPeriod + ONE_DAY_SECONDS);
                operandList.add(generateRestriction(DATE_CRITERIA_FIELD_NAME, Operator.GREATER_THAN_EQUAL, startPeriodString));
                operandList.add(generateRestriction(DATE_CRITERIA_FIELD_NAME, Operator.LESS_THAN, endPeriodString));
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
            operandList.add(generateRestriction(ParamKeys.REGION, Operator.EQUAL, region.toLowerCase()));
        }

        query.setOperands(operandList);
        criteria.setQuery(query);
        return criteria;
    }

    @Override
    protected YahooFinanceRequest generateRequest(YahooEndpoint endpoint, String ticker,
                                                  Map<String, String> paramMap, Object postBody, Map<String,String> headerMap)
    {
        YahooFinanceRequest req = super.generateRequest(endpoint, ticker, paramMap, postBody, headerMap);
        if (!isAggregate && size >= MIN_BATCHABLE_SIZE) {
            req = new YahooFinanceBatchRequest(req, this);
        }
        return req;
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


    private static Operand generateRestriction(String fieldName, Operator op, Object value)
    {
        Operand operand = new Operand();
        operand.setOperator(op.getValue());
        operand.setOperands(Arrays.asList(fieldName, value));
        return operand;
    }

    private static Operand generateRestriction(String fieldName, Operator op, Object value1, Object value2)
    {
        Operand operand = new Operand();
        operand.setOperator(op.getValue());
        operand.setOperands(Arrays.asList(fieldName, value1, value2));
        return operand;
    }
}
