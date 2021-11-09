package com.github.bradjacobs.yahoofinance.request.builder;

import com.github.bradjacobs.yahoofinance.batch.FullBatchResponseChecker;
import com.github.bradjacobs.yahoofinance.batch.VisualizationResponseChecker;
import com.github.bradjacobs.yahoofinance.converter.datetime.MetaEpochSecondsConverter;
import com.github.bradjacobs.yahoofinance.request.YahooFinanceBatchRequest;
import com.github.bradjacobs.yahoofinance.request.YahooFinanceRequest;
import com.github.bradjacobs.yahoofinance.types.YahooEndpoint;
import com.github.bradjacobs.yahoofinance.types.screener.Operand;
import com.github.bradjacobs.yahoofinance.types.screener.Operator;
import com.github.bradjacobs.yahoofinance.types.screener.Query;
import com.github.bradjacobs.yahoofinance.types.screener.VisualizationCriteria;
import org.apache.commons.lang3.StringUtils;

import java.util.*;

public class EarningsRequestBuilder extends BasePeriodRequestBuilder<EarningsRequestBuilder> implements BatchableRequestStrategy
{
    private int size = 100;
    private int offset = 0;

    private boolean isAggregate = false;  // todo - always false for now

    // for now sort fields are const
    private static final String SORT_FIELD = "companyshortname";
    private static final String SORT_TYPE = "ASC";

    private static final String ENTITY_TYPE = "earnings";
    private static final String DATE_CRITERIA_FIELD_NAME = "startdatetime";


    private static final long ONE_DAY_SECONDS = 24 * 60 * 60;
    private static final int MIN_BATCHABLE_SIZE = 10;

    private static final FullBatchResponseChecker BATCH_RESPONSE_CHECKER = new VisualizationResponseChecker();


    private List<String> includeFields = Arrays.asList(
            "ticker",
            "companyshortname",
            "startdatetime",
            "startdatetimetype",

            "epsestimate",
            "epsactual",
            "epssurprisepct",

            "sector",
            "industry",
            "beta",
            "bearish_count",   //


            //////"cashinterestpaid.lasttwelvemonths",   //
            //  ..
            // "cashinterestpaid" -> {LinkedHashMap@3013}  size = 4
            // key = "cashinterestpaid"
            // value = {LinkedHashMap@3013}  size = 4
            //  "lasttwelvemonths" -> {Double@3031} 6.377E7
            //  "yeartodate" -> null
            //  "annual" -> null
            //  "quarterly" -> null



            //"count",  // NOTE:  must NOT include count... it will A) not return anything.... B)...mess up the order!!!
            "dateisestimate",
            "epsconsensus",
            ///"eventId",   // not a field
            "eventname",
            "eventtype",
            "fiscalyear",
            "quarter",

            "timeZoneShortName"
            //"gmtOffsetMilliSeconds"
    );

    @Override
    protected YahooEndpoint _getRequestEndpoint()
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
        criteria.setSortField(SORT_FIELD);
        criteria.setSortType(SORT_TYPE);
        criteria.setEntityIdType(ENTITY_TYPE);
        criteria.setIncludeFields(includeFields);

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
            operandList.add(generateRestriction("region", Operator.EQUAL, region.toLowerCase()));
        }

        query.setOperands(operandList);
        criteria.setQuery(query);

        /*
          "query": {
    "operator": "and",
    "operands": [
      {
        "operator": "gte",
        "operands": [
          "startdatetime",
          "2021-11-05"
        ]
      },
      {
        "operator": "lt",
        "operands": [
          "startdatetime",
          "2021-11-06"
        ]
      },
      {
        "operator": "eq",
        "operands": [
          "region",
          "us"
        ]
      }
    ]

         */

//
//        Query query = this.queryBuilder.build();
//        criteria.setQuery(query);
        return criteria;
    }

    @Override
    protected YahooFinanceRequest generateRequest(YahooEndpoint endpoint, String ticker,
                                                  Map<String, String> paramMap, Object postBody, Map<String,String> headerMap)
    {
        BatchableRequestStrategy batchableRequestStrategy = this;

        if (isAggregate || size < MIN_BATCHABLE_SIZE) {
            batchableRequestStrategy = null;
        }

        return new YahooFinanceBatchRequest(endpoint, ticker, paramMap, postBody, headerMap, batchableRequestStrategy, BATCH_RESPONSE_CHECKER);
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

    @Override
    public YahooFinanceRequest buildNewRequest() {
        return this.build();
    }

    @Override
    protected EarningsRequestBuilder getThis() {
        return this;
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
