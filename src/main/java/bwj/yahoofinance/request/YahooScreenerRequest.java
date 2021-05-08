/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package bwj.yahoofinance.request;

import bwj.yahoofinance.request.builder.BaseRequestParamMapBuilder;
import bwj.yahoofinance.request.builder.ParamKeys;
import bwj.yahoofinance.types.YahooEndpoint;
import bwj.yahoofinance.types.screener.Operand;
import bwj.yahoofinance.types.screener.Operator;
import bwj.yahoofinance.types.screener.Query;
import bwj.yahoofinance.types.screener.ScreenerCriteria;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

public class YahooScreenerRequest extends YahooFinanceRequest
{

    // screener criteria will ultimately represent the "post body" of the request.
    private final ScreenerCriteria criteria;


    protected YahooScreenerRequest(YahooEndpoint endpoint, Map<String,String> paramMap, ScreenerCriteria criteria)
    {
        super(endpoint, "", paramMap);
        this.criteria = criteria;
    }

    public ScreenerCriteria getCriteria()
    {
        return criteria;
    }

    public static class Builder extends BaseRequestParamMapBuilder<Builder>
    {
        private Boolean formatted;
        private int size = 0; // default
        private int offset = 25;

        private String sortField = "intradaymarketcap";
        private String sortType = "DESC";

        private ScreenerQueryBuilder queryBuilder = new ScreenerQueryBuilder();

        // thse remain const until there's need otherwise.
        private String quoteType = "EQUITY";
        private String topOperator = Operator.AND.getValue();
        private String userId = "";
        private String userIdType = "guid";



        public Builder setFormatted(Boolean formatted) {
            this.formatted = formatted;
            return this;
        }
        public Builder setSize(int size) {
            this.size = Math.max(size, 0); // no negative allowed
            return this;
        }
        public Builder setOffset(int offset) {
            this.offset = Math.max(offset, 0); // no negative allowed
            return this;
        }
        public Builder setSortField(String sortField) {
            this.sortField = sortField;
            return this;
        }
        public Builder setSortType(String sortType) {
            this.sortField = sortField;
            return this;
        }


        // TODO: methods need better names.
        public Builder addFilterEq(String field, Number value)
        {
            this.queryBuilder.eq(field, value);
            return this;
        }
        public Builder addFilterLt(String field, Number value)
        {
            this.queryBuilder.lt(field, value);
            return this;
        }
        public Builder addFilterGt(String field, Number value)
        {
            this.queryBuilder.gt(field, value);
            return this;
        }
        public Builder addFilterBetween(String field, Number value1, Number value2)
        {
            this.queryBuilder.between(field, value1, value2);
            return this;
        }
        public Builder addFilterInList(String field, List<String> values)
        {
            this.queryBuilder.in(field, values);
            return this;
        }
        public Builder addFilterEq(String field, String value)
        {
            this.queryBuilder.eq(field, value);
            return this;
        }


        @Override
        protected Map<String, String> buildRequestSpecificMap()
        {
            Map<String,String> map = new LinkedHashMap<>();
            if (this.formatted != null) {
                map.put(ParamKeys.FORMATTED, Boolean.toString(this.formatted));
            }
            return map;
        }


        public YahooScreenerRequest build()
        {
            ScreenerCriteria criteria = new ScreenerCriteria();
            criteria.setSize(size);
            criteria.setOffset(offset);
            criteria.setSortField(sortField);
            criteria.setSortType(sortType);
            criteria.setQuoteType(quoteType);
            criteria.setTopOperator(topOperator);
            criteria.setUserId(userId);
            criteria.setUserIdType(userIdType);

            Query query = this.queryBuilder.build();
            criteria.setQuery(query);

            YahooScreenerRequest req = new YahooScreenerRequest(YahooEndpoint.SCREENER, this.buildParamMap(), criteria);
            return req;
        }

        @Override
        protected Builder getThis() {
            return this;
        }
    }





    private static class ScreenerQueryBuilder
    {
        private static final Operator op = Operator.AND;  // unchangable (for now)


        private Map<String, Operand> fieldOperandMap = new LinkedHashMap<>();  // preserving creation order.

        public ScreenerQueryBuilder eq(String field, Number value)
        {
            fieldOperandMap.put(field, generateRestriction(field, Operator.EQUAL, value));
            return this;
        }
        public ScreenerQueryBuilder lt(String field, Number value)
        {
            fieldOperandMap.put(field, generateRestriction(field, Operator.LESS_THAN, value));
            return this;
        }
        public ScreenerQueryBuilder gt(String field, Number value)
        {
            fieldOperandMap.put(field, generateRestriction(field, Operator.GREATER_THAN, value));
            return this;
        }
        public ScreenerQueryBuilder between(String field, Number value1, Number value2)
        {
            fieldOperandMap.put(field, generateRestriction(field, Operator.BETWEEN, value1, value2));
            return this;
        }
        public ScreenerQueryBuilder in(String field, List<String> values)
        {
            fieldOperandMap.put(field, generateInListRestriction(field, values));
            return this;
        }
        public ScreenerQueryBuilder eq(String field, String value)
        {
            fieldOperandMap.put(field, generateRestriction(field, Operator.EQUAL, value));
            return this;
        }

        public Query build() {
            Query query = new Query();
            query.setOperator(op.getValue());
            query.setOperands(new ArrayList<>(fieldOperandMap.values()));
            return query;
        }

        private static Operand generateRestriction(String field, Operator op, Object value)
        {
            Operand operand = new Operand();
            operand.setOperator(op.getValue());
            operand.setOperands(Arrays.asList(field, value));
            return operand;
        }

        private static Operand generateRestriction(String field, Operator op, Object value1, Object value2)
        {
            Operand operand = new Operand();
            operand.setOperator(op.getValue());
            operand.setOperands(Arrays.asList(field, value1, value2));
            return operand;
        }

        private static Operand generateInListRestriction(String field, List<String> values)
        {
            Operand operand = new Operand();
            operand.setOperator(Operator.OR.getValue());

            List<Object> subOpList = new ArrayList<>();
            for (String value : values)
            {
                Operand valueOperand = generateRestriction(field, Operator.EQUAL, value);
                subOpList.add(valueOperand);
            }
            operand.setOperands(subOpList);
            return operand;
        }
    }
}