package com.github.bradjacobs.yahoofinance.request.builder;

import com.github.bradjacobs.yahoofinance.types.ScreenerField;
import com.github.bradjacobs.yahoofinance.types.Type;
import com.github.bradjacobs.yahoofinance.types.YahooEndpoint;
import com.github.bradjacobs.yahoofinance.types.screener.Operand;
import com.github.bradjacobs.yahoofinance.types.screener.Operator;
import com.github.bradjacobs.yahoofinance.types.screener.Query;
import com.github.bradjacobs.yahoofinance.types.screener.ScreenerCriteria;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

public class ScreenerBuilder extends BaseRequestBuilder<ScreenerBuilder>
{
    private static final String SORT_DESC = "DESC";
    private static final String SORT_ASC = "ASC";

    // __NOTE__: all variables are set to DEFAULT value
    private int size = 25;
    private int offset = 0;
    private ScreenerField sortField = ScreenerField.INTRADAYMARKETCAP;
    private String sortType = SORT_DESC;
    private Boolean formatted = null;

    private Boolean useRecordResponse = null;  // not sure what this actually does
    private Boolean totalOnly = null; // return record count only

    // thse remain const until there's need otherwise.
    private final Type quoteType = Type.EQUITY;
    private final String topOperator = Operator.AND.getValue();
    private final String userId = "";
    private final String userIdType = "guid";

    // side note:  it's possible to use 'entityIdType' instead of a quoteType, but is untested/unsupported for now

    private ScreenerQueryBuilder queryBuilder = new ScreenerQueryBuilder();


    public ScreenerBuilder()
    {
    }


    public ScreenerBuilder setFormatted(Boolean formatted) {
        this.formatted = formatted;
        return this;
    }
    public ScreenerBuilder setSize(int size) {
        this.size = Math.max(size, 0); // no negative allowed
        return this;
    }
    public ScreenerBuilder setOffset(int offset) {
        this.offset = Math.max(offset, 0); // no negative allowed
        return this;
    }
    public ScreenerBuilder setSortField(ScreenerField sortField) {
        this.sortField = sortField;
        return this;
    }
    public ScreenerBuilder sortDecending() {
        this.sortType = SORT_DESC;
        return this;
    }
    public ScreenerBuilder sortAscending() {
        this.sortType = SORT_ASC;
        return this;
    }
    public ScreenerBuilder setUseRecordResponse(Boolean useRecordResponse) {
        this.useRecordResponse = useRecordResponse;
        return this;
    }
    public ScreenerBuilder setTotalOnly(Boolean totalOnly) {
        this.totalOnly = totalOnly;
        return this;
    }


    public ScreenerBuilder eq(ScreenerField field, Long value)
    {
        this.queryBuilder.eq(field, value);
        return this;
    }
    public ScreenerBuilder lt(ScreenerField field, Number value)
    {
        this.queryBuilder.lt(field, value);
        return this;
    }
    public ScreenerBuilder gt(ScreenerField field, Number value)
    {
        this.queryBuilder.gt(field, value);
        return this;
    }
    public ScreenerBuilder btwn(ScreenerField field, Number value1, Number value2)
    {
        this.queryBuilder.between(field, value1, value2);
        return this;
    }
    public ScreenerBuilder in(ScreenerField field, List<String> values)
    {
        this.queryBuilder.in(field, adjustValuesCase(field, values));
        return this;
    }
    public ScreenerBuilder in(ScreenerField field, String ... values)
    {
        return (values != null ? in(field, Arrays.asList(values)) : this);
    }

    private List<String> adjustValuesCase(ScreenerField field, List<String> values) {
        // region must be lowercase in screener
        if (ScreenerField.REGION.equals(field)) {
            values.replaceAll(String::toLowerCase);
        }
        return values;
    }


    @Override
    protected YahooEndpoint _getRequestEndpoiint()
    {
        if (Boolean.TRUE.equals(this.totalOnly)) {
            return YahooEndpoint.SCREENER_TOTALS;
        }
        return YahooEndpoint.SCREENER;
    }


    @Override
    protected String _getRequestTicker()
    {
        return "";
    }

    @Override
    protected Map<String, String> _buildParamMap()
    {
        Map<String,String> map = new LinkedHashMap<>();
        if (this.formatted != null) {
            map.put(ParamKeys.FORMATTED, Boolean.toString(this.formatted));
        }

        if (! Boolean.TRUE.equals(this.totalOnly))
        {
            if (this.useRecordResponse != null) {
                map.put(ParamKeys.USE_RECORD_RESPONSE, Boolean.toString(this.useRecordResponse));
            }

            // note: apparently you can add 'fields' parameter on screener similar to quote endpoint.
            List<String> fieldList = QuoteFieldFactory.getQuoteFields(this.quoteType);
            String fieldValueString = String.join(",", fieldList);
            map.put(ParamKeys.FIELDS, fieldValueString);
        }

        return map;
    }

    @Override
    protected Object _buildRequestPostBody()
    {
        validateSortField(sortField);

        ScreenerCriteria criteria = new ScreenerCriteria();
        criteria.setSize(size);
        criteria.setOffset(offset);
        criteria.setSortField(sortField.getValue());
        criteria.setSortType(sortType);
        criteria.setQuoteType(quoteType.toString());
        criteria.setTopOperator(topOperator);
        criteria.setUserId(userId);
        criteria.setUserIdType(userIdType);

        Query query = this.queryBuilder.build();
        criteria.setQuery(query);
        return criteria;
    }

    @Override
    protected ScreenerBuilder getThis()
    {
        return this;
    }



    // todo - should make run w/ common validation
    private void validateSortField(ScreenerField sortField) {
        if (sortField == null) {
            throw new IllegalArgumentException("The sortField cannot be null.");
        }
        if (! sortField.isSortable()) {
            throw new IllegalArgumentException(String.format("Cannnot sort by '%s'.  It is not a sortable field", sortField.toString()));
        }
    }

    private static class ScreenerQueryBuilder
    {
        private static final Operator op = Operator.AND;  // unchangable (for now)


        private Map<ScreenerField, Operand> fieldOperandMap = new LinkedHashMap<>();  // preserving creation order.

        public ScreenerQueryBuilder eq(ScreenerField field, Long value)
        {
            fieldOperandMap.put(field, generateRestriction(field, Operator.EQUAL, value));
            return this;
        }
        public ScreenerQueryBuilder lt(ScreenerField field, Number value)
        {
            fieldOperandMap.put(field, generateRestriction(field, Operator.LESS_THAN, value));
            return this;
        }
        public ScreenerQueryBuilder gt(ScreenerField field, Number value)
        {
            fieldOperandMap.put(field, generateRestriction(field, Operator.GREATER_THAN, value));
            return this;
        }
        public ScreenerQueryBuilder between(ScreenerField field, Number value1, Number value2)
        {
            fieldOperandMap.put(field, generateRestriction(field, Operator.BETWEEN, value1, value2));
            return this;
        }
        public ScreenerQueryBuilder in(ScreenerField field, List<String> values)
        {
            fieldOperandMap.put(field, generateInListRestriction(field, values));
            return this;
        }

        public Query build() {
            Query query = new Query();
            query.setOperator(op.getValue());
            query.setOperands(new ArrayList<>(fieldOperandMap.values()));
            return query;
        }

        private static Operand generateRestriction(ScreenerField field, Operator op, Object value)
        {
            Operand operand = new Operand();
            operand.setOperator(op.getValue());
            operand.setOperands(Arrays.asList(field.getValue(), value));
            return operand;
        }

        private static Operand generateRestriction(ScreenerField field, Operator op, Object value1, Object value2)
        {
            Operand operand = new Operand();
            operand.setOperator(op.getValue());
            operand.setOperands(Arrays.asList(field.getValue(), value1, value2));
            return operand;
        }

        private static Operand generateInListRestriction(ScreenerField field, List<String> values)
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
