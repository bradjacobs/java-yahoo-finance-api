package com.github.bradjacobs.yahoofinance.request.builder.helper;

import com.github.bradjacobs.yahoofinance.types.criteria.CriteriaKey;
import com.github.bradjacobs.yahoofinance.types.criteria.Operand;
import com.github.bradjacobs.yahoofinance.types.criteria.Operator;
import com.github.bradjacobs.yahoofinance.types.criteria.Query;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

public class QueryBuilder {

    private final Map<CriteriaKey, Operand> fieldOperandMap = new LinkedHashMap<>();  // preserving creation order.

    public QueryBuilder eq(CriteriaKey field, Long value) {
        fieldOperandMap.put(field, generateRestriction(field, Operator.EQUAL, value));
        return this;
    }
    public QueryBuilder eq(CriteriaKey field, String value) {
        fieldOperandMap.put(field, generateRestriction(field, Operator.EQUAL, value));
        return this;
    }

    public QueryBuilder lt(CriteriaKey field, Number value) {
        fieldOperandMap.put(field, generateRestriction(field, Operator.LESS_THAN, value));
        return this;
    }

    public QueryBuilder gt(CriteriaKey field, Number value) {
        fieldOperandMap.put(field, generateRestriction(field, Operator.GREATER_THAN, value));
        return this;
    }

    public QueryBuilder between(CriteriaKey field, Number value1, Number value2) {
        fieldOperandMap.put(field, generateRestriction(field, Operator.BETWEEN, value1, value2));
        return this;
    }
    public QueryBuilder between(CriteriaKey field, String value1, String value2) {
        fieldOperandMap.put(field, generateRestriction(field, Operator.BETWEEN, value1, value2));
        return this;
    }

    public QueryBuilder in(CriteriaKey field, List<String> values) {
        fieldOperandMap.put(field, generateInListRestriction(field, values));
        return this;
    }

    private Operand generateRestriction(CriteriaKey field, Operator op, Object value) {
        Operand operand = new Operand();
        operand.setOperator(op.getValue());
        operand.setOperands(Arrays.asList(field.getKeyName(), value));
        return operand;
    }

    private Operand generateRestriction(CriteriaKey field, Operator op, Object value1, Object value2) {
        Operand operand = new Operand();
        operand.setOperator(op.getValue());
        operand.setOperands(Arrays.asList(field.getKeyName(), value1, value2));
        return operand;
    }

    private Operand generateInListRestriction(CriteriaKey field, List<String> values) {
        Operand operand = new Operand();
        operand.setOperator(Operator.OR.getValue());

        List<Object> subOpList = new ArrayList<>();
        for (String value : values) {
            Operand valueOperand = generateRestriction(field, Operator.EQUAL, value);
            subOpList.add(valueOperand);
        }
        operand.setOperands(subOpList);
        return operand;
    }

    public Query build() {
        Query query = new Query();
        query.setOperator(Operator.AND.getValue());
        query.setOperands(new ArrayList<>(fieldOperandMap.values()));
        return query;
    }

}
