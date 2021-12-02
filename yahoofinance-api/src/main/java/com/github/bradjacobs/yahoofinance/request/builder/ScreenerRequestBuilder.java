package com.github.bradjacobs.yahoofinance.request.builder;

import com.github.bradjacobs.yahoofinance.request.YahooRequest;
import com.github.bradjacobs.yahoofinance.request.batch.CriteriaPostBodyBatchUpdater;
import com.github.bradjacobs.yahoofinance.request.batch.PostBodyBatchUpdater;
import com.github.bradjacobs.yahoofinance.request.batch.YahooBatchRequest;
import com.github.bradjacobs.yahoofinance.request.builder.helper.QueryBuilder;
import com.github.bradjacobs.yahoofinance.types.Exchange;
import com.github.bradjacobs.yahoofinance.types.ScreenerField;
import com.github.bradjacobs.yahoofinance.types.Type;
import com.github.bradjacobs.yahoofinance.types.YahooEndpoint;
import com.github.bradjacobs.yahoofinance.types.criteria.CriteriaEnum;
import com.github.bradjacobs.yahoofinance.types.criteria.Operator;
import com.github.bradjacobs.yahoofinance.types.criteria.Query;
import com.github.bradjacobs.yahoofinance.types.criteria.ScreenerCriteria;

import java.util.Arrays;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

// todo - fix bug if offset value is manuall set
public class ScreenerRequestBuilder extends BaseRequestBuilder<ScreenerRequestBuilder>
{
    private static final int MIN_BATCHABLE_SIZE = 10;
    private static final Set<Exchange> NASDAQ_SUB_TYPES =
            new LinkedHashSet<>(Arrays.asList(Exchange.NASDAQGM, Exchange.NASDAQGS, Exchange.NASDAQCM));

    private boolean usePremium = false;

    // __NOTE__: all variables are set to DEFAULT value
    private int size = 250;  // note: going much bigger than 250 can result in a yahoo error saying the value is 'too big'
    private int offset = 0;
    private int maxResults = 1000;

    private ScreenerField sortField = ScreenerField.INTRADAYMARKETCAP;
    private boolean isSortDescending = true;
    private Boolean formatted = null;

    private Boolean useRecordResponse = null;  // not sure what this actually does
    private Boolean totalOnly = null; // return record count only

    // these remain const until there's need otherwise.
    //        side note:  it's possible to use 'entityIdType' instead of a quoteType, but is untested/unsupported for now
    private final Type quoteType = Type.EQUITY;
    private static final String TOP_OPERATOR = Operator.AND.getValue().toUpperCase();  // make uppercase only b/c the website does it.
    private static final String USER_ID = "";
    private static final String USER_ID_TYPE = "guid";


    // TODO - FIX... 'technically' if supply an industry w/o a sector then the sector could be 'auto-magically' added,
    //    but for now explicitly require sector is set if an industry is set (b/c that's what the web does)
    //      however the method of validation is kludgy.
    //    it's important to do this check, or else the query can produce 'zero results' and might not realize there was a problem.
    private boolean sectorIsSet = false;
    private boolean industryIsSet = false;
    private final QueryBuilder queryBuilder = new QueryBuilder();


    public ScreenerRequestBuilder()
    {
    }

    public ScreenerRequestBuilder setFormatted(Boolean formatted) {
        this.formatted = formatted;
        return this;
    }
    public ScreenerRequestBuilder setSize(int size) {
        this.size = Math.max(size, 0); // no negative allowed
        return this;
    }
    public ScreenerRequestBuilder setMaxResults(int maxResults) {
        this.maxResults = Math.max(maxResults, 0); // no negative allowed
        return this;
    }
    public ScreenerRequestBuilder setOffset(int offset) {
        this.offset = Math.max(offset, 0); // no negative allowed
        return this;
    }
    public ScreenerRequestBuilder setSortAscending(ScreenerField sortField) {
        this.sortField = sortField;
        this.isSortDescending = false;
        return this;
    }
    public ScreenerRequestBuilder setSortDescending(ScreenerField sortField) {
        this.sortField = sortField;
        this.isSortDescending = true;
        return this;
    }
    public ScreenerRequestBuilder setUseRecordResponse(Boolean useRecordResponse) {
        this.useRecordResponse = useRecordResponse;
        return this;
    }
    public ScreenerRequestBuilder setTotalOnly(Boolean totalOnly) {
        this.totalOnly = totalOnly;
        return this;
    }

    public ScreenerRequestBuilder eq(ScreenerField field, Long value)
    {
        this.queryBuilder.eq(field, value);
        return this;
    }
    public ScreenerRequestBuilder lt(ScreenerField field, Number value)
    {
        this.queryBuilder.lt(field, value);
        return this;
    }
    public ScreenerRequestBuilder gt(ScreenerField field, Number value)
    {
        this.queryBuilder.gt(field, value);
        return this;
    }
    public ScreenerRequestBuilder btwn(ScreenerField field, Number value1, Number value2)
    {
        this.queryBuilder.between(field, value1, value2);
        return this;
    }
    public ScreenerRequestBuilder in(ScreenerField field, List<String> values)
    {
        // see boolean declarations for info
        this.industryIsSet |= ScreenerField.INDUSTRY.equals((field));
        this.sectorIsSet |= ScreenerField.SECTOR.equals((field));

        this.queryBuilder.in(field, values);
        return this;
    }
    public ScreenerRequestBuilder in(ScreenerField field, String ... values)
    {
        return (values != null ? in(field, Arrays.asList(values)) : this);
    }

    public ScreenerRequestBuilder in(ScreenerField field, CriteriaEnum ... values)
    {
        List<String> criteriaValues = getCriteriaEnumValues(values);
        if (criteriaValues.size() > 0) {
            return in(field, criteriaValues);
        }
        return this;
    }

    private List<String> getCriteriaEnumValues(CriteriaEnum... values) {
        if (values == null || values.length == 0) {
            return Collections.emptyList();
        }

        // depending on context, either needs the NASDAQ or the 3 subtypes
        //   so just handle all scenarios.
        //      need a better spot for this!!!
        Set<CriteriaEnum> criteriaEnums = new LinkedHashSet<>(Arrays.asList(values));
        if (criteriaEnums.contains(Exchange.NASDAQ)) {
            criteriaEnums.addAll(NASDAQ_SUB_TYPES);
        }
        if (criteriaEnums.containsAll(NASDAQ_SUB_TYPES)) {
            criteriaEnums.add(Exchange.NASDAQ);
        }
        return criteriaEnums.stream().map(CriteriaEnum::getCriteriaValue).collect(Collectors.toList());
    }

    @Override
    public YahooEndpoint getEndpoint()
    {
        if (Boolean.TRUE.equals(this.totalOnly)) {
            return YahooEndpoint.SCREENER_TOTALS;
        }
        else if (usePremium) {
            return YahooEndpoint.PREMIUM_SCREENER;
        }
        else {
            return YahooEndpoint.SCREENER;
        }
    }

    public ScreenerRequestBuilder setPremium(boolean premium) {
        this.usePremium = premium;
        return this;
    }

    @Override
    protected String getRequestTicker()
    {
        return "";
    }

    @Override
    protected Map<String, String> buildEndpointParamMap()
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
            List<String> fieldList = QuoteRequestFieldFactory.getQuoteFields(this.quoteType);
            String fieldValueString = String.join(",", fieldList);
            map.put(ParamKeys.FIELDS, fieldValueString);
        }

        return map;
    }

    @Override
    protected Object buildRequestPostBody()
    {
        ScreenerCriteria criteria = new ScreenerCriteria();
        criteria.setSize(size);
        criteria.setOffset(offset);
        criteria.setSortField(sortField.getValue());
        criteria.setIsSortDescending(isSortDescending);
        criteria.setQuoteType(quoteType.toString());
        criteria.setTopOperator(TOP_OPERATOR);
        criteria.setUserId(USER_ID);
        criteria.setUserIdType(USER_ID_TYPE);

        // TODO -- need to confirm if this is desired.
//        String region = this.getRegion();
//        if (!StringUtils.isEmpty(region)) {
//            this.queryBuilder.in(ScreenerField.REGION, Collections.singletonList(region.toLowerCase()));
//        }

        Query query = this.queryBuilder.build();

        criteria.setQuery(query);
        return criteria;
    }

    @Override
    protected ScreenerRequestBuilder getThis()
    {
        return this;
    }


    // this will throw exception if request is invalid
    @Override
    protected void validateRequest(YahooRequest req)
    {
        super.validateRequest(req);

        // to - fix
        //     this validation is misleading b/c can validate other than just the method parameter

        if (sortField == null) {
            throw new IllegalArgumentException("The sortField cannot be null.");
        }
        if (! sortField.isSortable()) {
            throw new IllegalArgumentException(String.format("Cannot sort by '%s'.  It is not a sortable field", sortField));
        }

        if (this.industryIsSet && !this.sectorIsSet) {
            throw new IllegalArgumentException("Must set a 'sector' before can set an 'industry'.");
        }
    }

    @Override
    protected YahooRequest generateRequest(
            YahooEndpoint endpoint, String ticker,
            Map<String, String> paramMap, Object postBody, Map<String,String> headerMap)
    {
        YahooRequest req = super.generateRequest(endpoint, ticker, paramMap, postBody, headerMap);
        if (!Boolean.TRUE.equals(this.totalOnly) && size >= MIN_BATCHABLE_SIZE) {

            // todo - fix -- prob use more builder
            PostBodyBatchUpdater postBodyBatchUpdater = new CriteriaPostBodyBatchUpdater(size);
            req = new YahooBatchRequest(req, null, postBodyBatchUpdater, size, maxResults);
        }
        return req;
    }
}
