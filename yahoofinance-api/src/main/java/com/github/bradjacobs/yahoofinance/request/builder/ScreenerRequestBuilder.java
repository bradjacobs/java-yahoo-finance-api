package com.github.bradjacobs.yahoofinance.request.builder;

import com.github.bradjacobs.yahoofinance.request.YahooRequest;
import com.github.bradjacobs.yahoofinance.request.batch.CriteriaPostBodyBatchUpdater;
import com.github.bradjacobs.yahoofinance.request.batch.PostBodyBatchUpdater;
import com.github.bradjacobs.yahoofinance.request.batch.YahooBatchRequest;
import com.github.bradjacobs.yahoofinance.request.builder.helper.QueryBuilder;
import com.github.bradjacobs.yahoofinance.types.Exchange;
import com.github.bradjacobs.yahoofinance.types.Industry;
import com.github.bradjacobs.yahoofinance.types.ScreenerField;
import com.github.bradjacobs.yahoofinance.types.Sector;
import com.github.bradjacobs.yahoofinance.types.Type;
import com.github.bradjacobs.yahoofinance.types.YahooEndpoint;
import com.github.bradjacobs.yahoofinance.types.criteria.CriteriaEnum;
import com.github.bradjacobs.yahoofinance.types.criteria.Operator;
import com.github.bradjacobs.yahoofinance.types.criteria.Query;
import com.github.bradjacobs.yahoofinance.types.criteria.ScreenerCriteria;
import org.apache.commons.lang3.StringUtils;

import java.util.Arrays;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

// todo - fix bug if offset value is manually set
public class ScreenerRequestBuilder extends BaseRequestBuilder<ScreenerRequestBuilder>
{
    private static final int MAX_BATCH_SIZE = 250; // note: going bigger than 250 can result in a yahoo error saying the value is 'too big'
    private static final Set<Exchange> NASDAQ_SUB_TYPES =
            new LinkedHashSet<>(Arrays.asList(Exchange.NASDAQGM, Exchange.NASDAQGS, Exchange.NASDAQCM));

    private boolean usePremium = false;

    // __NOTE__: all variables are set to DEFAULT value
    private int offset = 0;
    private int maxResults = MAX_BATCH_SIZE * 2;

    private ScreenerField sortField = ScreenerField.INTRADAYMARKETCAP;
    private boolean isSortDescending = true;
    private Boolean formatted = null;

    private Boolean useRecordResponse = null;  // not sure what this actually does
    private boolean totalOnly = false; // return record count only

    // these remain const until there's need otherwise.
    //        side note:  it's possible to use 'entityIdType' instead of a quoteType, but is untested/unsupported for now
    private final Type quoteType = Type.EQUITY;
    private static final String TOP_OPERATOR = Operator.AND.getValue().toUpperCase();  // make uppercase only b/c the website does it.
    private static final String USER_ID = "";
    private static final String USER_ID_TYPE = "guid";


    private final QueryBuilder queryBuilder = new QueryBuilder();


    public ScreenerRequestBuilder()
    {
    }

    public ScreenerRequestBuilder setFormatted(Boolean formatted) {
        this.formatted = formatted;
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
    public ScreenerRequestBuilder setTotalOnly(boolean totalOnly) {
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

    // todo - candidate for removal
//    public ScreenerRequestBuilder setRegions(Region... regions) {
//        return in(ScreenerField.REGION, regions);
//    }

    public ScreenerRequestBuilder setExchanges(Exchange... exchanges) {
        return in(ScreenerField.EXCHANGE, exchanges);
    }
    public ScreenerRequestBuilder setSectors(Sector... sectors) {
        return in(ScreenerField.SECTOR, sectors);
    }
    public ScreenerRequestBuilder setIndustries(Industry... industries) {
        if (industries != null && industries.length > 0) {
            Set<Sector> sectorSet = new LinkedHashSet<>();
            for (Industry industry : industries) {
                sectorSet.add(industry.getSector());
            }
            setSectors(sectorSet.toArray(new Sector[0]));
        }
        return in(ScreenerField.INDUSTRY, industries);
    }

    private ScreenerRequestBuilder in(ScreenerField field, CriteriaEnum ... values)
    {
        List<String> criteriaValues = getCriteriaEnumValues(values);
        if (criteriaValues.size() > 0) {
            return in(field, criteriaValues);
        }
        return this;
    }

    private ScreenerRequestBuilder in(ScreenerField field, List<String> values)
    {
        this.queryBuilder.in(field, values);
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
        if (this.totalOnly) {
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

        if (!this.totalOnly)
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
        criteria.setSize(calculateRequestBatchSize());
        criteria.setOffset(offset);
        criteria.setSortField(sortField.getValue());
        criteria.setIsSortDescending(isSortDescending);
        criteria.setQuoteType(quoteType.toString());
        criteria.setTopOperator(TOP_OPERATOR);
        criteria.setUserId(USER_ID);
        criteria.setUserIdType(USER_ID_TYPE);

        String region = this.getRegion();
        if (!StringUtils.isEmpty(region)) {
            // note: must be lowercase (or else will return 0 results)
            this.queryBuilder.in(ScreenerField.REGION, Collections.singletonList(region.toLowerCase()));
        }

        Query query = this.queryBuilder.build();

        criteria.setQuery(query);
        return criteria;
    }

    private int calculateRequestBatchSize() {
        return Math.min(MAX_BATCH_SIZE, this.maxResults);
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
    }

    @Override
    protected YahooRequest generateRequest(
            YahooEndpoint endpoint, String ticker,
            Map<String, String> paramMap, Object postBody, Map<String,String> headerMap)
    {
        YahooRequest req = super.generateRequest(endpoint, ticker, paramMap, postBody, headerMap);
        int batchSize = calculateRequestBatchSize();

        if (!this.totalOnly && maxResults > batchSize) {
            // todo - fix -- prob use more builder
            PostBodyBatchUpdater postBodyBatchUpdater = new CriteriaPostBodyBatchUpdater(batchSize);
            req = new YahooBatchRequest(req, null, postBodyBatchUpdater, batchSize, maxResults);
        }
        return req;
    }
}
