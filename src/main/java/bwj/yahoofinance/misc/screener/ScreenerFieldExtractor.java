package bwj.yahoofinance.misc.screener;

import bwj.yahoofinance.misc.screener.autogen.Category;
import bwj.yahoofinance.misc.screener.autogen.ScreenerField;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;

import java.io.IOException;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Scanner;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.stream.Collectors;

public class ScreenerFieldExtractor
{
    // note: url copied from web (haven't analyzed what all the params mean)
    private static final String FEILD_DEFN_QUERY = "https://query1.finance.yahoo.com/v1/finance/screener/instrument/equity/fields?lang=en-US&region=US&category=keystats%2Cfinancials%2Cvaluation%2Csector_industry%2Cesgscores%2Cincome%2Ccashflowstatement%2Cbalance_sheet%2Cearnings%2Cdividends_and_splits%2Cprofile%2Cfair_value%2Cpopular_filters%2Cchanges_in_price_and_market_cap%2Cchanges_in_volume_and_ownership%2Cvaluation_metric%2Cprofitability_ratios_and_dividends%2Cdebt_ratios%2Cliquidity_ratios%2Ceps_and_income_statement%2Ccash_flow%2Cesg_scores%2Cshort_interest%2Cfair_value&corsDomain=finance.yahoo.com";

    public static void main(String[] args) throws Exception
    {
        ScreenerFieldExtractor sfe = new ScreenerFieldExtractor();
        sfe.contructFieldDefnitions();
    }

    public void contructFieldDefnitions() throws Exception
    {
        String json = urlRead(FEILD_DEFN_QUERY);

        ObjectMapper mapper = new ObjectMapper().enable(SerializationFeature.INDENT_OUTPUT);
        JsonNode root = mapper.readTree(json);

        JsonNode innerNode = root.required("finance").required("result").required(0).required("fields");

        // NOTE: i'm sure there's a simpler way to extract the data
        String innerJsonString = innerNode.toString();

        // convert to map of map objects
        Map<String, Map<String, Object>> mapOfMaps = mapper.readValue(innerJsonString, new TypeReference<Map<String, Map<String, Object>>>() {});
        // the key name is the same as the fieldId, thus we only need all the "values" from the collection.
        List<Map<String, Object>> listOfMaps = new ArrayList<>(mapOfMaps.values());

        ScreenerField[] fields = mapper.convertValue(listOfMaps, ScreenerField[].class);


        // filter out fields want to ignore.
        List<ScreenerField> filteredList = Arrays.stream(fields)
                .filter(sf -> !sf.getDeprecated())
                .filter(sf -> !sf.getIsPremium())
                .collect(Collectors.toList());

        Map<Category,Set<ScreenerField>> categoryFieldMap = new TreeMap<>(); // treemap only to keep key order consistent

        for (ScreenerField field : filteredList) {
            Category category = field.getCategory();
            Set<ScreenerField> categoryFields = categoryFieldMap.computeIfAbsent(category, k -> new TreeSet<>());
            categoryFields.add(field);
        }


        System.out.println("Total Fields: " + fields.length);
    }

    private static String urlRead(String url) throws IOException {
        URL urlObj = new URL(url);
        try (Scanner scanner = new Scanner(urlObj.openStream(), String.valueOf(StandardCharsets.UTF_8))) {
            return scanner.useDelimiter("\\A").next();
        }
        catch (Exception e) {
            throw new RuntimeException("Unable to read URL: " + url);
        }
    }
}
