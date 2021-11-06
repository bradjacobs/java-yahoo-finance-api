package com.github.bradjacobs.yahoofinance.response.converter;

import com.fasterxml.jackson.databind.JavaType;
import com.fasterxml.jackson.databind.json.JsonMapper;
import com.github.bradjacobs.yahoofinance.converter.datetime.EpochStrConverter;
import com.github.bradjacobs.yahoofinance.converter.datetime.MetaEpochSecondsConverter;
import com.github.bradjacobs.yahoofinance.model.beta.EarningsVisualizationResult;
import com.github.bradjacobs.yahoofinance.response.ResponseConverterConfig;
import com.github.bradjacobs.yahoofinance.response.converter.util.SimpleMapOfMapsGenerator;
import com.github.bradjacobs.yahoofinance.util.JsonMapperFactory;
import com.jayway.jsonpath.Configuration;
import com.jayway.jsonpath.DocumentContext;
import com.jayway.jsonpath.JsonPath;
import com.jayway.jsonpath.Option;
import org.apache.commons.lang3.ArrayUtils;

import java.util.*;

public class VisualizationEarningsResponseConverter implements ResponseConverter
{

    // path locations for data within the JSON response
    private static final String BASE_PATH      = "$.chart.result[0]";

    private static final String COLUMN_NAMES_PATH = "$.finance.result[0].documents[0].columns[*].id";
    private static final String DATA_PATH = "$.finance.result[0].documents[0].rows[*]";



    // configure to return NULL (instead of Exception) if the _LEAF_ is missing
    //   b/c the field might not always be there.
    private static final Configuration JSON_PATH_CONFIG =
        Configuration.defaultConfiguration()
            .addOptions(Option.DEFAULT_PATH_LEAF_TO_NULL)
            .addOptions(Option.SUPPRESS_EXCEPTIONS);




    public VisualizationEarningsResponseConverter()
    {
    }


    @Override
    public Map<String, Map<String, Object>> convertToMapOfMaps(String json)
    {
        return null;
    }

    @Override
    public List<Map<String, Object>> convertToListOfMaps(String json)
    {
        // todo - using own special way of creating docContext.
        //     tbd if even worrying about this...(to ponder)
        DocumentContext jsonDoc = JsonPath.using(JSON_PATH_CONFIG).parse(json);

        String[] columnNames = jsonDoc.read(COLUMN_NAMES_PATH, String[].class);
        int columnCount = columnNames.length;

        Object obj = jsonDoc.read(DATA_PATH, Object.class);  // works
        //Object[] dataObjects = jsonDoc.read(DATA_PATH, Object[].class);  // doesn't work
        List<List<Object>>  myList = jsonDoc.read(DATA_PATH);

        List<Map<String,Object>> listOfMaps = new ArrayList<>();

        for (List<Object> elementValueList : myList) {

            Map<String,Object> elementEntryMap = new HashMap<>();
            for (int i = 0; i < columnCount; i++) {
                elementEntryMap.put(columnNames[i], elementValueList.get(i));
            }
            listOfMaps.add(elementEntryMap);

        }

        DefaultResponseConverter temp = new DefaultResponseConverter();
        DefaultResponsePojoConverter defaultResponsePojoConverter = new DefaultResponsePojoConverter(temp);

        JsonMapper mapper = JsonMapperFactory.getMapper();

        JavaType javaType = mapper.getTypeFactory().constructParametricType(List.class, EarningsVisualizationResult.class);
        List<EarningsVisualizationResult> pojoList = mapper.convertValue(listOfMaps, javaType);

        String ticker = "ANET";

        for (EarningsVisualizationResult pojo : pojoList) {
            if (ticker.equals(pojo.getTicker()))
            {
                int kjkjj = 333;
            }
        }

        Object first = myList.get(0);
        List<Object> firstB = myList.get(0);

        Object firstElement = firstB.get(0);

        return null;
    }
}
