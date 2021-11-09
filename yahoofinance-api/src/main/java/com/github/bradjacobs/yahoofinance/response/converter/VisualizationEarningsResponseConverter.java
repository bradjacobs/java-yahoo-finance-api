package com.github.bradjacobs.yahoofinance.response.converter;

import com.fasterxml.jackson.databind.JavaType;
import com.fasterxml.jackson.databind.json.JsonMapper;
import com.github.bradjacobs.yahoofinance.converter.datetime.EpochStrConverter;
import com.github.bradjacobs.yahoofinance.converter.datetime.MetaEpochSecondsConverter;
import com.github.bradjacobs.yahoofinance.model.beta.EarningsVisualizationResult;
import com.github.bradjacobs.yahoofinance.response.ResponseConverterConfig;
import com.github.bradjacobs.yahoofinance.response.converter.util.SimpleMapOfMapsGenerator;
import com.github.bradjacobs.yahoofinance.util.JsonMapperFactory;
import com.github.bradjacobs.yahoofinance.util.JsonPathDocContextCreator;
import com.jayway.jsonpath.Configuration;
import com.jayway.jsonpath.DocumentContext;
import com.jayway.jsonpath.JsonPath;
import com.jayway.jsonpath.Option;
import org.apache.commons.lang3.ArrayUtils;

import java.util.*;

public class VisualizationEarningsResponseConverter implements ResponseConverter
{
    private static final String DOCUMENTS_PATH = "$.finance.result[0].documents[0]";
    private static final String COLUMN_NAMES_PATH = DOCUMENTS_PATH + ".columns[*].id";
    private static final String DATA_PATH = DOCUMENTS_PATH + ".rows[*]";

    private static final String MAP_KEY = "ticker";

    private static final JsonPathDocContextCreator JSON_PATH_DOC_CONTEXT_CREATOR = new JsonPathDocContextCreator();
    private final SimpleMapOfMapsGenerator mapOfMapsGenerator = new SimpleMapOfMapsGenerator(MAP_KEY, true);

    @Override
    public Map<String, Map<String, Object>> convertToMapOfMaps(String json)
    {
        return mapOfMapsGenerator.convertToMap( this.convertToListOfMaps(json) );
    }

    @Override
    public List<Map<String, Object>> convertToListOfMaps(String json)
    {
        List<Map<String,Object>> resultListOfMaps = new ArrayList<>();

        DocumentContext jsonDoc = JSON_PATH_DOC_CONTEXT_CREATOR.createDocContext(json);
        String[] columnNames = jsonDoc.read(COLUMN_NAMES_PATH, String[].class);
        int columnCount = columnNames.length;

        List<List<Object>>  listOfEntryResults = jsonDoc.read(DATA_PATH);

        for (List<Object> elementValueList : listOfEntryResults) {

            Map<String,Object> elementEntryMap = new HashMap<>();
            for (int i = 0; i < columnCount; i++) {
                elementEntryMap.put(columnNames[i], elementValueList.get(i));
            }
            resultListOfMaps.add(elementEntryMap);
        }

        return resultListOfMaps;
    }
}
