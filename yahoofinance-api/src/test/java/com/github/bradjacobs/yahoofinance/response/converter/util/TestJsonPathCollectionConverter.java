package com.github.bradjacobs.yahoofinance.response.converter.util;

import com.github.bradjacobs.yahoofinance.response.converter.AbstractJsonPathCollectionConverter;
import com.jayway.jsonpath.DocumentContext;
import org.apache.commons.lang3.StringUtils;

import java.util.Collections;
import java.util.List;
import java.util.Map;

// todo - class needs better name & location
public class TestJsonPathCollectionConverter extends AbstractJsonPathCollectionConverter
{

    public List<Object> extractListOfObjects(String json, String path) {

        DocumentContext jsonDoc = createJsonPathDocContext(json);
        if (jsonDoc == null) {
            return Collections.emptyList();
        }
        return jsonDoc.read(path);
    }

}
