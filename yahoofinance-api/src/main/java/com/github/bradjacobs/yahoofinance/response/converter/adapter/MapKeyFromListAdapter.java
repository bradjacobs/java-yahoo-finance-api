package com.github.bradjacobs.yahoofinance.response.converter.adapter;

import com.github.bradjacobs.yahoofinance.response.converter.ResponseConverter;
import com.github.bradjacobs.yahoofinance.response.converter.util.SimpleMapOfMapsGenerator;

import java.util.List;
import java.util.Map;


/**
 * Using to construct a "map of maps" with a given listOfMaps plus the name of attribute to be used as the map key.
 */
public class MapKeyFromListAdapter implements ResponseConverter
{
    private final ResponseConverter targetConverter;
    private final SimpleMapOfMapsGenerator simpleMapOfMapsGenerator;

    public MapKeyFromListAdapter(ResponseConverter targetResponseConverter, String keyName, boolean sortMapKeys) {
        if (targetResponseConverter == null) {
            throw new IllegalArgumentException("Must provide a target response converter.");
        }
        this.targetConverter = targetResponseConverter;
        this.simpleMapOfMapsGenerator = new SimpleMapOfMapsGenerator(keyName, sortMapKeys);
    }

    @Override
    public List<Map<String, Object>> convertToListOfMaps(String json) {
        return targetConverter.convertToListOfMaps(json);
    }

    @Override
    public Map<String, Map<String, Object>> convertToMapOfMaps(String json) {
        return this.simpleMapOfMapsGenerator.convertToMap( this.convertToListOfMaps(json) );
    }
}
