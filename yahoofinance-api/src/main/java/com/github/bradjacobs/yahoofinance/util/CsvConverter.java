package com.github.bradjacobs.yahoofinance.util;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.MappingIterator;
import com.fasterxml.jackson.databind.ObjectReader;
import com.fasterxml.jackson.dataformat.csv.CsvMapper;
import com.fasterxml.jackson.dataformat.csv.CsvSchema;
import org.apache.commons.lang3.StringUtils;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

public class CsvConverter {

    private static final CsvMapper csvMapper = new CsvMapper();

    private CsvConverter() {}

    public static String convertToCsv(List<Map<String, Object>> listOfMaps)
    {
        if (listOfMaps == null || listOfMaps.isEmpty()) {
            return "";
        }

        String json = JsonConverter.toJson(listOfMaps);
        JsonNode jsonTree = JsonConverter.convertToNode(json);

        CsvSchema.Builder csvSchemaBuilder = CsvSchema.builder();
        List<String> firstRecordKeys = new ArrayList<>(listOfMaps.get(0).keySet());
        for (String firstRecordKey : firstRecordKeys) {
            csvSchemaBuilder.addColumn(firstRecordKey);
        }

        CsvSchema csvSchema = csvSchemaBuilder.build().withHeader();
        try {
            return csvMapper
                    .writerFor(JsonNode.class)
                    .with(csvSchema)
                    .writeValueAsString(jsonTree);
        }
        catch (JsonProcessingException e) {
            throw new IllegalArgumentException(String.format("Unable to convert node to csv string: " + e.getMessage(), e));
        }
    }

    // proof of concept
    public static <T> List<T> convertToPojos(List<Map<String, Object>> listOfMaps, Class<T> targetType) {
        String csvString = convertToCsv(listOfMaps);
        if (StringUtils.isEmpty(csvString)) {
            return Collections.emptyList();
        }
        return convertToPojos(csvString, targetType);
    }

    public static <T> List<T> convertToPojos(String csvString, Class<T> targetType) {
        CsvSchema schema = CsvSchema.emptySchema().withHeader();
        try {
            ObjectReader objReader = csvMapper.readerFor(targetType).with(schema);
            MappingIterator<T> iterator = objReader.readValues(csvString);
            return iterator.readAll();
        }
        catch (IOException e) {
            throw new UncheckedIOException("Unable to create pojos from csv: " + e.getMessage(), e);
        }
    }
}

