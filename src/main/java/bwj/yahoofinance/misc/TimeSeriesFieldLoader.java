package bwj.yahoofinance.misc;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import org.apache.commons.lang.StringUtils;

import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

/**
 * Simple class to read in data stored in json file.
 *  (still tbd what to actually do with it)
 */
public class TimeSeriesFieldLoader
{
    private static final String FILE_NAME = "timeseries_type.json";
    private static final ObjectMapper mapper = new ObjectMapper().enable(SerializationFeature.INDENT_OUTPUT);

    // some possible tickers to get with include:
    //  CAT, NIO, MSFT, O,

    public static void main(String[] args) {
        TimeSeriesFieldLoader timeSeriesFieldLoader = new TimeSeriesFieldLoader();
        Map<String, Boolean> map = timeSeriesFieldLoader.readTimeseriesTypeMap();
        int totalMapSize = map.size();
        List<String> list = timeSeriesFieldLoader.readTimeSeriesTypes();
        int totalListSize = list.size();
    }

    public List<String> readTimeSeriesTypes() {
        Map<String, Boolean> map = readTimeseriesTypeMap();
        return new ArrayList<>(map.keySet());
    }

    /**
     * Reads data from resource json file
     * @return map
     *   KEY: 'type' field name
     *   VALUE: true => appears to be a valid type that returns data
     *          false => is a 'valid' type, but have yet to see it actually return back any useful data. (THUS FAR)
     *   i.e. values with a false __might__ not be useful ...OR... just have used a ticker that shows a value (yet)
     */
    public Map<String,Boolean> readTimeseriesTypeMap()
    {
        String rawJson = readResourceFile(FILE_NAME);
        return convertToMap(rawJson);
    }

    private Map<String, Boolean> convertToMap(String json) {
        if (StringUtils.isEmpty(json)) {
            return Collections.emptyMap();
        }

        try {
            return mapper.readValue(json, new TypeReference<Map<String, Boolean>>() {});
        }
        catch (JsonProcessingException e) {
            throw new RuntimeException("Unable to convert json string to map of maps: " + e.getMessage(), e);
        }
    }

    private String readResourceFile(String fileName)
    {
        try {
            URL resource = getClass().getClassLoader().getResource(fileName);
            return new String ( Files.readAllBytes( Paths.get(resource.getPath()) ) );
        }
        catch (Exception e) {
            throw new RuntimeException(String.format("Unable to read test resource file: %s.  Reason: %s", fileName, e.getMessage()), e);
        }
    }
}
