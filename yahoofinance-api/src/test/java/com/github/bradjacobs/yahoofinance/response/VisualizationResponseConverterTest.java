package com.github.bradjacobs.yahoofinance.response;

import com.github.bradjacobs.yahoofinance.model.EarningsEventResult;
import com.github.bradjacobs.yahoofinance.response.converter.DefaultResponsePojoConverter;
import com.github.bradjacobs.yahoofinance.response.converter.VisualizationResponseConverter;
import com.github.bradjacobs.yahoofinance.util.ResourceUtil;
import org.testng.annotations.Test;

import java.util.List;
import java.util.Map;

//  TODO -- this needs to be fixed and finished
public class VisualizationResponseConverterTest
{
    private static final VisualizationResponseConverter responseConverter = new VisualizationResponseConverter();
    private static final String SAMPLE_RESPONSE_FILE = "earn_event_response.json";


    // todo -- must fix below because would currently make a network call (for crumb value)
//    @Test
//    public void testEarningsRequest() throws Exception
//    {
//        EarningsEventRequestBuilder earningsEventRequestBuilder = new EarningsEventRequestBuilder();
//
//        String dateStart = "2021-11-01";
//        String dateEnd = "2021-11-05";
//
//        earningsEventRequestBuilder.setStart(dateStart);
//        //earningsRequestBuilder.setEnd(dateEnd);
//
//        YahooRequest request = earningsEventRequestBuilder.build();
//        String postBody = request.getPostBody();
//        String prettyPost = PrettyFormatter.prettyJson(postBody);
//        //System.out.println(prettyPost);
//    }

    @Test
    public void testConvertToListOfMaps() throws Exception
    {
        String json = ResourceUtil.readResourceFileAsString(SAMPLE_RESPONSE_FILE);

        List<Map<String, Object>> listOfMaps = responseConverter.convertToListOfMaps(json);
        Map<String, Map<String, Object>> mapOfMaps = responseConverter.convertToMapOfMaps(json);

        DefaultResponsePojoConverter pojoConverter = new DefaultResponsePojoConverter(responseConverter);

        List<EarningsEventResult> listOfPojos = pojoConverter.convertToListOfPojos(json, EarningsEventResult.class);
        Map<String, EarningsEventResult> mapOfPojos = pojoConverter.convertToMapOfPojos(json, EarningsEventResult.class);
    }

}
