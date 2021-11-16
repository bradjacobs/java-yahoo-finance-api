package com.github.bradjacobs.yahoofinance.response;

import com.github.bradjacobs.yahoofinance.model.EarningsEventResult;
import com.github.bradjacobs.yahoofinance.request.YahooFinanceRequest;
import com.github.bradjacobs.yahoofinance.request.builder.EarningsEventRequestBuilder;
import com.github.bradjacobs.yahoofinance.request.builder.ScreenerRequestBuilder;
import com.github.bradjacobs.yahoofinance.response.converter.DefaultResponsePojoConverter;
import com.github.bradjacobs.yahoofinance.response.converter.VisualizationEarningsResponseConverter;
import com.github.bradjacobs.yahoofinance.types.ScreenerField;
import com.github.bradjacobs.yahoofinance.util.PrettyFormatter;
import org.apache.commons.io.FileUtils;
import org.testng.annotations.Test;

import java.io.File;
import java.io.IOException;
import java.nio.charset.Charset;
import java.util.List;
import java.util.Map;

public class VisualizationEarningsResponseConverterTest
{
    private static final VisualizationEarningsResponseConverter responseConverter = new VisualizationEarningsResponseConverter();

    private static final String DIR = "/Users/bradjacobs/git/bradjacobs/java-yahoo-finance-api/yahoofinance-api/src/main/java/com/github/bradjacobs/yahoofinance/tools/internal/generator/types/data/visualization/";

    //private static final String filePath = DIR + "resp1_earn.json";
    //private static final String filePath = DIR + "resp9_earn_past1.json";

    //private static final String filePath = "/Users/bradjacobs/git/bradjacobs/java-yahoo-finance-api/yahoofinance-demo/src/main/java/com/github/bradjacobs/yahoofinance/demo/visualization/resp3_good.json";
    private static final String filePath = "/Users/bradjacobs/git/bradjacobs/java-yahoo-finance-api/yahoofinance-demo/src/main/java/com/github/bradjacobs/yahoofinance/demo/visualization/resp4_good2.json";


    // EarningsRequestBuilder
    @Test
    public void testEarningsRequest() throws Exception
    {
        EarningsEventRequestBuilder earningsEventRequestBuilder = new EarningsEventRequestBuilder();
        ScreenerRequestBuilder screenerRequestBuilder = new ScreenerRequestBuilder();

        screenerRequestBuilder.eq(ScreenerField.SOCIAL_SCORE, 99L);
        screenerRequestBuilder.gt(ScreenerField.CURRENTRATIO, 1.5d);

        YahooFinanceRequest screenerRequest = screenerRequestBuilder.build();
        String screenerBody = screenerRequest.getPostBody();
        String prettyScreenerBody = PrettyFormatter.prettyJson(screenerBody);
        System.out.println(prettyScreenerBody);

        String dateStart = "2021-11-01";
        String dateEnd = "2021-11-05";

        earningsEventRequestBuilder.setStart(dateStart);
//        earningsRequestBuilder.setEnd(dateEnd);

        YahooFinanceRequest request = earningsEventRequestBuilder.build();

        String postBody = request.getPostBody();

        String prettyPost = PrettyFormatter.prettyJson(postBody);
        System.out.println(prettyPost);

        int kjkkj = 333;

    }


    @Test
    public void testConvertToListOfMaps() throws Exception
    {
        String json = readFile();

        List<Map<String, Object>> listOfMaps = responseConverter.convertToListOfMaps(json);
        Map<String, Map<String, Object>> mapOfMaps = responseConverter.convertToMapOfMaps(json);

        DefaultResponsePojoConverter pojoConverter = new DefaultResponsePojoConverter(responseConverter);

        List<EarningsEventResult> listOfPojos = pojoConverter.convertToListOfPojos(json, EarningsEventResult.class);
        Map<String, EarningsEventResult> mapOfPojos = pojoConverter.convertToMapOfPojos(json, EarningsEventResult.class);


        int kjkjk = 333;

    }

    private String readFile()
    {
        try {
            return FileUtils.readFileToString(new File(filePath), Charset.defaultCharset());
        } catch (IOException e) {
            throw new RuntimeException("Cant read file!");
        }
    }

}
