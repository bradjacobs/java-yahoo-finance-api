package com.github.bradjacobs.yahoofinance.util;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.json.JsonMapper;
import org.testng.annotations.Test;

import java.io.File;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

public class JsonMapperSingletonTest
{

    /*
       https://stackoverflow.com/questions/37722152/set-jackson-objectmapper-class-not-to-use-scientific-notation-for-double

    While converting a BigDecimal I have faced below is working
        mapper = mapper.setNodeFactory(JsonNodeFactory.withExactBigDecimals(true));
    while this is not working for me
        mapper.configure(JsonGenerator.Feature.WRITE_BIGDECIMAL_AS_PLAIN, true);

     */

    @Test
    public void testFoo() throws Exception
    {
        JsonMapper mapper = JsonMapperSingleton.getPrettyInstance();

        File file = new File("/Users/bjacob101/git/github.com/bradjacobs/sec-data-loader/src/test/resources/aapl_timeseries_respoinse.json");

        Map<String,Object> map = mapper.readValue(file, new TypeReference<Map<String,Object>>() {});

        Map<String,Object> resultMap = (Map<String,Object>)map.get("timeseries");
        List<Map<String,Object>> entryList = (List<Map<String, Object>>) resultMap.get("result");
        Map<String,Object> firstEntryMap = entryList.get(0);
        List<Map<String,Object>> attributeEntryList = (List<Map<String, Object>>) firstEntryMap.get("annualNetIncomeContinuousOperations");
        Map<String,Object> firstAttributeEntryMap = attributeEntryList.get(0);
        Map<String,Object> reportedValueMap = (Map<String, Object>) firstAttributeEntryMap.get("reportedValue");
        Object rawValue = reportedValueMap.get("raw");

        System.out.println("RAW VALUE CLASS: " + rawValue.getClass());

        Map<String,Object> tempMap = new LinkedHashMap<>();
        tempMap.put("key", rawValue);

        String json = mapper.writeValueAsString(tempMap);


       // Object inner = map.get("quoteSummary");

        int kjkj = 333333;

    }

    @Test
    public void testWrite() throws Exception
    {
//        JsonMapper mapper = JsonMapperSingleton.getPrettyInstance();
        JsonMapper mapper = JsonMapperSingleton.getInstance();

        Map<String,Number> map = new LinkedHashMap<>();

//        Long bigLong = 134744997889L;
//        Double bigDouble = bigLong + 0.22d;

        Long bigLong = 134700000000L;
        Double bigDouble = bigLong.doubleValue();

        Double doubleValueA = 0.11d;
        Double doubleValueB = doubleValueA * 4.1;

        //    0.11d   *   4.1
        //  {"small_num":33,"big_long":134700000000,"big_double":1.347E11,"dbl_A":0.11,"dbl_B":0.45099999999999996}

        map.put("small_num", 33);
        map.put("big_long", bigLong);
        map.put("big_double", bigDouble);
        map.put("dbl_A", doubleValueA);
        map.put("dbl_B", doubleValueB);

        String json = mapper.writeValueAsString(map);

        String jsonStr = "{\"small_num\":33,\"big_long\":134744997889,\"big_double\":1.3474E11,\"dbl_A\":0.1122,\"dbl_B\":0.4488}";

        Map<String,Object> readMap = mapper.readValue(TEST_JSON2, new TypeReference<Map<String,Object>>() {});
        Map<String,Object> readMap2 = mapper.readValue(json, new TypeReference<Map<String,Object>>() {});
        Map<String,Object> readMap3 = mapper.readValue(jsonStr, new TypeReference<Map<String,Object>>() {});


        int kjkj = 333333;

    }



    private static final String TEST_JSON = "{\n" +
        "  \"quoteSummary\": {\n" +
        "    \"result\": [\n" +
        "      {\n" +
        "        \"financialData\": {\n" +
        "          \"maxAge\": 86400,\n" +
        "          \"currentPrice\": {\n" +
        "            \"raw\": 139.96,\n" +
        "            \"fmt\": \"139.96\"\n" +
        "          },\n" +
        "          \"recommendationKey\": \"buy\",\n" +
        "          \"numberOfAnalystOpinions\": {\n" +
        "            \"raw\": 39,\n" +
        "            \"fmt\": \"39\",\n" +
        "            \"longFmt\": \"39\"\n" +
        "          },\n" +
        "          \"totalCash\": {\n" +
        "            \"raw\": 69833998336,\n" +
        "            \"fmt\": \"69.83B\",\n" +
        "            \"longFmt\": \"69,833,998,336\"\n" +
        "          },\n" +
        "          \"ebitda\": {\n" +
        "            \"raw\": 99820003328,\n" +
        "            \"fmt\": \"99.82B\",\n" +
        "            \"longFmt\": \"99,820,003,328\"\n" +
        "          },\n" +
        "          \"totalDebt\": {\n" +
        "            \"raw\": 134744997888,\n" +
        "            \"fmt\": \"134.74B\",\n" +
        "            \"longFmt\": \"134,744,997,888\"\n" +
        "          },\n" +
        "          \"currentRatio\": {\n" +
        "            \"raw\": 1.142,\n" +
        "            \"fmt\": \"1.14\"\n" +
        "          },\n" +
        "          \"totalRevenue\": {\n" +
        "            \"raw\": 325405999104,\n" +
        "            \"fmt\": \"325.41B\",\n" +
        "            \"longFmt\": \"325,405,999,104\"\n" +
        "          },\n" +
        "          \"ebitdaMargins\": {\n" +
        "            \"raw\": 0.30675,\n" +
        "            \"fmt\": \"30.67%\"\n" +
        "          },\n" +
        "          \"financialCurrency\": \"USD\"\n" +
        "        }\n" +
        "      }\n" +
        "    ],\n" +
        "    \"error\": null\n" +
        "  }\n" +
        "}";

    private static final String TEST_JSON2= "{\"quoteSummary\":{\"result\":[{\"incomeStatementHistory\":{\"incomeStatementHistory\":[{\"maxAge\":1," +
        "\"endDate\":{\"raw\":1601078400,\"fmt\":\"2020-09-26\"},\"totalRevenue\":{\"raw\":274515000000,\"fmt\":\"274.51B\",\"longFmt\":\"274,515,000,000\"}," +
        "\"costOfRevenue\":{\"raw\":169559000000,\"fmt\":\"169.56B\",\"longFmt\":\"169,559,000,000\"},\"grossProfit\":{\"raw\":104956000000,\"fmt\":\"104.96B\"," +
        "\"longFmt\":\"104,956,000,000\"},\"researchDevelopment\":{\"raw\":18752000000,\"fmt\":\"18.75B\",\"longFmt\":\"18,752,000,000\"}," +
        "\"sellingGeneralAdministrative\":{\"raw\":19916000000,\"fmt\":\"19.92B\",\"longFmt\":\"19,916,000,000\"},\"nonRecurring\":{},\"otherOperatingExpenses\":{}," +
        "\"totalOperatingExpenses\":{\"raw\":208227000000,\"fmt\":\"208.23B\",\"longFmt\":\"208,227,000,000\"},\"operatingIncome\":{\"raw\":66288000000,\"fmt\":\"66.29B\"," +
        "\"longFmt\":\"66,288,000,000\"},\"totalOtherIncomeExpenseNet\":{\"raw\":803000000,\"fmt\":\"803M\",\"longFmt\":\"803,000,000\"},\"ebit\":{\"raw\":66288000000," +
        "\"fmt\":\"66.29B\",\"longFmt\":\"66,288,000,000\"},\"interestExpense\":{\"raw\":-2873000000,\"fmt\":\"-2.87B\",\"longFmt\":\"-2,873,000,000\"}," +
        "\"incomeBeforeTax\":{\"raw\":67091000000,\"fmt\":\"67.09B\",\"longFmt\":\"67,091,000,000\"},\"incomeTaxExpense\":{\"raw\":9680000000,\"fmt\":\"9.68B\",\"longFmt\":\"9," +
        "680,000,000\"},\"minorityInterest\":{},\"netIncomeFromContinuingOps\":{\"raw\":57411000000,\"fmt\":\"57.41B\",\"longFmt\":\"57,411,000,000\"}," +
        "\"discontinuedOperations\":{},\"extraordinaryItems\":{},\"effectOfAccountingCharges\":{},\"otherItems\":{},\"netIncome\":{\"raw\":57411000000,\"fmt\":\"57.41B\"," +
        "\"longFmt\":\"57,411,000,000\"},\"netIncomeApplicableToCommonShares\":{\"raw\":57411000000,\"fmt\":\"57.41B\",\"longFmt\":\"57,411,000,000\"}},{\"maxAge\":1," +
        "\"endDate\":{\"raw\":1569628800,\"fmt\":\"2019-09-28\"},\"totalRevenue\":{\"raw\":260174000000,\"fmt\":\"260.17B\",\"longFmt\":\"260,174,000,000\"}," +
        "\"costOfRevenue\":{\"raw\":161782000000,\"fmt\":\"161.78B\",\"longFmt\":\"161,782,000,000\"},\"grossProfit\":{\"raw\":98392000000,\"fmt\":\"98.39B\",\"longFmt\":\"98," +
        "392,000,000\"},\"researchDevelopment\":{\"raw\":16217000000,\"fmt\":\"16.22B\",\"longFmt\":\"16,217,000,000\"},\"sellingGeneralAdministrative\":{\"raw\":18245000000," +
        "\"fmt\":\"18.25B\",\"longFmt\":\"18,245,000,000\"},\"nonRecurring\":{},\"otherOperatingExpenses\":{},\"totalOperatingExpenses\":{\"raw\":196244000000,\"fmt\":\"196" +
        ".24B\",\"longFmt\":\"196,244,000,000\"},\"operatingIncome\":{\"raw\":63930000000,\"fmt\":\"63.93B\",\"longFmt\":\"63,930,000,000\"}," +
        "\"totalOtherIncomeExpenseNet\":{\"raw\":1807000000,\"fmt\":\"1.81B\",\"longFmt\":\"1,807,000,000\"},\"ebit\":{\"raw\":63930000000,\"fmt\":\"63.93B\",\"longFmt\":\"63," +
        "930,000,000\"},\"interestExpense\":{\"raw\":-3576000000,\"fmt\":\"-3.58B\",\"longFmt\":\"-3,576,000,000\"},\"incomeBeforeTax\":{\"raw\":65737000000,\"fmt\":\"65.74B\"," +
        "\"longFmt\":\"65,737,000,000\"},\"incomeTaxExpense\":{\"raw\":10481000000,\"fmt\":\"10.48B\",\"longFmt\":\"10,481,000,000\"},\"minorityInterest\":{}," +
        "\"netIncomeFromContinuingOps\":{\"raw\":55256000000,\"fmt\":\"55.26B\",\"longFmt\":\"55,256,000,000\"},\"discontinuedOperations\":{},\"extraordinaryItems\":{}," +
        "\"effectOfAccountingCharges\":{},\"otherItems\":{},\"netIncome\":{\"raw\":55256000000,\"fmt\":\"55.26B\",\"longFmt\":\"55,256,000,000\"}," +
        "\"netIncomeApplicableToCommonShares\":{\"raw\":55256000000,\"fmt\":\"55.26B\",\"longFmt\":\"55,256,000,000\"}},{\"maxAge\":1,\"endDate\":{\"raw\":1538179200," +
        "\"fmt\":\"2018-09-29\"},\"totalRevenue\":{\"raw\":265595000000,\"fmt\":\"265.6B\",\"longFmt\":\"265,595,000,000\"},\"costOfRevenue\":{\"raw\":163756000000,\"fmt\":\"163" +
        ".76B\",\"longFmt\":\"163,756,000,000\"},\"grossProfit\":{\"raw\":101839000000,\"fmt\":\"101.84B\",\"longFmt\":\"101,839,000,000\"}," +
        "\"researchDevelopment\":{\"raw\":14236000000,\"fmt\":\"14.24B\",\"longFmt\":\"14,236,000,000\"},\"sellingGeneralAdministrative\":{\"raw\":16705000000,\"fmt\":\"16.7B\"," +
        "\"longFmt\":\"16,705,000,000\"},\"nonRecurring\":{},\"otherOperatingExpenses\":{},\"totalOperatingExpenses\":{\"raw\":194697000000,\"fmt\":\"194.7B\",\"longFmt\":\"194," +
        "697,000,000\"},\"operatingIncome\":{\"raw\":70898000000,\"fmt\":\"70.9B\",\"longFmt\":\"70,898,000,000\"},\"totalOtherIncomeExpenseNet\":{\"raw\":2005000000," +
        "\"fmt\":\"2B\",\"longFmt\":\"2,005,000,000\"},\"ebit\":{\"raw\":70898000000,\"fmt\":\"70.9B\",\"longFmt\":\"70,898,000,000\"},\"interestExpense\":{\"raw\":-3240000000," +
        "\"fmt\":\"-3.24B\",\"longFmt\":\"-3,240,000,000\"},\"incomeBeforeTax\":{\"raw\":72903000000,\"fmt\":\"72.9B\",\"longFmt\":\"72,903,000,000\"}," +
        "\"incomeTaxExpense\":{\"raw\":13372000000,\"fmt\":\"13.37B\",\"longFmt\":\"13,372,000,000\"},\"minorityInterest\":{}," +
        "\"netIncomeFromContinuingOps\":{\"raw\":59531000000,\"fmt\":\"59.53B\",\"longFmt\":\"59,531,000,000\"},\"discontinuedOperations\":{},\"extraordinaryItems\":{}," +
        "\"effectOfAccountingCharges\":{},\"otherItems\":{},\"netIncome\":{\"raw\":59531000000,\"fmt\":\"59.53B\",\"longFmt\":\"59,531,000,000\"}," +
        "\"netIncomeApplicableToCommonShares\":{\"raw\":59531000000,\"fmt\":\"59.53B\",\"longFmt\":\"59,531,000,000\"}},{\"maxAge\":1,\"endDate\":{\"raw\":1506729600," +
        "\"fmt\":\"2017-09-30\"},\"totalRevenue\":{\"raw\":229234000000,\"fmt\":\"229.23B\",\"longFmt\":\"229,234,000,000\"},\"costOfRevenue\":{\"raw\":141048000000," +
        "\"fmt\":\"141.05B\",\"longFmt\":\"141,048,000,000\"},\"grossProfit\":{\"raw\":88186000000,\"fmt\":\"88.19B\",\"longFmt\":\"88,186,000,000\"}," +
        "\"researchDevelopment\":{\"raw\":11581000000,\"fmt\":\"11.58B\",\"longFmt\":\"11,581,000,000\"},\"sellingGeneralAdministrative\":{\"raw\":15261000000,\"fmt\":\"15" +
        ".26B\",\"longFmt\":\"15,261,000,000\"},\"nonRecurring\":{},\"otherOperatingExpenses\":{},\"totalOperatingExpenses\":{\"raw\":167890000000,\"fmt\":\"167.89B\"," +
        "\"longFmt\":\"167,890,000,000\"},\"operatingIncome\":{\"raw\":61344000000,\"fmt\":\"61.34B\",\"longFmt\":\"61,344,000,000\"}," +
        "\"totalOtherIncomeExpenseNet\":{\"raw\":2745000000,\"fmt\":\"2.75B\",\"longFmt\":\"2,745,000,000\"},\"ebit\":{\"raw\":61344000000,\"fmt\":\"61.34B\",\"longFmt\":\"61," +
        "344,000,000\"},\"interestExpense\":{\"raw\":-2323000000,\"fmt\":\"-2.32B\",\"longFmt\":\"-2,323,000,000\"},\"incomeBeforeTax\":{\"raw\":64089000000,\"fmt\":\"64.09B\"," +
        "\"longFmt\":\"64,089,000,000\"},\"incomeTaxExpense\":{\"raw\":15738000000,\"fmt\":\"15.74B\",\"longFmt\":\"15,738,000,000\"},\"minorityInterest\":{}," +
        "\"netIncomeFromContinuingOps\":{\"raw\":48351000000,\"fmt\":\"48.35B\",\"longFmt\":\"48,351,000,000\"},\"discontinuedOperations\":{},\"extraordinaryItems\":{}," +
        "\"effectOfAccountingCharges\":{},\"otherItems\":{},\"netIncome\":{\"raw\":48351000000,\"fmt\":\"48.35B\",\"longFmt\":\"48,351,000,000\"}," +
        "\"netIncomeApplicableToCommonShares\":{\"raw\":48351000000,\"fmt\":\"48.35B\",\"longFmt\":\"48,351,000,000\"}}],\"maxAge\":86400}}],\"error\":null}}";

}
