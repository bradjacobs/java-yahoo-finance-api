/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package bwj.yahoofinance.converter;

import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;

import java.time.ZoneId;

import static org.testng.Assert.*;

/**
 * Tests for the TimestampStringConverter
 *
 *  IMPORTANT NOTE:
 *
 *    the converter has a "default" timezone if none explictly set.  Thus
 *    it's possible for some tests to fail if the timezone value isn't set
 *     (b/c the 'expected' result format could be off by an hour or 2 or more...)
 */
public class TimestampStringConverterTest
{
    private static final String LOCAL_TIME_ZONE = ZoneId.systemDefault().toString();

    // pick some timezone so the tests will still pass regardless of your local timezone.
    private static final String TEST_TIME_ZONE = "US/Pacific";


    @Test
    public void testDefaultDateHappyPath() throws Exception
    {
        long inputTime = 1619098200L;
        String expectedDate = "2021-04-22";

        TimestampStringConverter timestampStringConverter = new TimestampStringConverter();
        assertEquals(timestampStringConverter.toDateStr(inputTime), expectedDate, "mismatch of expected date string");
    }
    @Test
    public void testDateWithMilliseconds() throws Exception
    {
        long inputTime = 1619098200000L;
        String expectedDate = "2021-04-22";

        TimestampStringConverter timestampStringConverter = new TimestampStringConverter();
        assertEquals(timestampStringConverter.toDateStr(inputTime), expectedDate, "mismatch of expected date string");
    }

    @Test
    public void testDefaultDateHappyPathWithBuilder() throws Exception
    {
        long inputTime = 1619098200L;
        String expectedDate = "2021-04-22";

        TimestampStringConverter timestampStringConverter = new TimestampStringConverter.Builder().build();
        assertEquals(timestampStringConverter.toDateStr(inputTime), expectedDate, "mismatch of expected date string");
    }

    @Test
    public void testEndOfYear() throws Exception
    {
        // yahoo used this as end of 2020, but important to note the "expected" Timezone
        // ex:  reportDate -- raw: 1609372800  fmt: 2020-12-31
        long inputTime = 1609372800L;

        String expectedDate = "2020-12-31";

        TimestampStringConverter timestampStringConverter = new TimestampStringConverter.Builder().build();
        assertEquals(timestampStringConverter.toDateStr(inputTime), expectedDate, "mismatch of expected date string");
    }

    @Test
    public void testCustomDatePattern() throws Exception
    {
        long inputTime = 1619098200L;
        String pattern = "yyyy-MM-dd";
        String expectedDate = "2021-04-22";

        TimestampStringConverter timestampStringConverter = new TimestampStringConverter.Builder().withDatePattern(pattern).withTimeZone(TEST_TIME_ZONE).build();
        assertEquals(timestampStringConverter.toDateStr(inputTime), expectedDate, "mismatch of expected date string");
    }

    @Test
    public void testDateTimeHappyPath() throws Exception
    {
        long inputTime = 1619188200L;
        String expectedDate = "2021-04-23 07:30 AM";

        TimestampStringConverter timestampStringConverter =
                new TimestampStringConverter.Builder().withTimeZone(TEST_TIME_ZONE).build();

        assertEquals(timestampStringConverter.toDateTimeStr(inputTime), expectedDate, "mismatch of expected datetime string");
    }

    //  yyyy-MM-dd'T'HH:mm:ss

    @Test
    public void testCustomDateTimePattern() throws Exception
    {
        long inputTime = 1619188200L;
        String pattern = "yyyy-MM-dd'T'HH:mm:ss";
        String expectedDate = "2021-04-23T07:30:00";

        TimestampStringConverter timestampStringConverter = new TimestampStringConverter.Builder().withDateTimePattern(pattern).withTimeZone(TEST_TIME_ZONE).build();
        assertEquals(timestampStringConverter.toDateTimeStr(inputTime), expectedDate, "mismatch of expected datetime string");
    }


    @Test
    public void testConfirmDefaultTimeZone() throws Exception
    {
        String expectedDefaultTimeZone = "GMT";

        TimestampStringConverter timestampStringConverter =
                new TimestampStringConverter.Builder().build();

        assertEquals(timestampStringConverter.getTimeZone(), expectedDefaultTimeZone, "mismatch of expected timezone");
    }



    // Validation Tests ...
    ///////////////////////////////

    @DataProvider(name = "data-invalid-timezones")
    public Object[][] invalidTimeZoneParams(){
        return new Object[][] {
                {"__FAKE__"},
                {""},
                {null},
                {"US"},
                {"/"},
        };
    }

    @DataProvider(name = "data-invalid-date-patterns")
    public Object[][] invalidDatePatternParams(){
        return new Object[][] {
                {"yyyyyyyyyyyy mmmmmmmmm qqqqqq"},
              // {""},
                {null},
                {"ABCDEF"},
        };
    }


    @Test(dataProvider = "data-invalid-timezones",
            expectedExceptions = { IllegalArgumentException.class },
            expectedExceptionsMessageRegExp = "Invalid timezone.*")
    public void testInvalidTimeZone(String timezoneValue) throws Exception
    {
        TimestampStringConverter timestampStringConverter =
                new TimestampStringConverter.Builder().withTimeZone(timezoneValue).build();
    }

    @Test(dataProvider = "data-invalid-date-patterns",
            expectedExceptions = { IllegalArgumentException.class },
            expectedExceptionsMessageRegExp = "Invalid datePattern.*")
    public void testInvalidDatePattern(String datePatternValue) throws Exception
    {
        TimestampStringConverter timestampStringConverter =
                new TimestampStringConverter.Builder().withDatePattern(datePatternValue).build();
    }

    @Test(dataProvider = "data-invalid-date-patterns",
            expectedExceptions = { IllegalArgumentException.class },
            expectedExceptionsMessageRegExp = "Invalid dateTimePattern.*")
    public void testInvalidDateTimePattern(String datePatternValue) throws Exception
    {
        TimestampStringConverter timestampStringConverter =
                new TimestampStringConverter.Builder().withDateTimePattern(datePatternValue).build();
    }


}
