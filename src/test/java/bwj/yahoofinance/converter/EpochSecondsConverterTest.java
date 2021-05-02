/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package bwj.yahoofinance.converter;

import org.testng.annotations.Test;

import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.temporal.ChronoUnit;
import java.util.Date;

import static org.testng.Assert.*;

public class EpochSecondsConverterTest
{
    private EpochSecondsConverter converter = new EpochSecondsConverter();

    @Test
    public void testDateStringToSeconds() throws Exception
    {
        // note: these test values were from a yahoo response where it
        //   had the 'raw' value and the 'fmt' value
        String input = "2020-12-31";
        Long expected = 1609372800L;
        assertEquals(converter.convertToEpochSeconds(input), expected);
    }

    @Test
    public void testDateToSeconds() throws Exception
    {
        // note: these test values were from a yahoo response where it
        //   had the 'raw' value and the 'fmt' value
        Long expected = 1609372800L;
        Date date = new Date(expected * 1000L);
        assertEquals(converter.convertToEpochSeconds(date), expected);
    }

    @Test
    public void testNoOpToSeconds() throws Exception
    {
        // converter should not change the value in this case
        Long expected = 1609372800L;

        assertEquals(converter.convertToEpochSeconds(expected), expected);
    }

    @Test
    public void testWithMilliseconds() throws Exception
    {
        // given a millisecond value, it should convert it to seconds
        Long input = 1612051200987L;
        Long expected = 1612051200L;

        assertEquals(converter.convertToEpochSeconds(input), expected);
    }

    @Test
    public void testSecondsToString() throws Exception
    {
        // note: these test values were from a yahoo response where it
        //   had the 'raw' value and the 'fmt' value
        Long input = 1612051200L;
        String expected = "2021-01-31";

        assertEquals(converter.convertToString(input), expected);
    }

}
