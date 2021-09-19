/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package com.github.bradjacobs.yahoofinance.converter.datetime;

import org.testng.annotations.Test;

import java.time.Instant;
import java.util.Date;

import static org.testng.Assert.*;

public class EpochSecondsConverterTest
{
    private final EpochSecondsConverter converter = EpochSecondsConverter.getInstance();

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
    public void testInstantToSeconds() throws Exception
    {
        Instant instant = Instant.now();
        Long expected = instant.getEpochSecond();
        assertEquals(converter.convertToEpochSeconds(instant), expected);
    }

    @Test
    public void testNoOpToSeconds() throws Exception
    {
        // converter should NOT change the value in this case
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
    public void testSecondsToDateString() throws Exception
    {
        // note: these test values were from a yahoo response where it
        //   had the 'raw' value and the 'fmt' value
        Long input = 1612051200L;
        String expected = "2021-01-31";
        assertEquals(converter.convertToDateString(input), expected);
    }

    @Test
    public void testSecondsToDateTimeString() throws Exception
    {
        Long input = 1612059900L;
        String expected = "2021-01-31 02:25";
        assertEquals(converter.convertToDateTimeString(input), expected);
    }

    @Test
    public void testDateStringBackAndForth() throws Exception
    {
        String input = "2020-12-31";
        Long seconds = converter.convertToEpochSeconds(input);
        String result = converter.convertToDateString(seconds);
        assertEquals(result, input);
    }

    @Test
    public void testDateBackAndForth() throws Exception
    {
        // 1608603900  -- > GMT: Tuesday, December 22, 2020 02:25:00 AM
        Date inputDate = new Date(1608603900L * 1000);

        Long seconds = converter.convertToEpochSeconds(inputDate);
        Date result = converter.convertToDate(seconds);
        assertEquals(result, inputDate);
    }

    @Test
    public void testInstantBackAndForth() throws Exception
    {
        // 1608603900  -- > GMT: Tuesday, December 22, 2020 02:25:00 AM
        Instant inputInstant = Instant.now();

        Long seconds = converter.convertToEpochSeconds(inputInstant);
        Instant result = converter.convertToInstant(seconds);

        assertEquals(result.getEpochSecond(), inputInstant.getEpochSecond());
    }
}
