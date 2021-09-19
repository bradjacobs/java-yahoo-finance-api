/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package com.github.bradjacobs.yahoofinance.converter.datetime;

import org.testng.annotations.Test;

import java.time.Instant;
import java.util.Date;

import static org.testng.Assert.assertEquals;

public class MetaEpochSecondsConverterTest
{
    private final MetaEpochSecondsConverter converter = MetaEpochSecondsConverter.getInstance();

    @Test
    public void testDateStringToSeconds() throws Exception
    {
        // note: these test values were from a yahoo response where it
        //   had the 'raw' value and the 'fmt' value
        String input = "2020-12-31";
        Long expected = 1609372800L;
        assertEquals(converter.fromString(input), expected);
    }

    @Test
    public void testDateToSeconds() throws Exception
    {
        // note: these test values were from a yahoo response where it
        //   had the 'raw' value and the 'fmt' value
        Long expected = 1609372800L;
        Date date = new Date(expected * 1000L);
        assertEquals(converter.fromDate(date), expected);
    }

    @Test
    public void testInstantToSeconds() throws Exception
    {
        Instant instant = Instant.now();
        Long expected = instant.getEpochSecond();
        assertEquals(converter.fromInstant(instant), expected);
    }

//    @Test
//    public void testNoOpToSeconds() throws Exception
//    {
//        // converter should NOT change the value in this case
//        Long expected = 1609372800L;
//        assertEquals(converter.from(), expected);
//    }

//    @Test
//    public void testWithMilliseconds() throws Exception
//    {
//        // given a millisecond value, it should convert it to seconds
//        Long input = 1612051200987L;
//        Long expected = 1612051200L;
//        assertEquals(converter.convertToEpochSeconds(input), expected);
//    }

    @Test
    public void testSecondsToDateString() throws Exception
    {
        // note: these test values were from a yahoo response where it
        //   had the 'raw' value and the 'fmt' value
        Long input = 1612051200L;
        String expected = "2021-01-31";
        assertEquals(converter.toDateString(input), expected);
    }

    @Test
    public void testSecondsToDateTimeString() throws Exception
    {
        Long input = 1612059900L;
        String expected = "2021-01-31 02:25";
        assertEquals(converter.toDateTimeString(input), expected);
    }

    @Test
    public void testDateStringBackAndForth() throws Exception
    {
        String input = "2020-12-31";
        Long seconds = converter.fromString(input);
        String result = converter.toDateString(seconds);
        assertEquals(result, input);
    }

    @Test
    public void testDateTimeStringBackAndForth() throws Exception
    {
        String input = "2021-01-31 02:25";
        Long seconds = converter.fromString(input);
        String result = converter.toDateTimeString(seconds);
        assertEquals(result, input);
    }

    @Test
    public void testDateBackAndForth() throws Exception
    {
        // 1608603900  -- > GMT: Tuesday, December 22, 2020 02:25:00 AM
        Date inputDate = new Date(1608603900L * 1000);

        Long seconds = converter.fromDate(inputDate);
        Date result = converter.toDate(seconds);
        assertEquals(result, inputDate);
    }

    @Test
    public void testInstantBackAndForth() throws Exception
    {
        // 1608603900  -- > GMT: Tuesday, December 22, 2020 02:25:00 AM
        Instant inputInstant = Instant.now();

        Long seconds = converter.fromInstant(inputInstant);
        Instant result = converter.toInstant(seconds);

        assertEquals(result.getEpochSecond(), inputInstant.getEpochSecond());
    }
}
