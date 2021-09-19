/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package com.github.bradjacobs.yahoofinance.converter.datetime.helper;

import com.github.bradjacobs.yahoofinance.converter.datetime.helper.DateStringInstantConverter;
import org.testng.annotations.Test;

import java.time.Instant;

import static org.testng.Assert.*;

public class DateStringInstantConverterTest
{
    private final DateStringInstantConverter converter = new DateStringInstantConverter();

    @Test
    public void testBackAndForthConversion() throws Exception
    {
        String startDate = "2021-02-11";
        Instant instant = converter.convertToInstant(startDate);
        String convertedDateString = converter.convertToString(instant);

        assertEquals(convertedDateString, startDate, "mismatch expected date string");
    }

    @Test
    public void testNullDateString() throws Exception
    {
        assertNull(converter.convertToInstant(null));
    }

    @Test
    public void testNullInstant() throws Exception
    {
        assertNull(converter.convertToString(null));
    }

}
