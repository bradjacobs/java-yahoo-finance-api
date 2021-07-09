/*
 * This file is subject to the terms and conditions defined in 'LICENSE' file.
 */
package com.github.bradjacobs.yahoofinance.converter.datetime;

import org.testng.annotations.Test;

import java.time.Instant;

import static org.testng.Assert.*;

public class DateStringInstantConverterTest
{
    private DateStringInstantConverter converter = new DateStringInstantConverter();

    @Test
    public void testBackAndForthConversion() throws Exception
    {
        String startDate = "2021-02-11";
        Instant instant = converter.convertToInstant(startDate);
        String convertedDateString = converter.convertToString(instant);

        assertEquals(convertedDateString, startDate, "mismatch expected date string");
    }

}