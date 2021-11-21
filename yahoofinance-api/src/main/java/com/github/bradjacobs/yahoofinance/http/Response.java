package com.github.bradjacobs.yahoofinance.http;


import java.util.Collections;
import java.util.Map;
import java.util.TreeMap;

public class Response
{
    private final int code;
    private final String message;
    private final String body;
    private final Map<String,String> headerMap;

    private Response(Builder builder)
    {
        this.code = builder.code;
        this.message = builder.message;
        this.body = builder.body;
        this.headerMap = Collections.unmodifiableMap(builder.headerMap);
    }

    public int getCode()
    {
        return code;
    }

    public String getMessage()
    {
        return message;
    }

    public String getBody()
    {
        return body;
    }

    public boolean isError() {
        return this.code >= 400;
    }

    public String getHeader(String headerName)
    {
        if (headerName == null) {
            return null;
        }
        return this.headerMap.get(headerName);
    }

    public Map<String, String> getHeaderMap()
    {
        return this.headerMap;
    }

    public static Builder builder() {
        return new Builder();
    }

    public static class Builder
    {
        private Builder() { }

        private int code;
        private String message;
        private String body;
        private final Map<String,String> headerMap = new TreeMap<>();

        public Builder code(int code) {
            this.code = code;
            return this;
        }

        public Builder message(String message) {
            this.message = message;
            return this;
        }

        public Builder body(String body) {
            this.body = body;
            return this;
        }

        public Builder withHeader(String name, String value) {
            headerMap.put(name, value);
            return this;
        }

        public Response build() {
            return new Response(this);
        }
    }

}
