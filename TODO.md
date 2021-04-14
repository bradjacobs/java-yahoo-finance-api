# java-yahoo-finance-api

## TODO ITEMS (Not necessarily in order)
- [x] Implement a basic API call with valid JSON response
- [ ] Research "mock http clients"
- [ ] Implement a few "happy path" tests
- [ ] Add Validation/ErrorHandling for bad inputs
- [ ] Add option to return "pretty" json (if desired)
- [ ] Add option to return CSV instead of json
- [ ] JSON structure conversion
    - [ ] Add option keep 'raw', but remove 'fmt', 'longFmt' sections
    - [ ] Add ability to reformat the JSON (namely b/c json response looks a little weird imho)
- [ ] Implement support for multiple 'module' endpoints per a given request.
    - [ ] (ex ...v10/finance/quoteSummary/{symbol}?modules=assetProfile,price,financialData)
- [ ] Additional feature to return data in List/Map structures
- [ ] Add screener query support
  - [ ] Include any cookie/crumb handling (as needed)
- [ ] Research better way to handle parameter map values
  - [ ] This could morph into a 'builderish-looking' request (tbd)
- [ ] Devise simpler way for handling all params for a 'Price History' call
  - [ ] range, interval, period1, period1
  - [ ] events=div,split,...
  - [ ] (others)
- [ ] Add rudimentary 'retry' logic when certain exceptions occur.
- [ ] Take advantage of the PriceHistory `/v7/finance/download` CSV endpoint
- [ ] Confirm not pulling in 2 or more different versions for any given maven dependency
- [ ] Check and handle symbol formatting
  - [ ] i.e. Berkshire Hathaway Inc. Class B == "BRK.B" or "BRK-B"
- [ ] Add Documentation once code/interface structures are kinda figured out.

<br>
<br>

#### distant backlog (may or may not get to or only if needed)
- [ ] "Per request configurability" (tbd)
- [ ] Maybe add client 'interface', so it's possible to later have items like: "cachable YahooClient", etc
- [ ] Basic client side caching
- [ ] Can include something with News Feeds?
  - [ ] (i.e. https://feeds.finance.yahoo.com/rss/2.0/headline?s=APPL&region=US&lang=en-US)
- [ ] Performance improvements (as needed)
- [ ] Add ability to get response headers (only if there's a need)
