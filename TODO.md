# java-yahoo-finance-api

## TODO ITEMS (Not necessarily in order)
- [x] Implement a basic API call with valid JSON response
- [ ] Research "mock http clients"
- [ ] Implement a few "happy path" tests
- [ ] Add Validation/ErrorHandling for bad inputs
    - [ ] Also include case checks ("AAPL" vs "aapl")
- [ ] Add option to return "pretty" json (if desired)
- [ ] Add option to return CSV instead of json
- [ ] JSON structure conversion
    - [x] Add code keep 'raw', but remove 'fmt', 'longFmt' sections
    - [ ] Integrate in the remove 'raw' functionality above.
    - [ ] Add ability to reformat the JSON (namely b/c json response looks a little weird imho)
- [ ] Implement support for Multiple 'module' endpoints per a given request.
    - [ ] (ex ...v10/finance/quoteSummary/{symbol}?modules=assetProfile,price,financialData)
- [ ] Additional feature to return data in List/Map structures
- [ ] Add logger support (log4j, slf4j, etc, ...)
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
- [ ] Check and handle ticker symbol formatting
  - [ ] i.e. Berkshire Hathaway Inc. Class B == "BRK.B" or "BRK-B" or "BRKB"
- [ ] Add Documentation once code/interface structures are kinda figured out.
  - [ ] (may or may not utilize github pages)

  
<br>
<br>

#### distant backlog and random ideas ( lower priority and may or may not get to)
- [ ] Making httpClient behavior more customizable (setting different timeout, etc)
- [ ] Cursory check that special/extended characters are handled ok
  - [ ] ex - ...finance/quoteSummary/PBR?modules=assetProfile
- [ ] "Per Request Configurability" (tbd)
- [ ] Convenience way to have response saved directly to a file
- [ ] Maybe add client 'interface', so it's possible to later have items like: "cachable YahooClient", etc
- [ ] Basic client side caching
- [ ] Add in solution to easy view "code coverage" of existing unit tests
- [ ] Can include something with News Feeds?
  - [ ] (i.e. https://feeds.finance.yahoo.com/rss/2.0/headline?s=APPL&region=US&lang=en-US)
- [ ] Performance improvements (as needed)
- [ ] Add ability to get response headers (only if there's a need)
- [ ] Code scanning for vulnerabilities / bad dependency versions.
- [ ] Should test if works correctly when used in a multi-threaded fashion.
- [ ] Research difference (if any) b/w `/v10/finance/quoteSummary` vs `/v11/finance/quoteSummary`
- [ ] Option/Ability to only return certain fields

<br>
<br>
<br>

##### extra miscellanous thoughts for myself
* `Date` vs `Instant` (does it matter?)
* how useful are features in newer Java versions to 'force' a requirement of a newer Java?
