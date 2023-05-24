# euler-events-hs


## New api metrics

1. New metrics placed completely within [directory](src/Euler/Events/MetricApi).
2. [MetricApi](src/Euler/Events/MetricApi/MetricApi.hs) contains new metric api with documentation.
3. [Extra](Euler.Events.MetricApi.Extra) contains `Ready`, `histogramRequestTime` (previous `instrumentApp`) and `sendHistorgam` landed on new metrics.
4. [Tests](test/MetricApiSpec.hs) contain more examples how to use new metrics api.
