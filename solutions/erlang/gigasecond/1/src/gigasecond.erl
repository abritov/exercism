-module(gigasecond).

-export([from/1]).

from({Date, Time}) ->
  from(calendar:datetime_to_gregorian_seconds({Date, Time}));

from(Date) when is_tuple(Date) ->
  from(calendar:datetime_to_gregorian_seconds({Date, {0, 0, 0}}));

from({Date}) when is_tuple(Date) ->
  from(calendar:datetime_to_gregorian_seconds({Date, {0, 0, 0}}));

from(Sec) when is_integer(Sec) ->
  calendar:gregorian_seconds_to_datetime(Sec + 1_000_000_000).
