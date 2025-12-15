-module(meetup).

-export([meetup/4, find_day2/4]).

meetup(Year, Month, DayOfWeek, last) ->
  find_day2(DayOfWeek, lists:reverse(get_month_days(Year, Month)), 1, undefined);

meetup(Year, Month, DayOfWeek, teenth) ->
  Now = calendar:date_to_gregorian_days(Year, Month, 13),
  Days = lists:seq(Now, Now + 19), 
  find_day2(DayOfWeek, Days, 1, undefined);

meetup(Year, Month, DayOfWeek, Week) ->
  find_day2(DayOfWeek, get_month_days(Year, Month), word_to_index(Week), undefined).

get_month_days(Year, Month) ->
  Now = calendar:date_to_gregorian_days(Year, Month, 1),
  lists:takewhile(fun (Day) ->
                      case calendar:gregorian_days_to_date(Day) of 
                        {Year, Month, _D} ->
                          true;
                        _ ->
                          false
                      end
                  end,
                  lists:seq(Now, Now + 31)).

find_day2(DayToFind, [Day | Days], WeekIndex, _Result) when WeekIndex > 0 ->
  Weekday = week_day_to_index(DayToFind),
  Date = calendar:gregorian_days_to_date(Day),
  case calendar:day_of_the_week(Date) of
    Weekday ->
      find_day2(DayToFind, Days, WeekIndex - 1, Day);
    _ ->
      find_day2(DayToFind, Days, WeekIndex, Day)
  end;

find_day2(_DayToFind, _Days, WeekIndex, Result) when WeekIndex == 0 -> calendar:gregorian_days_to_date(Result).

word_to_index(first) -> 1;
word_to_index(second) -> 2;
word_to_index(third) -> 3;
word_to_index(fourth) -> 4;
word_to_index(last) -> 5.

week_day_to_index(monday) -> 1;
week_day_to_index(tuesday) -> 2;
week_day_to_index(wednesday) -> 3;
week_day_to_index(thursday) -> 4;
week_day_to_index(friday) -> 5;
week_day_to_index(saturday) -> 6;
week_day_to_index(sunday) -> 7.

