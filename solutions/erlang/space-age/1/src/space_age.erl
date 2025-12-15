-module(space_age).

-export([age/2]).

seconds_in_year(earth) -> 31_557_600;
seconds_in_year(mercury) -> seconds_in_year(earth) * 0.2408467;
seconds_in_year(venus) -> seconds_in_year(earth) * 0.61519726;
seconds_in_year(mars) -> seconds_in_year(earth) * 1.8808158;
seconds_in_year(jupiter) -> seconds_in_year(earth) * 11.862615;
seconds_in_year(saturn) -> seconds_in_year(earth) * 29.447498;
seconds_in_year(uranus) -> seconds_in_year(earth) * 84.016846;
seconds_in_year(neptune) -> seconds_in_year(earth) * 164.79132.

age(Planet, Seconds) -> Seconds / seconds_in_year(Planet).
