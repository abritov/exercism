-module(atbash_cipher).

-export([decode/1, encode/1, split/1]).


decode(Str) when is_list(Str) ->
  io:format("~p:~p ~s -> ~p~n", [?MODULE, ?LINE, 'Str', Str]),
  lists:filtermap(fun decode/1, Str);

decode(32) -> {true, 32};
decode($A) -> {true, $Z};
decode($B) -> {true, $Y};
decode($C) -> {true, $X};
decode($D) -> {true, $W};
decode($E) -> {true, $V};
decode($F) -> {true, $U};
decode($G) -> {true, $T};
decode($H) -> {true, $S};
decode($I) -> {true, $R};
decode($J) -> {true, $Q};
decode($K) -> {true, $P};
decode($L) -> {true, $O};
decode($M) -> {true, $N};
decode($N) -> {true, $M};
decode($O) -> {true, $L};
decode($P) -> {true, $K};
decode($Q) -> {true, $J};
decode($R) -> {true, $I};
decode($S) -> {true, $H};
decode($T) -> {true, $G};
decode($U) -> {true, $F};
decode($V) -> {true, $E};
decode($W) -> {true, $D};
decode($X) -> {true, $C};
decode($Y) -> {true, $B};
decode($Z) -> {true, $A};
decode($a) -> {true, $z};
decode($b) -> {true, $y};
decode($c) -> {true, $x};
decode($d) -> {true, $w};
decode($e) -> {true, $v};
decode($f) -> {true, $u};
decode($g) -> {true, $t};
decode($h) -> {true, $s};
decode($i) -> {true, $r};
decode($j) -> {true, $q};
decode($k) -> {true, $p};
decode($l) -> {true, $o};
decode($m) -> {true, $n};
decode($n) -> {true, $m};
decode($o) -> {true, $l};
decode($p) -> {true, $k};
decode($q) -> {true, $j};
decode($r) -> {true, $i};
decode($s) -> {true, $h};
decode($t) -> {true, $g};
decode($u) -> {true, $f};
decode($v) -> {true, $e};
decode($w) -> {true, $d};
decode($x) -> {true, $c};
decode($y) -> {true, $b};
decode($z) -> {true, $a};
decode(_) -> false.

encode(Str) when is_list(Str) ->
  split(lists:filtermap(fun encode/1, Str));

encode(32) -> false;
encode($A) -> {true, $z};
encode($B) -> {true, $y};
encode($C) -> {true, $x};
encode($D) -> {true, $w};
encode($E) -> {true, $v};
encode($F) -> {true, $u};
encode($G) -> {true, $t};
encode($H) -> {true, $s};
encode($I) -> {true, $r};
encode($J) -> {true, $q};
encode($K) -> {true, $p};
encode($L) -> {true, $o};
encode($M) -> {true, $n};
encode($N) -> {true, $m};
encode($O) -> {true, $l};
encode($P) -> {true, $k};
encode($Q) -> {true, $j};
encode($R) -> {true, $i};
encode($S) -> {true, $h};
encode($T) -> {true, $g};
encode($U) -> {true, $f};
encode($V) -> {true, $e};
encode($W) -> {true, $d};
encode($X) -> {true, $c};
encode($Y) -> {true, $b};
encode($Z) -> {true, $a};
encode($a) -> {true, $z};
encode($b) -> {true, $y};
encode($c) -> {true, $x};
encode($d) -> {true, $w};
encode($e) -> {true, $v};
encode($f) -> {true, $u};
encode($g) -> {true, $t};
encode($h) -> {true, $s};
encode($i) -> {true, $r};
encode($j) -> {true, $q};
encode($k) -> {true, $p};
encode($l) -> {true, $o};
encode($m) -> {true, $n};
encode($n) -> {true, $m};
encode($o) -> {true, $l};
encode($p) -> {true, $k};
encode($q) -> {true, $j};
encode($r) -> {true, $i};
encode($s) -> {true, $h};
encode($t) -> {true, $g};
encode($u) -> {true, $f};
encode($v) -> {true, $e};
encode($w) -> {true, $d};
encode($x) -> {true, $c};
encode($y) -> {true, $b};
encode($z) -> {true, $a};

encode(Digit) when Digit >= $0, Digit =< $9 ->
  {true, Digit};

encode(_) -> false.


split([]) -> [];
split(List) when length(List) >= 5 ->
  {Split, Rest} = lists:split(5, List),
  string:join([Split | split(Rest)], " ");

split(List) ->
  List.

% split(List, Result) ->
%  {Split, Rest} = lists:split(5, List),
