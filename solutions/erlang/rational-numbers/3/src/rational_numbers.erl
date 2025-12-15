-module(rational_numbers).

-export([absolute/1, add/2, divide/2, exp/2, mul/2, reduce/1, sub/2, gcd/2, root/2]).


absolute({N, D}) -> reduce({abs(N), abs(D)}).

add({N1, D1}, {N2, D2}) -> reduce({N1 * D2 + N2 * D1, D1 * D2}).

divide({N1, D1}, {N2, D2}) -> reduce({N1 * D2, N2 * D1}).

% exp(Exp, {_N, D}) when Exp > 0, D == 0 -> 1.0;
exp(Exp, {N, D}) when is_integer(Exp), Exp > 0 -> root(math:pow(Exp, N), D);
exp(Exp, {N, D}) when is_integer(Exp), Exp > 0 -> math:pow(N, Exp) / math:pow(D, Exp);
exp(Exp, {N, D}) when is_integer(Exp), Exp < 0 -> math:pow(D, abs(Exp)) / math:pow(N, abs(Exp));

exp({N, D}, Exp) when Exp >= 0 -> {trunc(math:pow(N, Exp)), trunc(math:pow(D, Exp))};
exp({N, D}, Exp) when Exp < 0 -> reduce({trunc(math:pow(D, abs(Exp))), trunc(math:pow(N, abs(Exp)))}).

% exp(Exp, {N, D}) when is_float(Exp), Exp > 0 -> math:pow(N, Exp) / math:pow(D, Exp);

mul({N1, D1}, {N2, D2}) -> reduce({N1 * N2, D1 * D2}).

reduce({N, D}) ->
  Divisor = gcd(N, D),
  case D div Divisor of
    Denominator when Denominator > 0 ->
      {N div Divisor, Denominator};
    Denominator ->
      {-(N div Divisor), -Denominator}
  end.

sub({N1, D1}, {N2, D2}) -> reduce({N1 * D2 - N2 * D1, D1 * D2}).


gcd(A, 0) -> A;
gcd(A, B) -> gcd(B, A rem B).

root(Num, Pow) -> math:pow(Num, 1 / Pow).
