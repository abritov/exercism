-module(allergies).

-export([allergies/1, is_allergic_to/2]).


allergies(Score) ->
  Number = Score band 255,
  Flags = [1 bsl X || X <- lists:seq(0, 7)],
  Allergens = lists:filter(fun (Bit) -> Number band Bit == Bit end, Flags),
  lists:map(fun flag_to_allergy/1, Allergens).

is_allergic_to(_Substance, 0) -> false;
is_allergic_to(Substance, Score) ->
  case Substance of
    eggs ->
      Score band 1 =/= 0;
    peanuts ->
      Score band 2 =/= 0;
    shellfish ->
      Score band 4 =/= 0;
    strawberries ->
      Score band 8 =/= 0;
    tomatoes ->
      Score band 16 =/= 0;
    chocolate ->
      Score band 32 =/= 0;
    pollen ->
      Score band 64 =/= 0;
    cats ->
      Score band 128 =/= 0;
    _ ->
      undefined
  end.

flag_to_allergy(1) -> eggs;
flag_to_allergy(2) -> peanuts;
flag_to_allergy(4) -> shellfish;
flag_to_allergy(8) -> strawberries;
flag_to_allergy(16) -> tomatoes;
flag_to_allergy(32) -> chocolate;
flag_to_allergy(64) -> pollen;
flag_to_allergy(128) -> cats.

