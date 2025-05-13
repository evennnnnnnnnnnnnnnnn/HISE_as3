with Ada.Characters.Latin_1;

package MyStringTokeniser with SPARK_Mode is

   type TokenExtent is record
      Start : Positive;
      Length : Natural;
   end record;

   type TokenArray is array(Positive range <>) of TokenExtent;

   function Is_Whitespace(Ch : Character) return Boolean is
     (Ch = ' ' or Ch = Ada.Characters.Latin_1.LF or
        Ch = Ada.Characters.Latin_1.HT);

   procedure Tokenise(S : in String; Tokens : in out TokenArray; Count : out Natural) with
     Pre => (if S'Length > 0 then S'First <= S'Last) and Tokens'First <= Tokens'Last,
   -- Count does not exceed the number of elements inside the Token
   -- Ensures that the outside program does not get an 'index out of range error' when
   -- trying to access the Tokens array using Count.
     Post => Count <= Tokens'Length
   -- For every token stored inside Tokens array, if the token is non-empty, it should
   -- locate entirely within the boundary of string S.
   -- This ensures that very token inside Tokens array is a valid token generated from S,
   -- and does not cause memory leakage when trying to access the tokens.
     (for all Index in Tokens'First..Tokens'First+(Count-1) =>
          (Tokens(Index).Start >= S'First and
          Tokens(Index).Length > 0) and then
            Tokens(Index).Length-1 <= S'Last - Tokens(Index).Start)

end MyStringTokeniser;
