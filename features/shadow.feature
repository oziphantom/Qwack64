Feature: Shadow

  This assembles simple code and executes the code until a specific program counter value is reached.
  It then tests continuing the execution and stopping again.

Scenario Outline: Test that Y = 0 and both L and R are 0
  Given I have a simple overclocked 6502 system
  And I load prg "qwak.prg_test"
  And I load labels "qwak.acme"
  And I set register Y to <Y>
  When I execute the procedure at calcBCDEforTileY for no more than 40 instructions until PC = END_LEFT_RIGHT_CHECK
  Then I expect to see ZPTemp4 equal <VALUE>
  
  Examples:
    | Y  | VALUE |
    | 0  |  128  |
    | 2  |  0    |
    | 15 |  64   |
    | 16 |  128  |
    | 32 |  128  |
    | 47 |  64   |


  Scenario Outline: Test that BCDE bits match for tile data
  Given I have a simple overclocked 6502 system
  And I load prg "qwak.prg_test"
  And I load labels "qwak.acme"
  And I set register Y to <Y>
  And I write memory at tileMapTemp with <TL>
  And I write memory at tileMapTemp+1 with <TM>
  And I write memory at tileMapTemp+2 with <TR>
  And I write memory at tileMapTemp+16 with <L>
  When I execute the procedure at calcBCDEforTileY for no more than 400 instructions until PC = BCDEYVALUECHECK
  Then I expect register Y equal <Result>

  Examples:
    |  Y | TL | TM | TR | L | Result |
    | 17 | 0  | 0  | 0  | 0 | $0     |
    | 17 | 1  | 0  | 0  | 0 | $8     |
    | 17 | 0  | 1  | 0  | 0 | $4     |
    | 17 | 0  | 0  | 1  | 0 | $2     |
    | 17 | 0  | 0  | 0  | 1 | $1     |
    | 17 | 1  | 1  | 0  | 0 | 8+4    |
    | 17 | 2  | 0  | 3  | 4 | 8+2+1  |
    | 3  | 1  | 1  | 1  | 1 | 1      |
    | 18 | 1  | 1  | 1  | 1 | 8+4    |
    | 32 | 1  | 1  | 1  | 1 | 4      |
    | 16 | 0  | 0  | 0  | 0 | 0      |
    | 128| 0  | 0  | 0  | 0 | 0      |