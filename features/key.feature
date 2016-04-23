Feature: Key

  This assembles simple code and executes the code until a specific program counter value is reached.
  It then tests continuing the execution and stopping again.
  
  
  
 Scenario:test qwak to key collision and that it removes the blocks and opens the door
  Given I have a simple overclocked 6502 system
  And I load prg "qwak.prg_test"
  And I load labels "qwak.acme"
  And I load bin "testLevels\key.bin" at $7000 
  And I execute the procedure at $810 for no more than 100000 instructions until PC = MAINLOOP  
  And I write memory at $600 with 0
  And Joystick 2 is R
  When I continue executing until  $600 = 100
  And Joystick 2 is NONE 
  Then I expect to see tileMapTemp+1 equal 0
  And I expect to see tileMapTemp+2 equal 0
  And I expect to see tileMapTemp+3 equal 0
  And I expect to see tileMapTemp+4 equal 0
  And I expect to see GameData_exitOpen equal 1