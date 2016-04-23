Feature: Wrapping

  This assembles simple code and executes the code until a specific program counter value is reached.
  It then tests continuing the execution and stopping again.
  
Scenario: test collision when wrapping
  Given I have a simple overclocked 6502 system
  And I load prg "qwak.prg_test"
  And I load labels "qwak.acme"
  And I load bin "testLevels\fall1.bin" at $7000
  And I execute the procedure at $810 for no more than 100000 instructions until PC = MAINLOOP  
  And I write memory at $600 with 0
  When I continue executing until  $600 = 255 
  Then I expect to see mplexBuffer_ypos less than 80
  And I expect to see GameData_score equal 0
  And I expect to see GameData_score+1 equal 0
  And I expect to see GameData_score+2 equal 0
  And I expect to see GameData_score+3 equal 0
  And I expect to see GameData_score+4 equal 0
  
Scenario: test collision when wrapping 2
  Given I have a simple overclocked 6502 system
  And I load prg "qwak.prg_test"
  And I load labels "qwak.acme"
  And I load bin "testLevels\fall2.bin" at $7000
  And I execute the procedure at $810 for no more than 100000 instructions until PC = MAINLOOP  
  And I write memory at $600 with 0
  When I continue executing until  $600 = 100 
  Then I expect to see mplexBuffer_ypos greater than 170

Scenario: test collision when wrapping 3
  Given I have a simple overclocked 6502 system
  And I load prg "qwak.prg_test"
  And I load labels "qwak.acme"
  And I load bin "testLevels\fall3.bin" at $7000
  And I execute the procedure at $810 for no more than 100000 instructions until PC = MAINLOOP  
  And I write memory at $600 with 0
  And Joystick 2 is U
  When I continue executing until  $600 = 25
  And Joystick 2 is NONE 
  Then I expect to see mplexBuffer_ypos less than 80