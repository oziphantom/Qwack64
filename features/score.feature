Feature: Score Test

  This runs test levels to test the score and collisions.
  
Scenario: Fruit
  Given I have a simple overclocked 6502 system
  And I load prg "qwak.prg_test"
  And I load labels "qwak.acme"
  And I load bin "testLevels/fruitTest.bin" at $7000
  And I execute the procedure at $810 for no more than 100000 instructions until PC = MAINLOOP  
  And I write memory at $600 with 0
  And Joystick 2 is R
  When  Until $600 = 255 execute from $810
  Then Joystick 2 is NONE
  Then I expect to see GameData+2 equal 0
  And I expect to see GameData+3 equal 0
  And I expect to see GameData+4 equal 0
  And I expect to see GameData+5 equal 3
  And I expect to see GameData+6 equal 0
  And I expect to see GameData+7 equal 0

Scenario: FlowerFallTest
  Given I have a simple overclocked 6502 system
  And I load prg "qwak.prg_test"
  And I load labels "qwak.acme"
  And I load bin "testLevels/fallingFlowerTest.bin" at $7000
  And Joystick 2 is NONE
  And I execute the procedure at $810 for no more than 100000 instructions until PC = MAINLOOP  
  And I write memory at $600 with 0
  When  Until $600 = 255 execute from $810
  And I expect to see GameData_flowers equal 2
    
Scenario Outline: Test Joystick logic                                          
  Given I have a simple overclocked 6502 system                                              
  And I load prg "qwak.prg_test"                                                 
  And I load labels "qwak.acme"                                                  
  And Joystick 2 is L
  When I execute the procedure at scanJoystick for no more than 100 instructions 
  And I expect to see joyLeft equal 1                                            
 