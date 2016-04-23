Feature: Collision

  This assembles simple code and executes the code until a specific program counter value is reached.
  It then tests continuing the execution and stopping again.

Scenario Outline:test qwak to tile collision
  Given I have a simple overclocked 6502 system
  And I load prg "qwak.prg_test"
  And I load labels "qwak.acme"
  And I write memory at CollideSpriteToCheck with 0
  And I write memory at CollideSpriteBoxIndex with 0
  And I write memory at mplexBuffer+32 with <X>
  And I write memory at mplexBuffer+64 with <MSB>
  And I write memory at mplexBuffer with <Y>
  And I write memory at checkSpriteToCharData with <deltaX>
  And I write memory at checkSpriteToCharData+1 with <deltaY>
  When I execute the procedure at newCollision for no more than 100 instructions
  Then I expect to see CollideInternalTTLX equal <TLX>
  And I expect to see CollideInternalTTLY equal <TLY>
  And I expect to see CollideInternalTBRX equal <BRX>
  And I expect to see CollideInternalTBRY equal <BRY>

  Examples:
    |  X  |  MSB  |  Y  | deltaX | deltaY | TLX | TLY   | BRX | BRY    |
    | 24  |  1    |  50 | 0      | 0      | 0   | 0     | 0   | 0      |
    | 18  |  1    |  44 | 0      | 0      | 15  | 192   | 0   | 192    |
    | 18  |  1    |  44 | 0      | 1      | 15  | 192   | 0   | 192    |
    | 18  |  1    |  44 | 2      | 1      | 15  | 192   | 0   | 192    |
    | 146 |  1    | 140 | 0      | 0      | 7   | 80    | 8   | 6*16   |
    | 4   |  0    | 50  | 0      | 0      | 14  | 0     | 15  | 0      |
    
    
Scenario Outline: test qwak to ent collision
  Given I have a simple overclocked 6502 system
  And I load prg "qwak.prg_test"
  And I load labels "qwak.acme"
  And I load bin "testLevels\collide.bin" at $7000
  Then I execute the procedure at convertLevelToTileMap for no more than 10000 instructions
  Then I execute the procedure at unpackEntityBytes for no more than 10000 instructions
  Then I execute the procedure at BuildEntCollisionTable for no more than 10000 instructions
  Then I write memory at mplexBuffer_ypos with <Y>
  Then I write memory at mplexBuffer_xpos with <X>
  Then I write memory at mplexBuffer_xmsb with <notMSB>
  Then I execute the procedure at collidePlayerAgainstRest for no more than 10000 instructions
  And I expect register ST exclude stC
  
  Examples:
   | X  | Y   |notMSB |
   | 24 | 50  | 1     |
   |136 | 130 | 1     | 
   |136 | 114 | 1     |
   |152 | 114 | 1     |
   |168 | 114 | 1     |
   |168 | 130 | 1     |
   |168 | 146 | 1     |
   |152 | 146 | 1     |
   |136 | 146 | 1     |
   
Scenario: test disolve
  Given I have a simple overclocked 6502 system
  And I load prg "qwak.prg_test"
  And I load labels "qwak.acme"
  And I load bin "testLevels\dissolve.bin" at $7000
  And I execute the procedure at $810 for no more than 100000 instructions until PC = MAINLOOP  
  And I write memory at $600 with 0
  When I continue executing until  $600 = 255 
  Then I expect to see mplexBuffer_ypos greater than 178 
  
Scenario: test spike
  Given I have a simple overclocked 6502 system
  And I load prg "qwak.prg_test"
  And I load labels "qwak.acme"
  And I load bin "testLevels\spike.bin" at $7000
  And I execute the procedure at $810 for no more than 100000 instructions until PC = MAINLOOP  
  And I write memory at $600 with 0
  And Joystick 2 is R
  When I continue executing until  $600 = 50
  And Joystick 2 is NONE 
  Then I expect to see GameData_lives less than 5 
  
 Scenario: test spike with shield
  Given I have a simple overclocked 6502 system
  And I load prg "qwak.prg_test"
  And I load labels "qwak.acme"
  And I load bin "testLevels\spikeShield.bin" at $7000
  And I execute the procedure at $810 for no more than 100000 instructions until PC = MAINLOOP  
  And I write memory at $600 with 0
  And Joystick 2 is R
  When I continue executing until  $600 = 50
  And Joystick 2 is NONE 
  Then I expect to see GameData_lives equal 5 