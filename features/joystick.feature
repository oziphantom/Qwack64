Feature: Joystick

Test and handle to make sure the joystick code reports right state and events

Scenario Outline: Test Joystick events
  Given I have a simple overclocked 6502 system
  And I load prg "qwak.prg_test"
  And I load labels "qwak.acme"
  And I write memory at joyLeft with 0
  And I write memory at joyRight with 0
  And I write memory at joyUp with 0
  And I write memory at joyDown with 0
  And I write memory at joyFire with 0
  And I write memory at oldJoyLeft with 0
  And I write memory at oldJoyRight with 0
  And I write memory at oldJoyUp with 0
  And I write memory at oldJoyDown with 0
  And I write memory at oldJoyFire with 0
  And I write memory at $Dc00 with <joy1>
  When I execute the procedure at scanJoystick for no more than 100 instructions
  And I write memory at $Dc00 with <joy2>
  Then I execute the procedure at scanJoystick for no more than 100 instructions
  And I expect to see joyLeft equal <left>
  And I expect to see joyRight equal <right>
  And I expect to see joyUp equal <up>
  And I expect to see joyDown equal <down>
  And I expect to see oldJoyLeft equal <oleft>
  And I expect to see oldJoyRight equal <oright>
  And I expect to see oldJoyUp equal <oup>
  And I expect to see oldJoyDown equal <odown>
  And I expect to see joyUpStart equal <uevent>
  And I expect to see joyFireEvent equal <fevent>
  
  Examples:
  | joy1  | joy2  | left | right | up | down | oleft | oright | oup | odown | uevent | fevent |
  | %11111|  0    |  1   |   0   | 1  |  0   |  0    |  0     | 0   | 0     | 1      | 1      |
  | %11111| %10101|  0   |   1   | 0  |  1   |  0    |  0     | 0   | 0     | 0      | 0      |
  | 0     | 0     |  1   |   0   | 1  |  0   |  1    |  0     | 1   | 0     | 0      | 0      |