$regfile = "m1280def.dat"                                   ' Set the chip as a Mega 168
$crystal = 16000000
$baud = 9600
$hwstack = 32                                               ' default use 32 for the hardware stack
$swstack = 10                                               ' default use 10 for the SW stack
$framesize = 40
$lib "pulsein.lib"
$baud1 = 9600
Config Serialin1 = Buffered , Size = 20
Dim J As Word , Pulsewidth As Word , Temp As Byte , X As Word , W As Word , Temp1 As Word
Dim Rwheel As Byte , Lwheel As Byte , Speed As Byte , Color2 As Dword , Length As Word , I As Word
Dim Red As Word , Green As Word , Blue As Word , Vpoint As Word , Ws As Single
Dim Flag1 As Byte , K As Byte , Redold As Word , Test1 As Single , Factor As Single , Temps As Single
Dim Ii As Byte , Comp(10) As Byte , Angle As Single , Xx As Single , Angle1 As Single , Anglef As Single
Dim Offset1 As Byte , Offset2 As Byte , T As Byte , Bima As Byte , Anglen As Byte , Anglew As Byte
Dim B(20) As Byte , Thesi As Byte , Bit1 As Bit , Point1 As Byte , Factors As Single , Factorw As Single , Factore As Single
Dim K1 As Word , Point As Byte , Xpos As Word , Ypos As Word , Pos As Word , Pos1 As Word

Config Com3 = 9600 , Synchrone = 0 , Parity = None , Stopbits = 1 , Databits = 8 , Clockpol = 0
Config Serialin2 = Buffered , Size = 20                     'THIS IS FOR COM3  TX2,RX2 ARDUINO BOARD
Open "COM3:" For Binary As #3
Open "COM2:" For Binary As #1                               'THIS IS FOR COM1  TX1,RX1 ARDUINO BOARD
Enable Interrupts


Config Porta = Output                                       'STEPPERS
Porta = 0
Config Portb.0 = Input                                      'HORIZONTAL TERMINAL SWITCH
Config Portl.3 = Output                                     'RELAY
Config Porte.3 = Output                                     'POWER +5V FOR SCANNER
Config Portb.1 = Output                                     'SERVO
Config Portb.3 = Input                                      'VERTICAL POSITION SENSOR
Config Portl.1 = Input                                      'HEAD POSITION SENSOR
Config Portc.4 = Input                                      'COLOR2
Config Portc.0 = Input                                      'TERMINAL BUTTON'
Config Portg.2 = Input                                      'BLACK LINE DETECT FRONT'
Config Portg.0 = Input                                      'BLACK LINE DETECT BACK'
Config Portl.6 = Input                                      'START BUTTON'
Config Portb.2 = Input                                      'OBJECT DISTANCE
S2 Alias Porte.4                                            'COLOR1 SELLECT
S3 Alias Porte.5                                            'COLOR2 SELECT
Sout Alias Portc.2
Config S2 = Output
Config S3 = Output
Config Sout = Input
Portb.1 = 0
Speed = 140                                                 'max speed
Bima = 0                                                    'max speed




 '=== PWM SETUP ===
Config Portb.7 = Output
Config Portb.4 = Output
Config Porth.6 = Output
Config Portg.5 = Output


Config Timer0 = Pwm , Prescale = 1 , Compare A Pwm = Clear Up , Compare B Pwm = Clear Up
Config Timer2 = Pwm , Prescale = 1 , Compare A Pwm = Clear Up , Compare B Pwm = Clear Up
Offset1 = 92                                                'RIGHT TURN
Offset2 = 84



Goto Start0                                                'GOTO TO THE BEGINNING OF THE PROGRAMM

                                                            'SOME CALIBRATION RUTINES USED FOR VARIOUS COMPONETS
                                                            'SUCH AS COMPASS, RGB SENSORS

Goto Callib_compass                                         'COMPASS CALIBRATION
                                                            'MAINLY NOT USED IN THE ROBOT MOVES



While Pinc.0 = 1

   Waitms 200
Wend

Goto Callib_red

Anglef = 134
Offset1 = 105

Callib_red:

'Goto Turn


Callibrations:



   Set Porte.3
   Waitms 500
   Pulsein Color2 , Pinc , 4 , 0

   For I = 1 To 50                                       'RED DATABASE
      Gosub Diavase_red
   Next I

   Do
      Gosub Diavase_red
      Temps = Color2 / W
      Print Color2 ; "   " ; W ; "       " ; Temps
   Loop


   Goto Start0

Callib_rgb:
   Gosub Openservo
   Wait 1
   Do

      Gosub Read_colors
      Wait 1
   Loop
Callib_compass:
   Do
      Gosub Angles
      Print Round(angle)
      Wait 1
   Loop                                                     'END OF CALIBRATION RUTINES



                                                            '******    HERE STARTS THE ROBOT REACTIONS     ******

Start0:
   Factors = 3.5                                            'RED SENSITIVITY FACTORS
   Factorw = 5.5                                            'WEST DIRECTION COLOR FACTOR
   Factore = 5                                              'EAST DIRECTION COLOR FACTOR
   Reset Porte.3                                            'STOP RED LASER SCANNER
   While Pinc.0 = 1                                         'WAIT UNTIL THE FRONT BUTTON IS PRESSED
   Wend
   Wait 1
   Print "PRESS FRONT BUTTON OR"                            'COMUNICATE WITH BLUETOOTH AND TERMINAL
   Print "WRITE S TO SETUP ANGLES"                          'CHOOSE CALIBRATION
   Print "PRESS BACK BUTTON OR R"
   Print "TO RUN THE ROBOT"
   Print #3 , "PRESS FRONT BUTTON OR"
   Print #3 , "WRITE S TO SETUP ANGLES"
   Print #3 , "PRESS BACK BUTTON OR R"
   Print #3 , "TO RUN THE ROBOT"
   While Pinc.0 = 1
      Gosub Get_char                                        'READ BLUETOOTH
      If B(1) = 83 Then Goto Xekina                         'S CHARACTER PRESSED
      If B(1) = 82 Or Pinl.6 = 0 Then Goto Moterakia        'R CHARACTER PRESSED'
      Waitms 200
   Wend

Xekina:
   Print "PRESS FRONT BUTTON OR"                            'DECIDE WHAT TO DO
   Print "WRITE A TO SETUP ANGLES"
   Print "PRESS BACK BUTTON OR"
   Print "WRITE R TO RUN THE ROBOT"
   Print #3 , "PRESS FRONT BUTTON OR"
   Print #3 , "WRITE A TO SETUP ANGLES"
   Print #3 , "PRESS BACK BUTTON OR"
   Print #3 , "WRITE R TO RUN THE ROBOT"

   Wait 2
   While Pinc.0 = 1
      Gosub Get_char
      If B(1) = 65 Then Goto Start1                         'A CHARACTER PRESSED'
      If B(1) = 82 Or Pinl.6 = 0 Then Goto Moterakia        'R CHARACTER PRESSED'
      Waitms 100
   Wend
Start1:
   Print "PUT ROBOT AT STARTING POSITION"                   'CALIBRATE COMPASS  FOR SOUTH DIRECTION
   Print "PRESS FRONT BUTTON OR"
   Print "WRITE S TO SET SOUTH ANGLE"
   Print "ANGLE NO MORE THAN 100 DEGREES"
   Print "ROTATE COMPASS"
   Print #3 , "PUT ROBOT AT STARTING POSITION"
   Print #3 , "PRESS FRONT BUTTON OR"
   Print #3 , "WRITE S TO SET SOUTH ANGLE"
   Print #3 , "ANGLE NO MORE THAN 100 DEGREES"
   Print #3 , "ROTATE COMPASS"
Start2:
   Wait 1
   Gosub Angles
   Print Round(angle)
   Print #3 , Round(angle)
   Gosub Get_char
   If Pinc.0 = 0 Or B(1) = 83 Then Goto Start5

   Goto Start2

Start5:                                                     'START OF THE MAIN PROGRAMM
   Anglef = 0                                               ''***********     INITIALIZE    STARTING ANGLE    *************
   For I = 1 To 20                                          'CREATE THE ANGLE STARTING MEASURE
      Gosub Angles
      Anglef = Anglef + Angle
   Next I
   Anglef = Anglef / 20                                     'CALCULATE THE MEAN MEASURE
   Anglef = Round(anglef)
   Anglen = Anglef
   Print "SOUTH ANGLE MEASURED " ; Anglen                   'CALIBRATE COMPASS FOR WEST DIRECTION
   Print "PUT ROBOT AT WEST DIRECTION"
   Print "DO NOT TURN THE COMPASS"
   Print "PRESS W NOW TO MEASURE IT"
   Print "OR PRESS FRONT BUTTON"

   Print #3 , "SOUTH ANGLE MEASURED " ; Anglen
   Print #3 , "PUT ROBOT AT WEST DIRECTION"
   Print #3 , "DO NOT TURN THE COMPASS"
   Print #3 , "PRESS W NOW TO MEASURE IT"
   Print #3 , "OR PRESS FRONT BUTTON"

Start4:
   Gosub Get_char
   If Pinc.0 = 0 Then Goto Start6                           'WAIT TO START
   If B(1) <> 87 Then Goto Start4                           '
Start6:
   For I = 1 To 20                                       '**********************     INITIALIZE    STARTING ANGLE    *************
      Gosub Angles
      Anglef = Anglef + Angle                               'CALCULATE THE MEAN WEST MEASURE ANGLE
   Next I
   Anglef = Anglef / 20
   Anglef = Round(anglef)
   Anglew = Anglef - Anglen
   Print #3 , "WEST ANGLE MEASURED " ; Anglef
   Print "WEST ANGLE MEASURED " ; Anglef
   Writeeeprom Anglen , 0                                   'SAVE MEASURES
   Writeeeprom Anglew , 1


'************************     MAIN PROGRAMM  ****************

Moterakia:
   Readeeprom Anglen , 0                                    'EVERYTHING FINISHED
   Anglef = Anglen
   Readeeprom Offset1 , 1
   Anglef = 140
   Offset1 = 90

    'Goto Tnorth


   If Anglef > 153 Or Offset1 > 110 Then                    'OUT OF LIMITS
      Print #3 , "WRONG DIRECTION SETUP"                    'IF YOU CHOOSE WRONG DIRECTIONS THE ROBOT DOES NOT RUN
      Print "WRONG DIRECTION SETUP"
      Goto Start0
   End If

   Do
      Gosub Closeservo
      Waitms 200                                            'CLOSE COLLECTOR HAND
      Gosub Horizontal_home                                 'GOTO HORIZONTALLY HOME
      Gosub Vertical_home                                   'GOTO VERTICALY HOME
      Gosub Head_home                                       'PULL HEAD AT HOME POSITION
      Gosub Head_stop                                       'STOP HEAD MOOVE


      Print #3 , "GOING SOUTH"                              'START RUNNING FORWARD SOUTH
      Print "GOING SOUTH"
      Set Porte.3
      Waitms 200                                            'START LASER
      Factor = Factors                                      'LOAD FACTORS
      Flag1 = 0
      Color2 = 0
      Length = 1
      Pulsein Color2 , Pinc , 4 , 0
      For I = 1 To 50                                       'RED DATABASE
         Gosub Diavase_red
      Next I

      Synexeia:                                             '**************************    GOING SOUTH      ***********************
         Gosub Openservo
         Waitms 200
         Set Porte.3                                        'START LASER
         Point1 = 0

         While Pinc.0 = 1                                   'GOING UNTIL REACHING THE END OF ROUTE

            Angle1 = Anglef

            Rwheel = Speed                                  'SET THE WHEELS SPEED
            Lwheel = Speed + 100                            'THE MOTORS ARE RUNNING IN STEPS
            Gosub Robot_forward                             'FOR ACCURATE POSITIONING
            Waitms 6
            Gosub Robot_stop                                'AND MAXIMUM TORQUE
            Waitms 35
            Incr Bima
            If Bima > 100 Then                              'EVERY 100 STEPS WE CHECK THE DIRECTION OD THE ROBOT
               Gosub Stop_check                             'IF WE FIND MISSMACH WE CORRECT IT
               Gosub Check_dir
               Bima = 0
            End If

            Gosub Mismes
            'Gosub Diavase_red
            'Gosub Check_red
            'If Flag1 > 0 Then Incr Point1                   'MISTAKEN MEASURES'
            'If Point1 > 35 Then
             '  Point1 = 0
              ' Flag1 = 0
            'End If
            Print Flag1

            If Flag1 > 3 Then                               'WE CHECK THE LASER SCANNER FOR 3 CORRECT RED VIEWS
               Flag1 = 0
               Point1 = 0                                   'FOUND IT'
               Gosub Elexe                                  'WE RUN THE RUTINES FOR FINAL FRUIT CHECK
               Goto Synexeia                                'IF IT IS CORRECT IT COLLECTS IT
            End If
                                                            'THE ROBOT HAS REACHED THE END OF SOUTH ROUTE AND HAS TO TURN TO THE RIGHT
         Wend

         'Reset Porte.3
         'End

      Back_steps:                                                '******************************      BACK TO NORTH 20 STEPS   **************
         Print #3 , "BACK STEPS"                            'MAKE SOME BACK STEPS
         Print "BACK STEPS"                                 'TURN OFF LASER
         Reset Porte.3
         Gosub Head_stop
         Rwheel = Speed
         Lwheel = Speed + 100
         For I = 1 To 15
            Gosub Robot_reverse
            Waitms 6
            Gosub Robot_stop
            Waitms 20
         Next I
 'End
      Turn_west:                                                 '*********************************   TURNING WEST    ************
         Print #3 , "TURNING WEST"
         Print "TURNING WEST"                               'ROTATE TO THE RIGHT
         Reset Porte.3                                      'UNTIL THE WEST ANGLE IS REACHED
         Angle1 = Anglef + Offset1
         Rwheel = 110
         Lwheel = 110
         While Angle < Angle1
            Gosub Angles
            Gosub Robot_right
         Wend
         Gosub Robot_stop                                   'AFTER TURNING MAKE A FINAL CHECK OF THE WEST ANGLE
         Gosub Check_dir



         Print #3 , "GOING WEST"
         Print "GOING WEST"
         'Goto Tnorth
                                               '
      West_going:                                                 '***********************      GOING WEST     **********************

         Set Porte.3                                        'START LASER
         Factor =factorw
         Point1 = 0
         Flag1 = 0
         Color2 = 0
         Bima = 0

         Length = 1
         Pulsein Color2 , Pinc , 4 , 0
         For I = 1 To 50                                       'RED DATABASE
            Gosub Diavase_red
         Next I


         While Pinc.0 = 1                                   ' ***************     RUN FORWARD WEST      ******************
            Rwheel = Speed
            Lwheel = Speed + 100
            Gosub Robot_forward
            Waitms 7
            Gosub Robot_stop
            Waitms 25
            Incr Bima
            If Bima > 100 Then                              'MAKE THE DIRECTION CORRECTIONS
               Gosub Stop_check
               Gosub Check_dir
               Bima = 0
            End If

            Gosub Mismes
            'Gosub Diavase_red
            '''Gosub Check_red
            'If Flag1 > 0 Then Incr Point1
            'If Point1 > 35 Then
             ''  Point1 = 0
             '  Flag1 = 0
            'End If
            Print Flag1                                     'CHECK IF IS FOUND
            If Flag1 > 3 Then                               'MORE THAN 3 INDICATIONS SUPPOSE THAT IT IS FOUND
               Flag1 = 0
               Gosub Elexe
               Goto West_going
            End If                                          'FOUND IT'

         Wend                                               'THE ROBOT REACHED THE END OF WEST ROUTE


         'Reset Porte.3
         'End

         Print #3 , "GOING EAST"
         Print "GOING EAST"
      East:                                                       '

         Reset Porte.3                                            '*******************    TURNING TO EAST   **************
         Factor = Factore
         Gosub Closeservo                                   'MOVE THE HEAD TO HOME POSITION
         Waitms 200
         Point1 = 0
         Pos = 60
         Gosub Vertical_set
      East1:
         Reset Porte.3
         Pos = 150
         Gosub Horizontal_set
         Gosub Vertical_home
         Gosub Openservo
         Waitms 200
         Set Porte.3                                              '                                           'START LASER
      Sos1:
         Flag1 = 0
         Color2 = 0
         Length = 1
         Pulsein Color2 , Pinc , 4 , 0
         For I = 1 To 50                                    'RED DATABASE
            Gosub Diavase_red
         Next I
         Bima = 0

         While Ping.0 = 0 And Ping.2 = 0                          '**********    RUN FORWARD EAST     ************
            Rwheel = Speed                                  'GOING IN REVERCE SPEED
            Lwheel = Speed + 100
            Gosub Robot_reverse
            Waitms 6
            Gosub Robot_stop
            Waitms 45

            Incr Bima
            If Bima > 100 Then                              'MAKE THE DIRECTION CORRECTIONS
               Gosub Stop_check
               Gosub Check_dir
               Bima = 0
            End If
            Gosub Mismes
            'Gosub Diavase_red
            'Gosub Check_red
            'If Flag1 > 0 Then Incr Point1
            'If Point1 > 35 Then
             ''  Point1 = 0
            'Flag1 = 0
            'End If
            Print Flag1
            If Flag1 > 4 Then                               'FOUND IT'
               Flag1 = 0
               Gosub Elexe2
               Goto East1
            End If

         Wend

      Turn:                                                 '*******************    FOUND FIRST BLACK LINE    **************
         Reset Porte.3
         Print #3 , "FOUND FIRST BLACK LINE"                'WHEN IT GOES TO EAST DIRECTION
         Print "FOUND FIRST BLACK LINE"                     'IT SIMPLY CHECHS FOR BLACK LINES ON THE FLOOR
                                                            'WHEN IT DETECTS ONE THIS MEANS THAT IT HAS REACHED THE END TO EAST DIRECTION

         Gosub Closeservo                                   'IT TURNS THE HEAD TO HOME POSITION AGAIN
         Point1 = 0
         Pos = 60
         Gosub Vertical_set
         Gosub Horizontal_home
         Gosub Vertical_home
         For I = 1 To 80
            Rwheel = Speed
            Lwheel = Speed + 100
            Gosub Robot_reverse
            Waitms 5
            Gosub Robot_stop
            Waitms 30
         Next I

      Tnorth:

         While Ping.0 = 0 And Ping.2 = 0                    '*****     RUN FORWARD EAST   UNTIL SECOND BLACK LINE   *****
            Rwheel = Speed
            Lwheel = Speed + 100
            Gosub Robot_reverse
            Waitms 6
            Gosub Robot_stop
            Waitms 30
         Wend


      Dfdf:


         'Reset Porte.3
         Print #3 , "TURNING NORTH"
         Print "TURNING NORTH"
         Angle1 = Anglef
         Rwheel = 130
         Lwheel = 130

         While Angle > Angle1                               '*****************      TURN NORTH      ******************
            Gosub Angles
            Gosub Robot_left
         Wend
         Gosub Robot_stop                                   'CHECK FOR THE CORRECT NORTH ANGLE
         Gosub Check_dir

      Goingnorth:

         Print #3 , "GOING NORTH"
         Print "GOING NORTH"
         While Pinl.6 = 1                                   '*****************      GOING NORTH      ******************
            Rwheel = Speed+50
            Lwheel = Speed + 100
            Sos2:
               If Ping.0 > Ping.2 Then

                  Lwheel = 200
                  Rwheel = 200
                  Gosub Robot_left
                  Waitms 10
                  Gosub Robot_stop
                  Waitms 20
                  Goto Sos2

               End If
            Sos3:
               If Ping.0 < Ping.2 Then                      'HERE WE HAVE THE PROBLEM TO TARGET THE ROBOT AT A SPECIFIC POINT

                  Lwheel = 200                              'SO WE HAVE 2 BLACK DETECTORS TRYING TO PUT THE ROBOT
                  Rwheel = 200                              'AT A SPECIFIC DIRECTION AND POSITION
                  Gosub Robot_right                         'IT STARTS DETECTING A TRIANGLE WHOSE TOP IS LOCATED AT THE CORRECT POINT
                  Waitms 15
                  Gosub Robot_stop                          'FINALY WE HAVE A BLACK DIRECTION LINE SO AS TO GUIDE THE ROBOT
                  Waitms 15
                  Goto Sos3                                 'AT THE EXACT DIRECTION AND POSITION
               End If
                                                            'WE HAVE TO DO THIS BECAUSE AT THE TARGET POSITION IS EXISTED
               Gosub Robot_reverse                          'THE COLLISION OF THE 2 BUTTONS
               Waitms 5                                     'ONE BUTTON OF THE ROBOT MEANING THE END OF COLLECTION
               Gosub Robot_stop                             'AND THE OTHER ONE THE STARTING BUTTON OF THE LEGO ROBOT
               Waitms 30                                    'WHEN THIS IS PRESED THE LEGO MINDSTORMS ROBOT STARTS
         Wend                                               'TO TAKE THE BASKET AND REPLACE IT WITH A NEW ONE
         Print #3 , "BACK TO HOME"
         Print "BACK TO HOME"
         For I = 1 To 5                                             '******************    5 STEPS FORWARD      ********************
            Rwheel = Speed
            Lwheel = Speed + 100
            Gosub Robot_forward
            Waitms 6
            Gosub Robot_stop
            Waitms 20
         Next I
         Print #3 , "TAKE THE BASKET PLEASE"
         Print "TAKE THE BASKET PLEASE"
         Wait 20
         Print #3 , "READY AGAIN"
         Print "READY AGAIN"
         For I = 1 To 10                                            '*********************     10 STEPS FORWARD      ***************
            Rwheel = Speed
            Lwheel = Speed + 100
            Gosub Robot_forward
            Waitms 6
            Gosub Robot_stop
            Waitms 20
         Next I


   Loop





'**************************************
' SUBRUTINES'
'**************************************



Elexe2:
   Reset Porte.3                                            'STOP LASER
   For K = 1 To 33                                          'LOCATE TARGET  GOING FORWARD
                                                            'THIS ROUTINE CHECKS IF IT HAS FOUND THE CORRECT TARGET
                                                            'FOR THE REVERCE GOING SCANS
      Rwheel = Speed
      Lwheel = Speed + 100
      Gosub Robot_reverse
      Waitms 5
      Gosub Robot_stop
      Waitms 30
   Next K


   Gosub Openservo                                          '     OPEN COLLECTOR FINGERS
   Waitms 200
   Gosub Head_home                                          'PUT HEAD AT HOME POSITION  ALL BACK
   Gosub Read_colors                                        'CHECK IF IT HAS BEEN FOUND
   Vpoint = 0

Gup3:                                                       'GO UP ONE STEP
   Gosub Liftup
   If Vpoint > 100 Then Goto Fail2                          'NOT FOUND'
   Gosub Read_colors
   If Red = 0 Then Goto Gup3
   Redold = Red
Going_up2:
   Gosub Liftup
   If Vpoint > 100 Then Goto Fail2                          'NOT FOUND'
   Gosub Read_colors
   If Red = 0 Then Goto Going_up
   If Redold > Red Then
      Redold = Red
      Goto Going_up2
   End If


   For I = 1 To 10                                          'SOMETHING FOUNT
      Gosub Rightturn                                       'START SCANNING HORIZONTALY
      Gosub Liftup                                          'AND VERTICALY
   Next I

Gup4:
   Gosub Leftturn
   Gosub Read_colors
   If Red = 0 Then Goto Gup4
   Redold = Red
Going_left1:
   Gosub Leftturn
   Gosub Read_colors
   If Red = 0 Then Goto Going_left1
   If Redold > Red Then
      Redold = Red
      Goto Going_left1
   End If

   If Red > 1300 Then Goto Fail2
   Test1 = Green - Red
   If Test1 < 55 Then Goto Fail2
   Test1 = Blue - Red
   If Test1 < 55 Then Goto Fail2

   For I = 1 To 5                                           'FOUND AT LAST
      Gosub Liftup
   Next I



   Point = 0
   Gosub Head_go                                            'HEAD GOING FORWARD
   For K1 = 1 To 500                                        'UNTIL IT REACHES THE TARGET
      Waitms 10
      If Pinb.2 = 0 Then Incr Point
      If Point > 20 Then Goto Getit1
   Next K1
Getit1:
   Gosub Head_stop                                          'GET THE FRUIT
   Waitms 200
   Gosub Closeservo
   Waitms 200
   Gosub Head_home                                          'TURN AT HOME POSITION
   Gosub Head_stop
   Waitms 200
   Pos = 60
   Gosub Vertical_set
   Pos = 80
   Gosub Horizontal_set                                     'RELEASE THE FRUIT IN THE BASKET
   Gosub Openservo
   Waitms 200
Fail2:
   Gosub Closeservo
   Waitms 200

Return


Elexe:                                                      'THE SAME RUTINES FOR GOING FORWARD
   Reset Porte.3                                            'STOP LASER
   For K = 1 To 40                                          'LOCATE TARGET  GOING FORWARD
      Rwheel = Speed
      Lwheel = Speed + 100
      Gosub Robot_forward
      Waitms 5
      Gosub Robot_stop
      Waitms 30
   Next K


   Gosub Openservo
   Waitms 200
   Gosub Head_home

   Gosub Read_colors

   Vpoint = 0

Gup:
   Gosub Liftup
   If Vpoint > 100 Then Goto Fail                           'NOT FOUND'
   Gosub Read_colors
   If Red = 0 Then Goto Gup
   Redold = Red
Going_up:
   Gosub Liftup
   If Vpoint > 100 Then Goto Fail                           'NOT FOUND'
   Gosub Read_colors
   If Red = 0 Then Goto Going_up
   If Redold > Red Then
      Redold = Red
      Goto Going_up
   End If

   For I = 1 To 10
      Gosub Rightturn
      Gosub Liftup
   Next I

Gup1:
   Gosub Leftturn
   Gosub Read_colors
   If Red = 0 Then Goto Gup1

   Redold = Red

Going_left:
   Gosub Leftturn
   Gosub Read_colors
   If Red = 0 Then Goto Going_left
   If Redold > Red Then
      Redold = Red
      Goto Going_left
   End If

   If Red > 1300 Then Goto Fail
   Test1 = Green - Red
   If Test1 < 55 Then Goto Fail
   Test1 = Blue - Red
   If Test1 < 55 Then Goto Fail


   For I = 1 To 5
      Gosub Liftup
   Next I

   Point = 0
   Gosub Head_go
   For K1 = 1 To 450
      Waitms 10
      If Pinb.2 = 0 Then Incr Point
      If Point > 20 Then Goto Getit
   Next K1
Getit:
   'Print K1

   Gosub Head_stop
   Waitms 200
   Gosub Closeservo
   Waitms 200
   Gosub Head_home
   Gosub Head_stop
   Waitms 200
   Pos = 60
   Gosub Vertical_set
   Pos = 85
   Gosub Horizontal_set
   Gosub Openservo
   Waitms 200
Fail:
   Gosub Closeservo
   Waitms 200
   Gosub Horizontal_home
   Gosub Vertical_home

Return


Stop_check:
   Gosub Get_char                                             'CHECKS IF S BUTTON ON BLUETOOTH IS PRESSED UNTIL G IS PRESSED AGAIN
   If B(1) = 83 Then                                          'S STOP BUTTON
      Bit1 = Pinc.3
      Reset Portc.3
      S_c:
         Gosub Get_char                                          'WAIT FOR G GO BUTTON'
         If B(1) <> 71 Then Goto S_c                             'G GO BUTTON


   End If
   Portc.3 = Bit1
Return

Get_char:
   For Thesi = 1 To 20
      B(thesi) = 255
   Next Thesi
   Thesi = 1
   While Thesi < 21

      If Ischarwaiting(#3) = 1 Then                               'was there a char?
         B(thesi) = Inkey(#3)
    'B(thesi) = B(thesi) - 48
         If B(thesi) > 90 Then B(thesi) = 255


     'Print B(thesi)
     'Print #3 , B(thesi)                                    'print it

      End If
      Waitms 10
      Incr Thesi
   Wend
Return

Check_dir:
   Gosub Angles                                             'CHECKS IF THE ROBOT IS RUNNING IN THE CORRECT DIRECTION
   If Angle > Angle1 Then
      Rwheel = 250
      Lwheel = 250
      Gosub Robot_left
      Waitms 15
      Gosub Robot_stop
   End If
   If Angle < Angle1 Then
      Rwheel = 250
      Lwheel = 250
      Gosub Robot_right
      Waitms 15
      Gosub Robot_stop
   End If
    'Print Angle1 ; "    " ; Angle
   If Angle1 <> Angle Then Goto Check_dir
Return

Angles:
'Enable Interrupts
   Put #1 , &H31                                            'CHECKS THE KEYBOARD

   Waitms 20
   For Ii = 1 To 8                                          'AND THE BLUETOOTH
      Comp(ii) = Inkey(#1)
      Comp(ii) = Comp(ii) - 48
 'Print ii ; "  " ; Comp(i)
      Waitms 10
   Next Ii

   Angle = Comp(3) * 100                                    'READ ANGLE FROM COMPASS
   Comp(4) = Comp(4) * 10
   Angle = Angle + Comp(4)
   Angle = Angle + Comp(5)
   Xx = Comp(7) / 10
   Angle = Angle + Xx
   Angle = Round(angle)

'Disable Interrupts
Return

Mismes:
   Gosub Diavase_red
   Gosub Check_red
   If Flag1 > 0 Then Incr Point1                            'MISTAKEN MEASURES'
   If Point1 > 50 Then                                      'SOMETHING WENT WRONG AND WE HAVE TO RESET THE POINTER
      Point1 = 0
      Flag1 = 0
   End If
Return


Diavase_red:

   Pulsein W , Pinc , 4 , 0                                 'READ THE READ SENSOR
   If Length > 500 Then Return
   If W > 2000 Then
      Color2 = Color2 * Length                              'IF IT IS IN THE CORRECT REGION
      Color2 = Color2 + W                                   'MEASURE IT
      Incr Length
      Color2 = Color2 / Length


   End If
 'Print Length
  'Print Color2
Return

Check_red:
   'Print W
   If W > 500 Then
      Temps = Color2 / Factor                               'IF THE RED IS CORRECT SAVE IT
   'Print W ; "   " ; Temp1
      Ws = W
      If Ws < Temps Then Incr Flag1
   End If
Return

Read_colors:
   S2 = 0
   S3 = 0                                                   'READ THE RGB SENSOR
   Pulsein Red , Pinc , 2 , 0
   Print "red  " ; Red ; "  ";
   Waitms 10
   S2 = 1
   S3 = 1
   Pulsein Green , Pinc , 2 , 0
   Print "green  " ; Green ; "  ";
   Waitms 10
   S2 = 0
   S3 = 1
   Pulsein Blue , Pinc , 2 , 0
   Print "blue  " ; Blue ; "  "
   Waitms 10
Return
Robot_reverse:
   Pwm0a = 0 : Pwm0b = Lwheel : Pwm2a = Rwheel : Pwm2b = 0
                                                              'MOVING SUBS
Return

Robot_stop:
   Pwm0a = 0 : Pwm0b = 0 : Pwm2a = 0 : Pwm2b = 0
Return

Robot_forward:
   Pwm0a = Lwheel : Pwm0b = 0 : Pwm2a = 0 : Pwm2b = Rwheel
Return

Robot_right:
   Pwm0a = Lwheel : Pwm0b = Rwheel : Pwm2a = 0 : Pwm2b = 0
Return

Robot_left:
   Pwm0a = 0 : Pwm0b = 0 : Pwm2a = Lwheel : Pwm2b = Rwheel
Return

Vertical_set:
   If Pos > Ypos Then                                       'PUT HEAD IN CORRECT VERTICAL POSITION
      Pos = Pos - Ypos
      For Pos1 = 1 To Pos
         Gosub Liftup
      Next Pos1
   Else
      Pos = Ypos - Pos
      For Pos1 = 1 To Pos
         Gosub Liftdown
      Next Pos1

   End If
Return

Horizontal_home:
   While Pinb.0 = 1                                         'PUT THE HEAD IN THE CORRECT HORIZONTAL POSITION
      Gosub Rightturn
   Wend
   Xpos = 0
   Pos = 10
Horizontal_set:
   If Pos > Xpos Then
      Pos = Pos - Xpos
      For Pos1 = 1 To Pos
         Gosub Leftturn
         'Print Pos1
      Next Pos1
   Else
      Pos = Xpos - Pos
      For Pos1 = 1 To Pos
         Gosub Rightturn
      Next Pos1

   End If
Return

Vertical_home:

   While Pinb.3 = 1                                         'PUT THE HEAD IN THE LOWEST AVAILABLE POSITION
      Gosub Liftdown
   Wend
   Ypos = 0
Return



Head_home:
   For K = 1 To 5                                           'PULL THE HEAD BACK
      Gosub Head_back
      While Pinl.1 = 1
         'Print Pinl.1
         Waitms 20
      Wend
      Gosub Head_stop
   Next K
   Waitms 100
Return


Head_go:
   X = 220                                                  'HEAD RUNNING FORWART TO THE FRUIT
   Set Portl.3
   Waitms 20
   Pwm0a = 0 : Pwm0b = X : Pwm2a = 0 : Pwm2b = 0
Return
Head_back:
   X = 180                                                  'HEAD RETURNING
   Set Portl.3
   Waitms 30
   Pwm0a = 0 : Pwm0b = 0 : Pwm2a = 0 : Pwm2b = X
Return
Head_stop:
   Pwm0a = 0 : Pwm0b = 0 : Pwm2a = 0 : Pwm2b = 0
   Waitms 20
   Reset Portl.3
Return

Closeservo:
   Pulsewidth = 4000                                        'CLOSE FINGERS
   Goto Servo1
Openservo:
   Pulsewidth = 8300                                        'OPEN FINGERS
Servo1:
   For I = 0 To 20                                          'Generate a 20-pulse pulsestream
      Pulseout Portb , 1 , Pulsewidth                       'Control servo
      Waitms 40                                             'Interpulse interval
   Next
Return

Rightturn:

   Set Porta.0                                              'STEPPER MOTOR CONTROLLERS
   Waitms 10
   Reset Porta.0
   Waitms 10
   Set Porta.4
   Waitms 10
   Reset Porta.4
   Waitms 10
   Set Porta.2
   Waitms 10
   Reset Porta.2
   Waitms 10
   Set Porta.6
   Waitms 10
   Reset Porta.6
   Waitms 10
   Decr Xpos
Return

Leftturn:

   Set Porta.6
   Waitms 10
   Reset Porta.6
   Waitms 10
   Set Porta.2
   Waitms 10
   Reset Porta.2
   Waitms 10
   Set Porta.4
   Waitms 10
   Reset Porta.4
   Waitms 10
   Set Porta.0
   Waitms 10
   Reset Porta.0
   Waitms 10
   Incr Xpos
Return

Liftdown:

   Set Porta.1
   Waitms 10
   Reset Porta.1
   Waitms 10
   Set Porta.5
   Waitms 10
   Reset Porta.5
   Waitms 10
   Set Porta.3
   Waitms 10
   Reset Porta.3
   Waitms 10
   Set Porta.7
   Waitms 10
   Reset Porta.7
   Waitms 10
   Decr Ypos
Return

Liftup:

   Incr Vpoint
   Set Porta.7
   Waitms 30
   Reset Porta.7
   Waitms 30
   Set Porta.3
   Waitms 30
   Reset Porta.3
   Waitms 30
   Set Porta.5
   Waitms 30
   Reset Porta.5
   Waitms 30
   Set Porta.1
   Waitms 30
   Reset Porta.1
   Waitms 30
   Incr Ypos
Return