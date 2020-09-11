

with Ada.Real_Time; use Ada.Real_Time;

package devices is

    ---------------------------------------------------------------------
    ------ INPUT devices interface
    ---------------------------------------------------------------------

    ---------------------------------------------------------------------
    ------ ELECTRODES --------------------------------------------------- 

    type Value_Electrode is new natural range 0..10;
    Number_Electrodes: constant integer := 10;

    type EEG_Samples_Index is new natural range 1..Number_Electrodes;
    type EEG_Samples_Type is array (EEG_Samples_Index) of Value_Electrode;


    procedure Reading_Sensors (L: out EEG_Samples_Type);
    -- It reads a sample of Electrode Sensors and returns a array of 10 values 

    ---------------------------------------------------------------------
    ------ EYES ---------------------------------------------------------

    type Eyes_Samples_Index is (left,right);
    type Eyes_Samples_Values is new natural range 0..100;
    type Eyes_Samples_Type is array (Eyes_Samples_Index) of Eyes_Samples_Values;

    procedure Reading_EyesImage (L: out Eyes_Samples_Type);
    -- It reads an image of the eyes, analyses the image and returns 
    --- the percentage of aperture (0..100) of every eye (left, right)


    ---------------------------------------------------------------------
    ------ OUTPUT devices interface  
    ---------------------------------------------------------------------

    type Values_Pulse_Rate is new float range 20.0..300.0;

    procedure Display_Pulse_Rate (P: Values_Pulse_Rate);
    -- It displays the pulse rate P

    ---------------------------------------------------------------------
    procedure Display_Electrodes_Sample (R: EEG_Samples_Type);
    -- It displays the 10 values of the electrodes sample 

    --------------------------------------------------------------------
    procedure Display_Eyes_Sample (R: Eyes_Samples_Type);
    -- It displays the values of eyes aperture (left and right) 

    ---------------------------------------------------------------------
    type Light_States is (On, Off);
    procedure Light (E: Light_States);
    -- It turns ON/OFF the light 

    ---------------------------------------------------------------------
    procedure Display_Cronometro (Origen: Ada.Real_Time.Time; Hora: Ada.Real_Time.Time);
    -- It displays a chronometer 

    ---------------------------------------------------------------------
    Type Volume is new integer range 1..5; 
    procedure Beep (v: Volume); 
    -- It beeps with a volume "v" 

     ---------------------------------------------------------------------
    procedure Activate_Automatic_Driving;
    -- It activates the automatic driving system 



end devices;



