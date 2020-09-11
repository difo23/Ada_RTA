
with Kernel.Serial_Output; use Kernel.Serial_Output;
with Ada.Real_Time; use Ada.Real_Time;

with System; use System;
with Ada.Interrupts.Names;

with Tools; use Tools;

with Devices; use Devices;
--with Pulse_Interrupt; use Pulse_Interrupt;


package body add is

    -------------------------------------------------------------------------
    -- Periodos, tiempos de ejecucion y prioridades de las tareas
    -------------------------------------------------------------------------

    Period_Electrodes: constant Time_Span := To_Time_Span (0.3); -- org. 0.2
    Period_Eyes_Detection: constant Time_Span := To_Time_Span (0.15);
    Period_Drowsiness_Control: constant Time_Span := To_Time_Span (0.75);  -- org. 0.25
    Period_Display: constant Time_Span := To_Time_Span(1.0); 
    Period_Risk_Control: constant Time_Span := To_Time_Span(0.5); 

    Priority_Electrodes: constant System.Priority := 15;
    Priority_Eyes_Detection: constant System.Priority := 20;
    Priority_Drowsiness_Control: constant System.Priority := 22;
    Priority_Display: constant System.Priority := 5;
    Priority_Risk_Control: constant System.Priority := 10;
    Priority_Ceiling_OP_Eyes: constant System.Priority := 24;
    Priority_Ceiling_OP_EEG: constant System.Priority := 24;


    Priority_Pulse: constant System.Priority := 24;
    Priority_Of_External_Interrupts_2 : constant System.Interrupt_Priority
                                       := System.Interrupt_Priority'First + 9;






   -- Declaración Objeto Protegido EYES

	Protected Eyes_States is
		pragma priority(Priority_Ceiling_OP_Eyes);
  		function Read return Eyes_Samples_Type ;
 		procedure Write (entrada : Eyes_Samples_Type) ;

	private 
 		Datos : Eyes_Samples_Type;
	end Eyes_States ;

	-- Declaración Objeto Protegido EGG

	Protected EEG_States is
		pragma priority(Priority_Ceiling_OP_EEG);
  		function Read return EEG_Samples_Type ;
 		procedure Write (entrada : EEG_Samples_Type) ;

	private 
 		Datos : EEG_Samples_Type;
	end EEG_States ;

    -----------------------------------------------------------------------
    ------------- declaracion de las tareas
    -----------------------------------------------------------------------


    task Electrodes is
      pragma priority(Priority_Electrodes);
    end Electrodes;

    task Eyes_Detection is
      pragma priority(Priority_Eyes_Detection);
    end Eyes_Detection;

    task Display is
      pragma priority(Priority_Display);
    end Display;

    task Risk_Control is
      pragma priority(Priority_Risk_Control);
    end Risk_Control;

    ----------------------------------------------------------------------

    ----------------------------------------------------------------------
    ------------- procedimiento exportado
    ----------------------------------------------------------------------
    procedure Background is
    begin
      loop
        null;
      end loop;
    end Background;
    ----------------------------------------------------------------------


---------------------------------------------------------------------
--         TAREAS                                                  --
---------------------------------------------------------------------

---------------------------------------------------------------------
    task body Electrodes  is  --------------------------- Tarea B
        siguiente: Time := Big_Bang;
        x: integer;
        R: EEG_Samples_Type;
	R_1: EEG_Samples_Type;
        e: Value_Electrode := 1;
    begin
      loop

         Starting_Notice ("Electrodes"); 

         Reading_Sensors (R);
	 
	 EEG_States.Write(R);
       --  R_1 := EEG_States.Read;

       --  Display_Electrodes_Sample (R_1);
	
	

         Finishing_Notice ("Electrodes");

         siguiente := siguiente + Period_Electrodes;
         delay until siguiente;
      end loop;
    end Electrodes;


---------------------------------------------------------------------
    task body Eyes_Detection  is  --------------------------- Tarea B
        siguiente: Time := Big_Bang;
        x: integer;
        Current_R: Eyes_Samples_Type :=   (99,99);
        R1_previous: Eyes_Samples_Type := (99,99);
        R2_previous: Eyes_Samples_Type := (99,99);
	R3_previous: Eyes_Samples_Type := (99,99);
	
  

        Current_Ap, Ap1, Ap2: Eyes_Samples_Values := 99; 

    begin
      loop
         Starting_Notice ("Eyes_Detection");

         R2_previous := R1_previous;
         R1_previous := Current_R;

         Reading_EyesImage (Current_R);
	 
	 Eyes_States.Write(Current_R);
       --  R3_previous := Eyes_States.Read;

       --  Display_Eyes_Sample (R3_previous);

         Ap2 := Ap1;
         Ap1 := Current_Ap;
         Current_Ap := 0;

    
         Finishing_Notice ("Eyes_Detection");
 
         siguiente := siguiente + Period_Eyes_Detection;
         delay until siguiente;
      end loop;
    end Eyes_Detection;


---------------------------------------------------------------------
    task body Display is  --------------------------- Tarea C
        siguiente: Time := Big_Bang;
        x: integer;
        Current_Eyes_R: Eyes_Samples_Type :=   (99,99);
	R_1: EEG_Samples_Type;

    begin
      loop
         Starting_Notice ("Start Display Info");

         Current_Eyes_R := Eyes_States.Read;
         Display_Eyes_Sample (Current_Eyes_R);
	 
	 R_1 := EEG_States.Read;
 	 Display_Electrodes_Sample (R_1);

         Finishing_Notice ("End Display Info");
 
         siguiente := siguiente + Period_Display;
         delay until siguiente;
      end loop;
    end Display;


---------------------------------------------------------------------
    task body Risk_Control is  --------------------------- Tarea D
        siguiente: Time := Big_Bang;
        x: integer;
        Eyes_R: Eyes_Samples_Type :=   (99,99);
	EEG_R: EEG_Samples_Type;

	Eyes_C: Eyes_Samples_Type :=   (10,10);
        EEG_L: integer;

    begin
      loop
         Starting_Notice ("Start Risk Control");

         Eyes_R := Eyes_States.Read;
	 EEG_R := EEG_States.Read;
         
         EEG_L := integer(EEG_R(6)) + integer(EEG_R(7)) + integer(EEG_R(8)) + integer(EEG_R(9));

	 -- Estado de los Ojos:
         if Eyes_R <= Eyes_C OR EEG_L < 20 then
		if Eyes_R  <= Eyes_C then
		Beep(2);
		end if;
		if EEG_L < 20 then
		Beep(1);
		Light(On);
		end if;
			if Eyes_R <= Eyes_C AND EEG_L < 20 then
			Beep(3);
			Light(On);
			end if;				
         end if;


         Finishing_Notice ("End Risk control");
 
         siguiente := siguiente + Period_Risk_Control;
         delay until siguiente;
      end loop;
    end Risk_Control;


-- Cuerpo Eyes_States

Protected body Eyes_States is

function Read return Eyes_Samples_Type is
begin
  return Datos ;
end Read ;

procedure Write(entrada : Eyes_Samples_Type) is
begin
  Datos := entrada;
end Write ;

end Eyes_States ;


-- Cuerpo EEG

Protected body EEG_States is

function Read return EEG_Samples_Type is
begin
  return Datos ;
end Read ;

procedure Write(entrada : EEG_Samples_Type) is
begin
  Datos := entrada;
end Write ;

end EEG_States ;


begin
   null;
end add;



