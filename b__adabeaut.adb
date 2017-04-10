pragma Ada_95;
pragma Source_File_Name (ada_main, Spec_File_Name => "b__adabeaut.ads");
pragma Source_File_Name (ada_main, Body_File_Name => "b__adabeaut.adb");
pragma Suppress (Overflow_Check);
with Ada.Exceptions;

package body ada_main is
   pragma Warnings (Off);

   E138 : Short_Integer; pragma Import (Ada, E138, "system__os_lib_E");
   E014 : Short_Integer; pragma Import (Ada, E014, "system__soft_links_E");
   E020 : Short_Integer; pragma Import (Ada, E020, "system__exception_table_E");
   E186 : Short_Integer; pragma Import (Ada, E186, "ada__containers_E");
   E118 : Short_Integer; pragma Import (Ada, E118, "ada__io_exceptions_E");
   E080 : Short_Integer; pragma Import (Ada, E080, "ada__strings_E");
   E082 : Short_Integer; pragma Import (Ada, E082, "ada__strings__maps_E");
   E085 : Short_Integer; pragma Import (Ada, E085, "ada__strings__maps__constants_E");
   E095 : Short_Integer; pragma Import (Ada, E095, "ada__tags_E");
   E117 : Short_Integer; pragma Import (Ada, E117, "ada__streams_E");
   E050 : Short_Integer; pragma Import (Ada, E050, "interfaces__c_E");
   E022 : Short_Integer; pragma Import (Ada, E022, "system__exceptions_E");
   E141 : Short_Integer; pragma Import (Ada, E141, "system__file_control_block_E");
   E199 : Short_Integer; pragma Import (Ada, E199, "ada__streams__stream_io_E");
   E134 : Short_Integer; pragma Import (Ada, E134, "system__file_io_E");
   E120 : Short_Integer; pragma Import (Ada, E120, "system__finalization_root_E");
   E115 : Short_Integer; pragma Import (Ada, E115, "ada__finalization_E");
   E122 : Short_Integer; pragma Import (Ada, E122, "system__storage_pools_E");
   E109 : Short_Integer; pragma Import (Ada, E109, "system__finalization_masters_E");
   E105 : Short_Integer; pragma Import (Ada, E105, "system__storage_pools__subpools_E");
   E048 : Short_Integer; pragma Import (Ada, E048, "ada__calendar_E");
   E060 : Short_Integer; pragma Import (Ada, E060, "ada__calendar__time_zones_E");
   E171 : Short_Integer; pragma Import (Ada, E171, "ada__wide_text_io_E");
   E191 : Short_Integer; pragma Import (Ada, E191, "system__assertions_E");
   E179 : Short_Integer; pragma Import (Ada, E179, "system__pool_global_E");
   E009 : Short_Integer; pragma Import (Ada, E009, "system__secondary_stack_E");
   E093 : Short_Integer; pragma Import (Ada, E093, "ada__strings__unbounded_E");
   E147 : Short_Integer; pragma Import (Ada, E147, "ada__strings__wide_maps_E");
   E046 : Short_Integer; pragma Import (Ada, E046, "ada__directories_E");
   E143 : Short_Integer; pragma Import (Ada, E143, "system__regexp_E");
   E197 : Short_Integer; pragma Import (Ada, E197, "system__strings__stream_ops_E");
   E151 : Short_Integer; pragma Import (Ada, E151, "ada__text_io_E");
   E183 : Short_Integer; pragma Import (Ada, E183, "ada_configuration_E");
   E177 : Short_Integer; pragma Import (Ada, E177, "adabeaut_config_E");

   Local_Priority_Specific_Dispatching : constant String := "";
   Local_Interrupt_States : constant String := "";

   Is_Elaborated : Boolean := False;

   procedure finalize_library is
   begin
      declare
         procedure F1;
         pragma Import (Ada, F1, "adabeaut_config__finalize_body");
      begin
         E177 := E177 - 1;
         F1;
      end;
      declare
         procedure F2;
         pragma Import (Ada, F2, "ada_configuration__finalize_body");
      begin
         E183 := E183 - 1;
         F2;
      end;
      declare
         procedure F3;
         pragma Import (Ada, F3, "ada_configuration__finalize_spec");
      begin
         F3;
      end;
      E151 := E151 - 1;
      declare
         procedure F4;
         pragma Import (Ada, F4, "ada__text_io__finalize_spec");
      begin
         F4;
      end;
      E171 := E171 - 1;
      E046 := E046 - 1;
      E143 := E143 - 1;
      declare
         procedure F5;
         pragma Import (Ada, F5, "system__regexp__finalize_spec");
      begin
         F5;
      end;
      declare
         procedure F6;
         pragma Import (Ada, F6, "ada__directories__finalize_spec");
      begin
         F6;
      end;
      E147 := E147 - 1;
      declare
         procedure F7;
         pragma Import (Ada, F7, "ada__strings__wide_maps__finalize_spec");
      begin
         F7;
      end;
      E093 := E093 - 1;
      declare
         procedure F8;
         pragma Import (Ada, F8, "ada__strings__unbounded__finalize_spec");
      begin
         F8;
      end;
      declare
         procedure F9;
         pragma Import (Ada, F9, "system__file_io__finalize_body");
      begin
         E134 := E134 - 1;
         F9;
      end;
      E109 := E109 - 1;
      E105 := E105 - 1;
      E179 := E179 - 1;
      declare
         procedure F10;
         pragma Import (Ada, F10, "system__pool_global__finalize_spec");
      begin
         F10;
      end;
      declare
         procedure F11;
         pragma Import (Ada, F11, "ada__wide_text_io__finalize_spec");
      begin
         F11;
      end;
      declare
         procedure F12;
         pragma Import (Ada, F12, "system__storage_pools__subpools__finalize_spec");
      begin
         F12;
      end;
      declare
         procedure F13;
         pragma Import (Ada, F13, "system__finalization_masters__finalize_spec");
      begin
         F13;
      end;
      E199 := E199 - 1;
      declare
         procedure F14;
         pragma Import (Ada, F14, "ada__streams__stream_io__finalize_spec");
      begin
         F14;
      end;
      declare
         procedure Reraise_Library_Exception_If_Any;
            pragma Import (Ada, Reraise_Library_Exception_If_Any, "__gnat_reraise_library_exception_if_any");
      begin
         Reraise_Library_Exception_If_Any;
      end;
   end finalize_library;

   procedure adafinal is
      procedure s_stalib_adafinal;
      pragma Import (C, s_stalib_adafinal, "system__standard_library__adafinal");

      procedure Runtime_Finalize;
      pragma Import (C, Runtime_Finalize, "__gnat_runtime_finalize");

   begin
      if not Is_Elaborated then
         return;
      end if;
      Is_Elaborated := False;
      Runtime_Finalize;
      s_stalib_adafinal;
   end adafinal;

   type No_Param_Proc is access procedure;

   procedure adainit is
      Main_Priority : Integer;
      pragma Import (C, Main_Priority, "__gl_main_priority");
      Time_Slice_Value : Integer;
      pragma Import (C, Time_Slice_Value, "__gl_time_slice_val");
      WC_Encoding : Character;
      pragma Import (C, WC_Encoding, "__gl_wc_encoding");
      Locking_Policy : Character;
      pragma Import (C, Locking_Policy, "__gl_locking_policy");
      Queuing_Policy : Character;
      pragma Import (C, Queuing_Policy, "__gl_queuing_policy");
      Task_Dispatching_Policy : Character;
      pragma Import (C, Task_Dispatching_Policy, "__gl_task_dispatching_policy");
      Priority_Specific_Dispatching : System.Address;
      pragma Import (C, Priority_Specific_Dispatching, "__gl_priority_specific_dispatching");
      Num_Specific_Dispatching : Integer;
      pragma Import (C, Num_Specific_Dispatching, "__gl_num_specific_dispatching");
      Main_CPU : Integer;
      pragma Import (C, Main_CPU, "__gl_main_cpu");
      Interrupt_States : System.Address;
      pragma Import (C, Interrupt_States, "__gl_interrupt_states");
      Num_Interrupt_States : Integer;
      pragma Import (C, Num_Interrupt_States, "__gl_num_interrupt_states");
      Unreserve_All_Interrupts : Integer;
      pragma Import (C, Unreserve_All_Interrupts, "__gl_unreserve_all_interrupts");
      Exception_Tracebacks : Integer;
      pragma Import (C, Exception_Tracebacks, "__gl_exception_tracebacks");
      Detect_Blocking : Integer;
      pragma Import (C, Detect_Blocking, "__gl_detect_blocking");
      Default_Stack_Size : Integer;
      pragma Import (C, Default_Stack_Size, "__gl_default_stack_size");
      Leap_Seconds_Support : Integer;
      pragma Import (C, Leap_Seconds_Support, "__gl_leap_seconds_support");

      procedure Runtime_Initialize (Install_Handler : Integer);
      pragma Import (C, Runtime_Initialize, "__gnat_runtime_initialize");

      Finalize_Library_Objects : No_Param_Proc;
      pragma Import (C, Finalize_Library_Objects, "__gnat_finalize_library_objects");
   begin
      if Is_Elaborated then
         return;
      end if;
      Is_Elaborated := True;
      Main_Priority := -1;
      Time_Slice_Value := -1;
      WC_Encoding := 'b';
      Locking_Policy := ' ';
      Queuing_Policy := ' ';
      Task_Dispatching_Policy := ' ';
      Priority_Specific_Dispatching :=
        Local_Priority_Specific_Dispatching'Address;
      Num_Specific_Dispatching := 0;
      Main_CPU := -1;
      Interrupt_States := Local_Interrupt_States'Address;
      Num_Interrupt_States := 0;
      Unreserve_All_Interrupts := 0;
      Exception_Tracebacks := 1;
      Detect_Blocking := 0;
      Default_Stack_Size := -1;
      Leap_Seconds_Support := 0;

      Runtime_Initialize (1);

      Finalize_Library_Objects := finalize_library'access;

      System.Soft_Links'Elab_Spec;
      System.Exception_Table'Elab_Body;
      E020 := E020 + 1;
      Ada.Containers'Elab_Spec;
      E186 := E186 + 1;
      Ada.Io_Exceptions'Elab_Spec;
      E118 := E118 + 1;
      Ada.Strings'Elab_Spec;
      E080 := E080 + 1;
      Ada.Strings.Maps'Elab_Spec;
      Ada.Strings.Maps.Constants'Elab_Spec;
      E085 := E085 + 1;
      Ada.Tags'Elab_Spec;
      Ada.Streams'Elab_Spec;
      E117 := E117 + 1;
      Interfaces.C'Elab_Spec;
      System.Exceptions'Elab_Spec;
      E022 := E022 + 1;
      System.File_Control_Block'Elab_Spec;
      E141 := E141 + 1;
      Ada.Streams.Stream_Io'Elab_Spec;
      E199 := E199 + 1;
      System.Finalization_Root'Elab_Spec;
      E120 := E120 + 1;
      Ada.Finalization'Elab_Spec;
      E115 := E115 + 1;
      System.Storage_Pools'Elab_Spec;
      E122 := E122 + 1;
      System.Finalization_Masters'Elab_Spec;
      System.Storage_Pools.Subpools'Elab_Spec;
      Ada.Calendar'Elab_Spec;
      Ada.Calendar'Elab_Body;
      E048 := E048 + 1;
      Ada.Calendar.Time_Zones'Elab_Spec;
      E060 := E060 + 1;
      Ada.Wide_Text_Io'Elab_Spec;
      System.Assertions'Elab_Spec;
      E191 := E191 + 1;
      System.Pool_Global'Elab_Spec;
      E179 := E179 + 1;
      E105 := E105 + 1;
      System.Finalization_Masters'Elab_Body;
      E109 := E109 + 1;
      System.File_Io'Elab_Body;
      E134 := E134 + 1;
      E050 := E050 + 1;
      Ada.Tags'Elab_Body;
      E095 := E095 + 1;
      E082 := E082 + 1;
      System.Soft_Links'Elab_Body;
      E014 := E014 + 1;
      System.Os_Lib'Elab_Body;
      E138 := E138 + 1;
      System.Secondary_Stack'Elab_Body;
      E009 := E009 + 1;
      Ada.Strings.Unbounded'Elab_Spec;
      E093 := E093 + 1;
      Ada.Strings.Wide_Maps'Elab_Spec;
      E147 := E147 + 1;
      Ada.Directories'Elab_Spec;
      System.Regexp'Elab_Spec;
      E143 := E143 + 1;
      Ada.Directories'Elab_Body;
      E046 := E046 + 1;
      System.Strings.Stream_Ops'Elab_Body;
      E197 := E197 + 1;
      Ada.Wide_Text_Io'Elab_Body;
      E171 := E171 + 1;
      Ada.Text_Io'Elab_Spec;
      Ada.Text_Io'Elab_Body;
      E151 := E151 + 1;
      Ada_Configuration'Elab_Spec;
      Ada_Configuration'Elab_Body;
      E183 := E183 + 1;
      Adabeaut_Config'Elab_Body;
      E177 := E177 + 1;
   end adainit;

   procedure Ada_Main_Program;
   pragma Import (Ada, Ada_Main_Program, "_ada_adabeaut");

   function main
     (argc : Integer;
      argv : System.Address;
      envp : System.Address)
      return Integer
   is
      procedure Initialize (Addr : System.Address);
      pragma Import (C, Initialize, "__gnat_initialize");

      procedure Finalize;
      pragma Import (C, Finalize, "__gnat_finalize");
      SEH : aliased array (1 .. 2) of Integer;

      Ensure_Reference : aliased System.Address := Ada_Main_Program_Name'Address;
      pragma Volatile (Ensure_Reference);

   begin
      gnat_argc := argc;
      gnat_argv := argv;
      gnat_envp := envp;

      Initialize (SEH'Address);
      adainit;
      Ada_Main_Program;
      adafinal;
      Finalize;
      return (gnat_exit_status);
   end;

--  BEGIN Object file/option list
   --   C:\Users\Nick\Documents\Devel\revtang\src\ada_configuration.o
   --   C:\Users\Nick\Documents\Devel\revtang\src\adabeaut_config.o
   --   C:\Users\Nick\Documents\Devel\revtang\src\adabeaut.o
   --   -LC:\Users\Nick\Documents\Devel\revtang\src\
   --   -LC:\Users\Nick\Documents\Devel\revtang\src\
   --   -LC:\GNAT\2010\lib\win32ada\relocatable\
   --   -LC:/gnat/2015/lib/gcc/i686-pc-mingw32/4.9.3/adalib/
   --   -static
   --   -lgnat
   --   -Wl,--stack=0x2000000
--  END Object file/option list   

end ada_main;
