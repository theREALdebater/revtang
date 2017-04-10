package body Revtang_Config is

   use Ada_Configuration;

   Info   : Help_Information;
   Config : Configuration_Data;

   Nongenerate_Mode_Config_Name : constant String := "Check_Only";

begin
   Insert_Option_Boolean
      (Info               => Info,
       Config_Name        => Nongenerate_Mode_Config_Name,
       Long_Name          => "check",
       Short_Name         => 'c',
       Description        => "Does not output any files");

   Insert_Standard_Options (Info);

   Set_Program_Info
      (Info                 => Info,
       Program_Description  => "Generates fragment files for a subtree of Ada source text files",
       Argument_Metaname    => "dir",
       Argument_Description => "Name or path of subroot directory containing Ada source text file to process" );

   Set_Help_Information (Config, Info);

   Load_Configuration (Config);

   Nongenerate_Mode := Get_Boolean (Config, Program_Options_Config_Name_Prefix & Nongenerate_Mode, Nongenerate_Mode);

   declare
      Root_Names : constant String_Array := Get_String_Array (Config, Program_Arguments_Config_Name, Null_String_Array); -- normal arguments only
   begin
      Source_File_Root_Names := new String_Array (1 .. Root_Names'Length);
      Source_File_Root_Names.all := Root_Names;
   end;

   Help_Requested := Ada_Configuration.Help_Requested (Config);
end;
