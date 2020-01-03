with Ada.Text_IO;
with Ada.Wide_Text_IO;
with Ada.IO_Exceptions;
with Ada.Exceptions;

with Ada.Strings.Wide_Fixed;
with Ada.Strings.Unbounded;
with Ada.Characters.Conversions;

with Ada.Directories;
with Adabeaut_Config;

use Ada.Wide_Text_IO;
use Ada.Directories;
use Ada.Strings.Wide_Fixed;
use Ada.Strings.Unbounded;
use Ada.Characters.Conversions;

use Adabeaut_Config;

--/ Main adabeaut procedure:

procedure adabeaut is

   --/ Exceptions:
 
   Source_Line_Too_Long : exception;
   Fragment_Number_Error: exception;
 
   --\
   --/ Warning messages:
 
   procedure Put_Warning (Message : in String) is
   begin
      Ada.Text_IO.Put_Line ( Message );
   end;
 
   --\
   --/ Fragment identifiers:
 
   type Fragment_Number is new Natural;
 
   --| Abbreviation HFN for Highest Fragment Number.

   HFN_File_Basename  : constant String := "fragnum";
   HFN_File_Extension : constant String := "txt";
   HFN_File_Path      : Unbounded_String;
 
   HFN_Loaded         : Boolean := False;
   HFN_Changed        : Boolean := False;
   HFN_Value          : Fragment_Number;

   package Fragment_Number_IO is new Ada.Text_IO.Integer_IO (Fragment_Number);
   package Wide_Fragment_Number_IO is new Ada.Wide_Text_IO.Integer_IO (Fragment_Number);
 
   procedure Load_HFN (Source_File_Path : String) is
 
      HFN_Dir  : Unbounded_String;
      HFN_File : Ada.Text_IO.File_Type;
   begin
      if HFN_Loaded then
         raise Fragment_Number_Error with "Cannot load Highest Fragment Number as it is already loaded";
      end if;
      Set_Unbounded_String (HFN_Dir, Containing_Directory (Source_File_Path));
      loop
         Set_Unbounded_String (HFN_File_Path, Compose (To_String (HFN_Dir),
                                                       HFN_File_Basename,
                                                       HFN_File_Extension));
         exit when Exists (To_String (HFN_File_Path));
         begin
            Set_Unbounded_String (HFN_Dir, Containing_Directory (To_String (HFN_Dir))); -- proceed to next directory level up
         exception
            when Ada.IO_Exceptions.Use_Error =>
               raise Fragment_Number_Error with "Highest Fragment Number file ['" & Simple_Name (To_String (HFN_File_Path)) & "'] could not be found";
         end;
      end loop;
      begin
         Ada.Text_IO.Open (HFN_File, Ada.Text_IO.In_File, To_String (HFN_File_Path));
         Fragment_Number_IO.Get (HFN_File, HFN_Value);
         Ada.Text_IO.Close (HFN_File);
      exception
         when Ada.IO_Exceptions.Name_Error | Ada.IO_Exceptions.Use_Error | Ada.IO_Exceptions.End_Error | Ada.IO_Exceptions.Data_Error =>
            raise Fragment_Number_Error with "Highest Fragment Number file ['" & To_String (HFN_File_Path) & "']: Could not open or read a valid value";
      end;
      HFN_Loaded := True;
      HFN_Changed := False;
   end;
 
   procedure Save_Highest_Frag_Id_If_Necessary is
 
      HFN_File : Ada.Text_Io.File_Type;
   begin
      if HFN_Loaded and HFN_Changed then
         Ada.Text_IO.Open (HFN_File, Ada.Text_IO.Out_File, To_String (HFN_File_Path));
         Fragment_Number_IO.Put (HFN_File, HFN_Value, Width => 0); -- no leading spaces
         Ada.Text_IO.Close (HFN_File);
      end if;
      HFN_Loaded := False;
      HFN_Changed := False;
   exception
      when Ada.IO_Exceptions.Name_Error | Ada.IO_Exceptions.Use_Error | Ada.IO_Exceptions.End_Error | Ada.IO_Exceptions.Data_Error =>
         raise Fragment_Number_Error with "Highest Fragment Number file ['" & To_String (HFN_File_Path) & "']: Could not open or write";
   end;
   
   procedure Increment_HFN is
   begin
      if not HFN_Loaded then 
         raise Fragment_Number_Error with "Cannot increment Highest Fragment Number as it is not loaded";
      end if;            
      HFN_Value := HFN_Value + 1;
      HFN_Changed := True;      
   end;
   
   function Highest_Fragment_Number return Fragment_Number is
   begin
      return HFN_Value;
   end;
       
   --\
   --/ Utility functions:
 
   function Is_All (Str : in Wide_String; Chr : in Wide_Character) return Boolean is
   begin
      for i in Str'Range loop
         if Str (i) /= Chr then return False; end if;
      end loop;
      return True;
   end;
 
   --\
   --/ Processing a source file:
 
   procedure Process_Source_File (Source_Name : in String) is

      Source_File : Ada.Wide_Text_IO.File_Type;
      Temp_File   : Ada.Wide_Text_IO.File_Type;
 
      Temp_Name   : constant String := Compose (Containing_Directory (Source_Name),
                                                Base_Name (Source_Name),
                                                "bak");
 
      IT          : Wide_String (1 .. Max_Line_Width);
      IL          : Natural range 0 .. IT'Last;
      I1          : Natural;
      Frag_Level  : Natural := 0;
 
      function Annotate_File_Name return String is
      begin
         return " [in file '" & Name (Source_File) & "']";
      end;
 
      function Annotate_File_Location return String is
      begin
         return " [at line" & Positive_Count'Image (Line) & " in file '" & Name (Source_File) & "']";
      end;
 
      function Line_Starts_With (Prefix : Wide_String; From : Positive) return Boolean is
      begin
         return From + Prefix'Length - 1 <= IL
            and then IT (From .. From + Prefix'Length - 1) = Prefix;
      end;

      function Line_Ends_With (Suffix : Wide_String; From : Positive) return Boolean is
      begin
         return From + Suffix'Length - 1 <= IL
            and then IT (IL - (Suffix'Length - 1) .. IL) = Suffix;
      end;

      function Get_Next_Frag_Id return Fragment_Number is
      begin
         if not HFN_Loaded then Load_HFN (Name (Source_File)); end if;
         Increment_HFN;
         return Highest_Fragment_Number;
      end;
      
      --/ Reformatting prose comments:
 
      procedure Reformat_Prose_Comment is

         PT                  : Wide_String (1 .. Max_Line_Width);
         PL, WL              : Natural range 0 .. IT'Last;
         I2, O1              : Natural;
         Empty_Input_Line    : Boolean;
         Initial_Output_Line : Boolean;
      begin
         PL := I1 + Prose_Comment_Prefix'Length - 1;
         PT (1 .. PL) := IT (1 .. PL); -- remember the actual prefix, including leading spaces
         I1 := PL + 1; -- then skip the prefix
         O1 := Right_Margin + 1; -- to force initial output of prefix
         Empty_Input_Line := False;
         Initial_Output_Line := True;
         loop
            while I1 <= IL and then IT (I1) = ' ' loop I1 := I1 + 1; end loop; -- skip blanks
            if I1 <= IL then -- if not there were only blanks at the end of the line
               Empty_Input_Line := False;
               I2 := I1;
               while I2 <= IL and then IT (I2) /= ' ' loop I2 := I2 + 1; end loop; -- search to 1 past end of word
               WL := I2 - I1; -- compute word length
               if O1 + WL > Right_Margin then -- if word won't fit on current output line
                  if Initial_Output_Line then Initial_Output_Line := False; else New_Line (Temp_File); end if; -- start new line if necessary
                  Put ( Temp_File, PT (1 .. PL) ); O1 := PL + 1; -- output prefix
               end if;
               Put (Temp_File, ' '); Put (Temp_File, IT (I1 .. I2 - 1)); O1 := O1 + 1 + WL; -- output a space and the word
               I1 := I2 + 1;
            else -- if end of line (or there were only blanks at the end of the line)
               exit when Empty_Input_Line;
               Get_Line (Source_File, IT, IL); -- get next input line
               if PL <= IL and then IT (1 .. PL) = PT (1 .. PL) then -- another prefixed line?
                  I1 := PL + 1; Empty_Input_Line := False; -- skip it
               else
                  I1 := 1; Empty_Input_Line := True; -- it might be an empty line
               end if;
            end if;
         end loop;
         New_Line (Temp_File, 2); -- terminate current output line and output a blank line
      end;
 
      --\
      --/ Handle fragment start and end:
 
      procedure Process_Fragment_Start is
      begin
         Frag_Level := Frag_Level + 1;
         Put_Line (Temp_File, IT (1 .. IL)); -- copy the line
      end;
 
      procedure Process_Fragment_End is
 
         type Parsing_Status is
            (Unparsed, Missing, Malformed, Unrecognised, Bad_Number, Good);
 
         I2, I3, I4 : Natural;
         Frag_Num_Last : Natural;
         Frag_Num   : Fragment_Number;
         Status     : Parsing_Status := Unparsed;
         -- Warn: Boolean := False;
      begin
         --PL := I1 + Prose_Comment_Prefix'Length - 1;
         --PT (1 .. PL) := IT (1 .. PL); -- remember the actual prefix, including leading spaces
         --I1 := PL + 1; -- then skip the prefix
         I1 := I1 + Prose_Comment_Prefix'Length; -- skip the prefix
         while I1 <= IL and then IT (I1) = ' ' loop I1 := I1 + 1; end loop; -- skip blanks
         if I1 > IL then Status := Missing; goto Needs_To_Be_Rewritten; end if; -- no id at all
         if not Line_Starts_With (Fragment_Tag_Prefix, I1)
            or else not Line_Ends_With (Fragment_Tag_Suffix, I1) then -- malformed id
            Status := Malformed; goto Needs_To_Be_Rewritten;
         end if;
         I2 := I1 + Fragment_Tag_Prefix'Length; -- start of fragment id
         if not Line_Starts_With (Fragment_Id_Prefix, I2) then Status := Unrecognised; goto Can_Stay_The_Same; end if;
         I3 := I2 + Fragment_Id_Prefix'Length; -- start of fragment number
         I4 := IL - Fragment_Tag_Suffix'Length + 1; -- just past end of fragment id
         if I3 > I4 then Status := Bad_Number; goto Needs_To_Be_Rewritten; end if; -- number part completely empty
         begin
            Wide_Fragment_Number_IO.Get ( IT (I3 .. I4 - 1), Frag_Num, Frag_Num_Last );
         exception
            when Ada.IO_Exceptions.Data_Error => Status := Bad_Number; goto Needs_To_Be_Rewritten;
         end;
         Status := Good; goto Can_Stay_The_Same;
      <<Needs_To_Be_Rewritten>>
         case Status is
            when Missing    => null; -- no warning if it is just missing
            when Malformed  => Put_Warning ("Fragment identifier '" & To_String (IT (I1 .. IL)) & "' malformed: replaced" & Annotate_File_Location);
            when Bad_Number => Put_Warning ("Fragment identifier numeric part '" & To_String (IT (I3 .. I4 - 1)) & "' faulty: replaced" & Annotate_File_Location);
            when others     => raise Program_Error; -- not expected at this point
         end case;
         Put (Temp_File, IT (1 .. I1 - 1)); -- copy prefix
         Put (Temp_File, ' '); -- one space
         Put (Temp_File, Fragment_Tag_Prefix);
         Put (Temp_File, Fragment_Id_Prefix);
         Wide_Fragment_Number_IO.Put (Temp_File, Get_Next_Frag_Id, 0); -- no leading space
         Put (Temp_File, Fragment_Tag_Suffix);
         New_Line (Temp_File);
         goto Check_Fragment_Level;
      <<Can_Stay_The_Same>>
         Put_Line (Temp_File, IT (1 .. IL)); -- copy the line
      <<Check_Fragment_Level>>
         if Frag_Level = 0 then
            Put_Warning ("Unexpected fragment terminator" & Annotate_File_Location);
         else
            Frag_Level := Frag_Level - 1;
         end if;
      end;
 
      --| Check for fragment identifier: if not there insert one; check for fragment level.

      procedure Check_And_Report_Fragment_Level_Error is
      begin
         if Frag_Level = 1 then
            Put_Warning ("Missing fragment terminator" & Annotate_File_Name);
         elsif Frag_Level > 1 then
            Put_Warning (Natural'Image (Frag_Level) & " missing fragment terminators" & Annotate_File_Name);
         end if;
      end;
 
      --\
 
   begin
      Open (Source_File, In_File, Source_Name);
      Create (Temp_File, Out_File, Temp_Name);

      while not End_Of_File (Source_File) loop
         Get_Line (Source_File, IT, IL);
         if IL = IT'Last then raise Source_Line_Too_Long; end if;
         while IL > 1 and then IT (IL) = ' ' loop IL := IL - 1; end loop; -- remove trailing spaces
         I1 := 1; while I1 <= IL and then IT (I1) = ' ' loop I1 := I1 + 1; end loop; -- skip leading spaces
         if Line_Starts_With (Prose_Comment_Prefix, I1) then
            Reformat_Prose_Comment;
         elsif Line_Starts_With (Fragment_Start_Prefix, I1) then
            Process_Fragment_Start;
         elsif Line_Starts_With (Fragment_End_Prefix, I1) then
            Process_Fragment_End;
         elsif I1 + 4 <= IL and then Is_All (IT (I1 .. IL), '-') and then I1 + 4 <= Right_Margin then
            Put (Temp_File, IT (1 .. I1 - 1)); Put_Line (Temp_File, (Right_Margin - I1 + 1) * '-'); -- line across
         else
            Put_Line (Temp_File, IT (1 .. IL)); -- just copy the line
         end if;
      end loop;
      New_Line (Temp_File); -- to counter a putative GNAT bug
 
      Close (Temp_File);
      Close (Source_File);
 
      Delete_File (Source_Name);
      Rename (Old_Name => Temp_Name, New_Name => Source_Name);
      
      Check_And_Report_Fragment_Level_Error;

   end Process_Source_File;
 
   --\
begin
   --/ Main program:
   if Help_Requested then return; end if;
   for Name of Source_File_Names.all loop
      Process_Source_File (To_String (Name)); -- loads highest frag id on demand
   end loop;
   Save_Highest_Frag_Id_If_Necessary;
   --\
exception
   when E: others =>
      Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, Ada.Exceptions.Exception_Information (E));
end adabeaut;

--\
