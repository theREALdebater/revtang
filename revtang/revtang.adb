with Ada.Text_IO;
with Ada.Directories;
with Ada.Containers.Doubly_Linked_Lists;

with Ada.Wide_Text_IO.Unbounded_IO;
with Ada.Strings.Wide_Unbounded;

use Ada.Text_IO;
use Ada.Directories;

use Ada.Wide_Text_IO.Unbounded_IO;
use Ada.Strings.Wide_Unbounded;

--/ Main revtang procedure:

procedure revtang is

   --/ Text lines:
   
   type Text_Line is
      record
         Text:   Wide_Unbounded_String := Null_Wide_Unbounded_String;
         Indent: Natural := 0; -- individual indent of this line
      end record;
      
   --| A `Text_Line` contains the text of the line in `Text`, but with all leading and trailing space removed.
   --| The `Indent` is the (physical) number of leading spaces removed.
   
   function Is_Empty (Line: in Text_Line) return Boolean is
      return Line.Text.Length = 0;
   
   --\
   --/ Handling of lists of text lines:
   
   package Line_Lists is new Ada.Containers.Doubly_Linked_Lists (Text_Line);
   
   --\
   --/ Text blocks:
   
   type Text_Block (Length: Natural) is
      record
         Lines:  array (1..Length) of Text_Line;
         Indent: Natural := 0; -- overall indent of the the whole block
      end record;
      
   --\
   --/ Remove character spacing that occurs in front of every line in a text block:
   
   procedure Normalize_Indent (Block: in out Text_Block) is
       
      Min_Indent: Natural := Natural'Last;
      Is_Moot:    Boolean := True; -- True if there are no non-empty lines in the block
      Indent:     Natural;
    begin
    
      -- First we scan all the lines to work out the common indentation:
      for Line of Block.Lines loop
         if not Is_Empty (Line) then
            Is_Moot := False; -- now we know there is at least one non-empty line in the block
            Indent := Line.Indent;
            if Indent < Min_Indent then
               Min_Indent := Indent;
               if Min_Indent = 0 then
                  return; -- early exit if we get down to 0, since it cannot possibly get less
               end if;
            end if;
         end if;
      end loop;
      
      if Is_Moot then return; end if; -- we don't do anything if there are no non-empty lines in the block
      
      -- We have a non-zero indentation, so first we adjust the current block indentation:
      Block.Indent := Block.Indent + Min_Indent;
      
      -- Then we adjust every line by reducing its indentation by the same amount:
      for Line of Block.Lines loop
         Line.Indent := Line.Indent - Min_Indent;
      end loop;      
   end;
   
   --\
   --/ Convert a text list into a text block:
      
   function To_Text_Block (List: in Line_Lists.List) return Text_Block is
                      
      Top_Trim:    Natural := 0;
      Bottom_Trim: Natural := 0;
   begin
      -- First, count how many empty lines at beginning to omit:
      for Line of List loop
         exit when not Is_Empty (Line);
         Top_Trim := Top_Trim + 1;
      end loop;
      
      -- Then count how many empty lines at end to omit:
      if Top_Trim < Length (List) then -- don't bother if all the lines are empty
         for Line of List reverse loop
            exit when not Is_Empty (Line);
            Bottom_Trim := Bottom_Trim + 1;
         end loop;
      end if;
         
      -- Create a block of the appropriate length and copy lines across (except the trimmed lines):
      declare
         Total_Trim: constant Natural := Top_Trim + Bottom_Trim;
         Block:      Text_Block (Length (List) - Total_Trim);
         List_Index: Natural := 0;
         Line_Num:   Natural := 0;
      begin
         for Line of List loop
            List_Index := List_Index + 1;
            if List_Index > Top_Trim then
               Line_Num := Line_Num + 1;
               Block.Lines (Line_Num) := Line;
               exit when Line_Num = Block.Length;
            end if;
         end loop;
         Block.Indent := 0;
         return Block;
      end;
   end;
   
   --\
   --/ How many spaces at the beginning of a line of text?
   
   procedure Compute_Line_Indent (Text:       in  Wide_Unbounded_String;
                                  Left_Trim:  out Natural;
                                  Indent:     out Natural;
                                  Right_Trim: out Natural) is
   
      N1: Natural := 0; -- number of characters at the beginning of the line we have already scanned past
      N2: Natural := 0; -- number of physical spaces there are at the beginning of the line
      N3: Natural := 0; -- number of characters to be trimmed from the end of the line
      C:  Wide_Character;
   begin
      while N1 < Length (Text) loop
         C := Element (Text, N1 + 1);
         if C = ' ' then
            N2 := N2 + 1;
         elsif C = Latin_1.HT then
            N2 := N2 + ((N2 - 7) mod 8) + 1;
         else
            exit;
         end if;
         N1 := N1 + 1;
      end loop;
      while N3 < Length (Text) - N1 loop
         C := Element (Text, Length (Text) - N3);
         exit when C /= ' ' and C /= Latin_1.HT;
         N3 := N3 + 1;
      end loop;
      Left_Trim := N1;
      Indent := N2;
      Right_Trim := N3;
   end;
   
   --\
   --/ Read an entire text file into a text list:
   
   procedure Load (File_Name: in  String;
                   List:      out Line_Lists.List) is

      File:       Wide_Text_IO.File_Type;
      Text:       Wide_Unbounded_String;
      Left_Trim:  Natural;
      Right_Trim: Natural;
      Indent:     Natural;
      Line:       Text_Line;
   begin
      List.Clear;
      Open (File, In_File, File_Name);
      while not End_Of_File (File) loop
         Get_Line (File, Text);
         Compute_Line_Indent (Text, Left_Trim, Indent, Right_Trim);
         Unbounded_Slice (Text, Line.Text, Left_Trim + 1, Length (Text) - Right_Trim);
         Line.Indent := Indent;
         List.Append (Line);
      end loop;
      Close (File);
   end;
   
   --\
   --/ :
   
   procedure Process_Fragment (Frag_Body:        in Text_Block; 
                               Title_Chain:      in Fragment_Title_Chain; 
                               ...
                               Generate_DocBook: in Boolean) is
                               
      DocBook_File:  Text_IO.File_Type;
      Line:          Text_Line;
      Subfrag_Level: Natural;
      SubFrag:       Line_Lists.List;
      Subfrag_Title: Unbounded_Wide_String;
   begin
      if Generate_HTML then
         Open( DocBook_File, In_File, Compose( Containing_Directory(Frag_Tag), Frag_Tag, "xml" ) );
         Put_Line( DocBook_File, "<figure role=""program-fragment"" id="" & Frag_Tag & "">");
         Put_Line( DocBook_File, "<title>" & Title & "</title>");
         Put_Line( DocBook_File, "<subtitle>" & To_DocBook(Title_Chain) & "</subtitle>");
         --Put_Line( DocBook_File, "");
         --Put_Line( DocBook_File, "");
      end if;
      for Line_Num in Natural range 1 .. Frag_Body.Length loop
         Line := Frag_Body.Lines(Line_Num);
         if Is_Fragment_Start(Line.Text) then
            Subfrag_Title := Extract_Fragment_Title(Line.Text);
            Frag_Level := 0;
            Subfrag.Clear;
            loop
               
            end loop;
            ...
            if Generate_DocBook then
            
               Put_Line( DocBook_File, );
            end if;
         else
            ...
            if Generate_DocBook then
            
               Put_Line( DocBook_File, );
            end if;
            if Generate_DocBook then
            
               Put_Line( DocBook_File, );
            end if;
            if Generate_DocBook then
            
               Put_Line( DocBook_File, );
               Put_Line( DocBook_File, );
               Put_Line( DocBook_File, );
            end if;

         end if;
         ...
      end loop;
      ...
      if Generate_DocBook then
         Put_Line( DocBook_File, );
      end if;
      if Generate_DocBook then
         Put_Line( DocBook_File, );
         Put_Line( DocBook_File, );
         Put_Line( DocBook_File, );
      end if;
      if Generate_DocBook then
      
         Put_Line( DocBook_File, );
      end if;
   end;
   
   --\
   
   function Is_Fragment_Start (Line: Text_Line) return Boolean is
      return Line_Starts_With (Line.Text, Fragment_Start_Prefix)
   
   function Is_Fragment_End (Line: Text_Line) return Boolean is
      return Line_Starts_With (Line.Text, Fragment_End_Prefix)
   
   function Line_Starts_With (Line:   Wide_Unbounded_String; 
                              Prefix: Wide_String; 
                              From :  Positive := 1) return Boolean is
   begin
      return From + Prefix'Length - 1 <= Line.Length
         and then Slice (Line, From .. From + Prefix'Length - 1) = Prefix;
   end;

   function Extract_Fragment_Title (Line: Unbounded_Wide_String) return Unbounded_Wide_String is
   
      Result: Unbounded_Wide_String;
   begin
      
   
   
   end;
   
   function To_DocBook (Chain: Fragment_Title_Chain) return Wide_String is
      
      Result: Unbounded_Wide_String;
      Is_First: Boolean := True;
   begin
      for Title: Unbounded_Wide_String in Chain.List loop
         if Is_First then
            Is_First := False;
         else
            Append( Result, " &#8594; " ); -- right arrow
         end if;
         Append( Result, XHTML_Safe(Title) );
         
   
   
   
   end;
   
   --\
   --/ Title chains:
   
   package Wide_String_Lists is new Doubly_Linked_Lists(Unbounded_Wide_String);
   
   type Fragment_Title_Chain is 
      record
         List: Wide_String_Lists.List; -- last is for most deeply nested fragment, the 'principal'
      end record;
   
   function Principal (Chain: Fragment_Title_Chain) return Unbounded_Wide_String is
      return Last_Element(Chain.List);
      
   function Extend (Chain: Fragment_Title_Chain; Title: Unbounded_Wide_String) return Fragment_Title_Chain is
      Result: Fragment_Title_Chain;
   begin
      Result.List := Copy(Chain.List);
      Append( Result.List, Title );
      return Result;
   end;
   
   
   
   
   
   --\
   --/ :

   procedure Process_Source_File (Source_File_Entry: in Directory_Entry_Type) is
   
      Line_List: Line_Lists.List;
   begin
      Info_Message ("Reading source file " & Quote (Full_Name (Source_File_Entry)));
      Load (Full_Name (Source_File_Entry), Line_List);
      declare
         Block: Text_Block := To_Text_Block (Line_List);
      begin
         Normalize_Indent (Block);
         Process_Fragment (Block, "Source text file " & Quote (Full_Name (Source_File_Entry)));
         --TODO: More?
      end;   
   end;

   procedure Process_Source_File_Directory (Directory_Entry: in Directory_Entry_Type) is
   begin
      Info_Message ("Scanning directory " & Quote (Full_Name (Source_File_Entry)));
      for Pattern of Source_File_Patterns (Ada_Family) loop
         Search (Full_Name (Source_File_Entry), Pattern, Ordinary_File, Process_Source_File);
      end loop;
      Search (Full_Name (Source_File_Entry), "", Directory, Process_Source_File_Directory'Access);
   end;

   procedure Process_Source_File_Root (Root_Name: in String) is
      Info_Message ("Scanning subroot directory " & Quote (Root_Name));
      for Pattern of Source_File_Patterns (Ada_Family) loop
         Search (Root_Name, Pattern, Ordinary_File, Process_Source_File);
      end loop;
      Search (Root_Name, "", Directory, Process_Source_File_Directory'Access);
   end;
   
   --\   
   --/ Index the fragments:
   
   procedure Create_Fragment_Index is
   begin
      Info_Message("Creating fragment index");
   end;
   
   --\
   --/ Write out the fragment files (`.xml`):
   
   procedure Output_Fragment_Files is
   begin
      Info_Message("Outputting fragment files");
   end;
   
   --\
   --/ Write our the fragment index file (`.dtd`):
   
   procedure Output_Fragment_Index_File is
   begin
      Info_Message("Outputting fragment index file");
   end;
   
   --\

   
   
   
   --/ Message output:
   
   procedure Vital_Message (Message: in String) is
   begin
      Put_Line (Message);
   end;
   
   procedure Normal_Message (Message: in String) is
   begin
      if Verbosity >= Normal_Verbosity then
         Put_Line (Message);
      end if;
   end;
   
   procedure Info_Message (Message: in String) is
   begin
      if Verbosity >= High_Verbosity then
         Put_Line (Message);
      end if;
   end;
   
   procedure Debugging_Message (Message: in String) is
   begin
      if Verbosity >= Debugging_Verbosity then
         Put_Line ("[DEBUG] " & Message);
      end if;
   end;
   
   function Quote (Value: in String) return String is
      return """" & Value & """";
      
   --\
   --/ HTML/XML utilities:
   
   function XHTML_Safe (Raw: in Wide_Unbounded_String) return Wide_Unbounded_String is
   
      Result: Wide_Unbounded_String;
   begin
      for C: Wide_Character of Raw loop
         Append( Result, XHTML_Safe(C) );
      end loop;
      return Result;   
   end;
   
   function XHTML_Safe (Raw: in Wide_Character) return Wide_String is   
   begin
      if Is_XHTML_Safe(Raw) then
         return Wide_String'( 1 => Raw );
      else
         return XML_Character_Entity_Encode(Raw);
      end if;
   end;
   
   function Is_XHTML_Safe (Raw: in Wide_Character) return Boolean is
   begin
      if Is_Control_Character(Raw) then return False;
      if not Is_Latin_1(Raw) then return False;
      
      
      
      If Raw = '<' then return False;
      If Raw = '>' then return False;
      If Raw = '&' then return False;
      If Raw = Latin_1.Apostrophe then return False;
      If Raw = '' then return False;
      If Raw = '' then return False;
      
      
      return True;
   end;
   
   
   --\
   
begin
   --/ Main program:
   Debugging_Message ("Starting program");
   if Help_Requested then return; end if;
   for Name of Source_File_Root_Names.all loop
      Process_Source_File_Root (To_String (Name));
   end loop;
   Create_Fragment_Index;
   if not Nongenerate_Mode then
      Output_Fragment_Files;
      Output_Fragment_Index_File;
   end if;
   Debugging_Message ("Finished program");
   --\
end revtang;

--\
