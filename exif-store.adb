with GNAT.Htable;
with GNAT.Table;
with Ada.Unchecked_Conversion;
package body Exif.Store is

   type String_Constant is access constant String;

   type Image_Data is record
      Name : String_Constant;
      Data : Storage_Access;
   end record;

   package Image_Table is new GNAT.Table (Image_Data, Image_Id, 1, 2000, 100);
   use Image_Table;

   type Hash_Range is new Natural range 0 .. 16729;

   function Hash_Info (K : Info_Key) return Hash_Range;
   function To_Text is new Ada.Unchecked_Conversion (Storage_Access, Text);

   package Info_Table is new GNAT.Htable.Simple_Htable
     (Header_Num => Hash_Range,
      Key        => Info_Key,
      Element    => Info_Element,
      No_Element => No_Info,
      Hash       => Hash_Info,
      Equal      => "=");

   function Get (K : Info_Key) return Info_Element renames Info_Table.Get;

   function Base_Name (Full_Name : String) return String;

   ---------------
   -- Base_Name --
   ---------------

   function Base_Name (Full_Name : String) return String is
   begin
      for J in reverse Full_Name'Range loop
         if Full_Name (J) = ' '
           or Full_Name (J) = '/' or Full_Name (J) = '\'
         then
            return (Full_Name (J + 1 .. Full_Name'Last));
         end if;
      end loop;

      return Full_Name;
   end Base_Name;

   ---------------
   -- File_Name --
   ---------------

   function File_Name (Image : Image_Id) return String is
   begin
      return Table (Image).Name.all;
   end File_Name;

   ---------
   -- Get --
   ---------

   function Get (K : Info_Key) return Text is
      Info : Info_Element := Get (K);
   begin
      if Info.Datatype /= Ascii then
         return No_Text;
      else
         return To_Text (Info.Data);
      end if;
   end Get;

   function Get_Data (Image : Image_Id) return Storage_Access is
   begin
      return Table (Image).Data;
   end Get_Data;

   function Get_First return Info_Element renames Info_Table.Get_First;

   function Get_Next return Info_Element renames Info_Table.Get_Next;

   function Get_Thumbnail (Image : Image_ID) return Storage_Access is
      kJpegRGB       : constant Unsigned_32 := 1; -- PhotoShop thumbnail format
      Photoshop_Info : Info_Element;
      Offset_Info    : Info_Element;
      Length_Info    : Info_Element;
      Offset : Storage_Offset;
      Length : Storage_Offset;

   begin
      --  If there is a Photoshop tag, this image must have been edited,
      --  and we return the thumbnail reflecting the edits, or null if the
      --  thumbnail is in an unknown format.

      Photoshop_Info :=
        Get ((Image, (Photoshop_IFD, New_Windows_Thumbnail)));

      if Photoshop_Info.Data /= null then
         declare
            Data : Storage_Access renames Photoshop_Info.Data;
         begin
            if Data'Length >= 30 and then
              Value (Data (Data'First .. Data'First + 3)) = kJpegRGB
            then
               return new Storage'(Data (Data'First + 28 .. Data'Last));
            else
               return null;
            end if;
         end;
      end if;

      --  If not, use the standard EXIF thumbnail

      Offset_Info :=
        Get ((Image, (Thumbnail_IFD, JPEG_Interchange_Format)));
      Length_Info :=
        Get ((Image, (Thumbnail_IFD, JPEG_Interchange_Format_Length)));

      if Storage_Size (Offset_Info.Datatype) = 4 and
         Storage_Size (Length_Info.Datatype) = 4
      then
         Offset := Storage_Offset (Value (Offset_Info));
         Length := Storage_Offset (Value (Length_Info));
         return new Storage'(Get_Data (Image) (Offset .. Offset + Length - 1));
      end if;

      return null;
   end Get_Thumbnail;

   function Has_JPEG_Thumbnail (Image : Image_ID) return Boolean is
      Compr_Info : constant Info_Element
        := Get ((Image, (Thumbnail_IFD, Compression)));

   begin
      return Storage_Size (Compr_Info.Datatype) = 2 and then
        Value (Compr_Info) = 6;
   end Has_JPEG_Thumbnail;

   function Hash_Info (K : Info_Key) return Hash_Range is
      function To_Unsigned is
         new Ada.Unchecked_Conversion (System.Address, Storage_Offset);
   begin
      return Hash_Range'Base
        (Unsigned_32 (K.Image) mod 2**30
           xor Unsigned_32 (Value (K.Tag)) * 2**15)
        mod Hash_Range'Last;

   end Hash_Info;

   procedure Iterate is
   begin
      for J in 1 .. Last loop
         Action (J);
      end loop;
   end Iterate;

   function Name (Image : Image_Id) return String is
   begin
      return Base_Name (Table (Image).Name.all);
   end Name;

   function New_Image (Name : String)
      return Image_Id
   is
   begin
      Increment_Last;
      Table (Last) := (new String'(Name), null);
      return Last;
   end New_Image;

   procedure Set (K : Info_Key; E : Info_Element) renames Info_Table.Set;

   procedure Set (K : Info_Key; Value : Unsigned_32) is
      function To_File is new Ada.Unchecked_Conversion (Unsigned_32, File_32);
      E : Info_Element (System.Default_Bit_Order);
   begin
      E.Datatype := Long;
      E.Data := new File_32'(To_File (Value));
      Set (K, E);
   end Set;

   procedure Set_Data (Image : Image_ID; Data : Storage_Access) is
   begin
      Table (Image).Data := Data;
   end Set_Data;

   procedure Remove (K : Info_Key) renames Info_Table.Remove;

   procedure Reset renames Info_Table.Reset;

   function Valid (Image : Image_Id) return Boolean is
   begin
      return Image /= No_Image and Image <= Last;
   end Valid;

   function Value (Info : Info_Element) return Unsigned_32 is
      Size    : Storage_Offset := Storage_Size (Info.Datatype);
      Result  : Unsigned_32 := 0;
   begin
      if Info.Data /= null and then
         Info.Data'Length in Size .. 4
      then
         declare
            use System;
            Swapped : constant Boolean := Info.Bit_Order /= High_Order_First;
            Data    : Storage (0 .. Info.Data'Length - 1) := Info.Data.all;
            Count   : constant Storage_Offset := Data'Length / Size;
            Temp    : Stream_Element;
         begin
            if Swapped and Size > 1 then
               for J in 0 .. Count - 1 loop
                  for K in 0 .. Size / 2 - 1 loop
                     Temp := Data (J * Size + K);
                     Data (J * Size + K) := Data (J * Size + Size - 1 - K);
                     Data (J * Size + Size - 1 - K) := Temp;
                  end loop;
               end loop;
            end if;

            for J in Storage_Offset'(0) .. Data'Length - 1 loop
                  Result := Result * 2**8 + Unsigned_32 (Data (J));
            end loop;
         end;

      else
         raise Constraint_Error;
      end if;

      return Result;
   end Value;
end Exif.Store;
