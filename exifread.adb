with Ada;                      use Ada;
with Ada.Command_Line;         use Ada.Command_Line;
with Ada.Exceptions;           use Ada.Exceptions;
with Ada.IO_Exceptions;
with Ada.Streams;              use Ada.Streams;
with Ada.Streams.Stream_IO;    use Ada.Streams.Stream_IO;
with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Text_IO.Text_Streams;

with Interfaces;               use Interfaces;
with System;

with GNAT.Command_Line;        use GNAT.Command_Line;
with GNAT.Sockets;             use GNAT.Sockets;
with Checked_Conversion;

with Exif; use Exif;
with Exif.Store;  use Exif.Store;
with Exif.Serve;  use Exif.Serve;

procedure Exifread is

   Data_Error : exception renames Ada.IO_Exceptions.Data_Error;
   Use_Error  : exception renames Ada.IO_Exceptions.Use_Error;
   Name_Error : exception renames Ada.IO_Exceptions.Name_Error;

   Describe_Values : Boolean := True;
   Run_Web_Server  : Boolean := False;

   New_Line   : constant String := ASCII.CR & ASCII.LF;

   Output          : Stream_IO.Stream_Access
     := Stream_IO.Stream_Access (Text_IO.Text_Streams.Stream
          (Text_IO.Current_Output.all));

   type File_Format is (Unknown, JPEG, TIFF, CRW, PSD);
   subtype Known_File_Format is
     File_Format range File_Format'Succ (Unknown) .. File_Format'Last;

   Tiff_Magic : constant Unsigned_16 := 16#2A#;
   CRW_Magic  : constant Unsigned_16 := 16#1A#;

   LOF_Magic  : constant File_16 := (16#49#, 16#49#); -- "II", Low_Order_First
   HOF_Magic  : constant File_16 := (16#4D#, 16#4D#); -- "MM", High_Order_First
   JPEG_SOI   : constant File_16 := (16#FF#, 16#D8#);
   JPEG_EOI   : constant File_16 := (16#FF#, 16#D9#);
   PS_Magic   : constant File_32 := (16#38#, 16#42#, 16#50#, 16#53#); -- "8BPS"

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Depth_Of_Field
     (Focal_Length, Hyperfocal, Subject_Distance : Float) return Float;

   function Far_Sharp_Distance
     (Focal_Length, Hyperfocal, Subject_Distance : Float) return Float;

   function Hyperfocal_Distance
     (Focal_Length, F_Number, Coc : Float) return Float;

   function Near_Sharp_Distance
     (Focal_Length, Hyperfocal, Subject_Distance : Float) return Float;

   procedure Process_Directory
     (Image  : Store.Image_Id;
      Kind   : IFD_Kind;
      Data   : Storage;
      Offset : Storage_Offset);

   procedure Process_TIFF_Header
    (Header          : Storage;
     First_Directory : out Storage_Offset);

   procedure Process_Maker_Note
     (Image  : Store.Image_Id;
      Data   : Storage);

   procedure Process_PSD
     (Image  : Store.Image_Id;
      Data   : Storage);

   procedure Skip_File
     (Input_File : Stream_IO.File_Type;
      Amount     : Unsigned_16);

   -------------------------
   -- Hyperfocal_Distance --
   -------------------------

   function Hyperfocal_Distance (Focal_Length, F_Number, Coc : Float)
     return Float is
   begin
      return Focal_Length**2 / (F_Number * Coc);
   end Hyperfocal_Distance;

   -------------------------
   -- Near_Sharp_Distance --
   -------------------------

   function Near_Sharp_Distance
     (Focal_Length, Hyperfocal, Subject_Distance : Float) return Float is
   begin
      return (Hyperfocal * Subject_Distance)
               / (Hyperfocal - (Subject_Distance - Focal_Length));
   end Near_Sharp_Distance;

   ------------------------
   -- Far_Sharp_Distance --
   ------------------------

   function Far_Sharp_Distance
     (Focal_Length, Hyperfocal, Subject_Distance : Float) return Float is
   begin
      return (Hyperfocal * Subject_Distance)
               / (Hyperfocal + (Subject_Distance - Focal_Length));
   end Far_Sharp_Distance;

   --------------------
   -- Depth_Of_Field --
   --------------------

   function Depth_Of_Field
     (Focal_Length, Hyperfocal, Subject_Distance : Float) return Float is
   begin
      return Far_Sharp_Distance (Focal_Length, Hyperfocal, Subject_Distance)
         - Near_Sharp_Distance (Focal_Length, Hyperfocal, Subject_Distance);
   end Depth_Of_Field;

   procedure Skip_File
     (Input_File : Stream_IO.File_Type;
      Amount     : Unsigned_16)
   is
      Idx : Stream_IO.Count := Index (Input_File) + Stream_IO.Count (Amount);
   begin
      Set_Index (Input_File, Idx);
   end Skip_File;

   procedure Process_PSD
     (Image  : Store.Image_Id;
      Data   : Storage)
   is
      IM_Magic   : File_32 := (16#38#, 16#42#, 16#49#, 16#4D#); -- "8BIM"
      Save_Order : constant System.Bit_Order := File_Bit_Order;
      Ptr        : Storage_Offset := Data'First;
      Tag        : IFD_Tag;
      Name_Len   : Unsigned_8;
      Value_Len  : Unsigned_32;
   begin
      File_Bit_Order := System.High_Order_First;
      while Ptr + 11 <= Data'Last
         and then Data (Ptr .. Ptr + 3) = IM_Magic
      loop
         Ptr := Ptr + IM_Magic'Length;

         String'Write (Output, New_Line);
         Tag := To_IFD_Tag (Photoshop_IFD, Value (Data (Ptr .. Ptr + 1)));

         Ptr := Ptr + 2;

         Name_Len := Value (Data (Ptr));

         declare
            Name_Data : Storage :=
                        Data (Ptr + 1 .. Ptr + Storage_Offset (Name_Len));
         begin

            if Tag.Kind = Unknown_IFD then
               String'Write (Output, "PHOTOSHOP");

               declare
                  Tag_Img :  String := Tag.Value'Img;
               begin
                  Tag_Img (Tag_Img'First) := '.';
                  String'Write (Output, Tag_Img);
                  Character'Write (Output, '.');
               end;

               if Name_Len > 0 then
                  for J in Name_Data'Range loop
                     if Name_Data (J) = Character'Pos (' ') then
                        Name_Data (J) := Character'Pos ('_');
                     end if;
                  end loop;
                  Storage'Write (Output, Name_Data);
               end if;

            else
               Write (Output, Tag);
            end if;

            Character'Write (Output, ' ');
            --  Add Name_Len + 1 to Ptr, rounding total up to even
            Ptr := Ptr + (Name_Data'Length + 2) / 2 * 2;
         end;

         Value_Len := Value (Data (Ptr .. Ptr + 3));
         Ptr := Ptr + 4;

         declare
            Tag_Data : Storage_Access :=
              new Storage (0 .. Storage_Offset (Value_Len) - 1);
         begin
            Tag_Data.all := Data (Ptr .. Ptr + Tag_Data'Length - 1);
            Set (K => (Image, Tag),
                 E => (File_Bit_Order, Undefined, Tag_Data));

            Write (Output, Byte, Tag_Data.all);

            if Tag = (Photoshop_IFD, EXIF.EXIF) or
               Tag = (Photoshop_IFD, EXIF.EXIF2)
            then
               declare
                  Prev_Order : constant System.Bit_Order := File_Bit_Order;
                  First_Dir  : Storage_Offset;
               begin
                  Process_TIFF_Header (Tag_Data.all, First_Dir);
                  Process_Directory (Image, Primary_Image_IFD,
                     Tag_Data.all, First_Dir);
                  File_Bit_Order := Prev_Order;
               end;
            end if;

            Ptr := Ptr + (Tag_Data'Length + 1) / 2 * 2; -- Round up to even
         end;

      end loop;

      if Ptr < Data'Last then
         Put_Line ("Unrecognized data in Photoshop segment");
      end if;

      File_Bit_Order := Save_Order;
   end Process_PSD;

   procedure Process_PSD (Image : Store.Image_Id; Input_File : Stream_IO.File_Type)
   is
      Input   : Stream_IO.Stream_Access := Stream (Input_File);
      Save_Order : constant System.Bit_Order := File_Bit_Order;
      Header     : Storage (4 .. 25);
      Version    : Unsigned_16;
      Length     : File_32;
      Len_Val    : Unsigned_32;
   begin
      File_Bit_Order := System.High_Order_First;

      Storage'Read (Input, Header);
      Version := Value (Header (4 .. 5));

      if Version /= 1 then
         raise Data_Error;
      end if;

      --  Skip color segment for now
      File_32'Read (Input, Length);
      Len_Val := Value (Length);

      if Len_Val > 0 then
         Put_Line ("Skipping" & Len_Val'Img & " bytes of color info");
         Set_Index (Input_File, Index (Input_File) + Stream_IO.Count (Len_Val));
      end if;

      --  Read image resources segment
      File_32'Read (Input, Length);
      Len_Val := Value (Length);
      if Len_Val > 0 then
         Put_Line ("Reading" & Len_Val'Img
           & " bytes of PhotoShop image resources");
         declare
            Data : Storage_Access :=
                     new Storage (0 .. Storage_Offset (Len_Val) - 1);
         begin
            Storage'Read (Input, Data.all);
            Set_Data (Image, Data);
            Process_PSD (Image, Data.all);
         end;

      else
         Put_Line ("No image resources found???");
      end if;

      File_Bit_Order := Save_Order;
   end Process_PSD;


   procedure Process_IFD
     (Image : Store.Image_Id;
      Kind  : IFD_Kind;
      IFD    : IFD_Entry;
      Data   : Storage)
   is
      Tag      : constant IFD_Tag := To_IFD_Tag (Kind, Value (IFD (0 ..  1)));
      Datatype : constant IFD_Datatype := To_Datatype (IFD (2 ..  3));
      Values   : constant Unsigned_32 := Value (IFD (4 ..  7));
      Info     : constant Unsigned_32 := Value (IFD (8 .. 11));
      Typesize : constant Storage_Offset := Storage_Size (Datatype);
      Size     : constant Storage_Offset := Storage_Offset (Values) * Typesize;
   begin
      String'Write (Output, New_Line);
      if Tag.Kind = Unknown_IFD and Kind /= Unknown_IFD then
         declare
            Img : constant String := Kind'Img;
         begin
            String'Write (Output, Img (Img'First .. Img'Last - 4) & '.');
         end;
      end if;
      Write (Output, Tag);
      Character'Write (Output, ' ');

      if Size <= 4 then
         declare
            Item        : Unsigned_32 := 0;
            Description : Text;
            Tag_Data    : Storage renames IFD (8 .. 8 + Size - 1);
         begin
            if Typesize = 1 then
               for C in Tag_Data'Range loop
                  Item := Item * 16#100# + Unsigned_32 (Tag_Data (C));
               end loop;

            elsif Typesize = 2 then
               if Values >= 1 then
                  Item := Unsigned_32 (Unsigned_16'(Value (Tag_Data (8 .. 9))));
               end if;
               if Values = 2 then
                  Item := Item * 16#10000# +
                     Unsigned_32 (Unsigned_16'(Value (Tag_Data (10 .. 11))));
               end if;

            elsif Typesize = 4 and Values = 1 then
               Item := Value (Tag_Data (8 .. 11));
            end if;

            if Describe_Values then
               Description := Describe (Tag, Datatype, Item);

            else
               Description := null;
            end if;

            Set
              (K => (Image, Tag),
               E => (File_Bit_Order, Datatype, new Storage'(Tag_Data)));

            if Description /= null then
               String'Write (Output, Description.all);

            else
               Write (Output, Datatype, Tag_Data);
            end if;
         end;

      elsif Storage_Offset (Info) >= Data'First and
            Storage_Offset (Info) + Size - 1 <= Data'Last
      then
         declare
            Tag_Data : Storage renames Data (Storage_Offset (Info) ..
                                          Storage_Offset (Info) + Size - 1);
         begin
            Set
              (K => (Image, Tag),
               E => (File_Bit_Order, Datatype, new Storage'(Tag_Data)));

            if Tag = (Exif_IFD, Maker_Note) then
               Process_Maker_Note (Image, Tag_Data);
            elsif Tag = (TIFF_IFD, PS_Image_Resources) then
               Process_PSD (Image, Tag_Data);

            else
               Write (Output, Datatype, Tag_Data);
            end if;
         end;

      else
         -- IFD Entry points outside IFD data block
         String'Write (Output, Datatype'Img & " * " & Values'Img
           & " at offset" & Info'Img & " (unreachable)");
         return;
      end if;

      if Tag = (Primary_Image_IFD, EXIF_Offset) then
         Process_Directory (Image, Exif_IFD, Data, Storage_Offset (Info));

      elsif Tag = (Primary_Image_IFD, GPS_Info) then
         Process_Directory (Image, GPS_IFD, Data, Storage_Offset (Info));

      elsif Tag = (Exif_IFD, Interoperability_Offset) then
         Process_Directory (Image, Interoperability_IFD, Data,
            Storage_Offset (Info));

      elsif Tag = (Kodak_IFD, Special_Effects) then
         Process_Directory (Image, Unknown_IFD, Data, Storage_Offset (Info));

      elsif Tag = (Kodak_IFD, Borders) then
         Process_Directory (Image, Unknown_IFD, Data, Storage_Offset (Info));
      end if;


   exception
      when E : Constraint_Error | Program_Error =>
         for I in IFD'Range loop
            Text_IO.Put (IFD (I)'Img);
         end loop;
         Text_IO.New_Line;
         Put_Line ("<" & Exception_Name (E) & " (" &
           Exception_Message (E) & ") in tag" &
           Unsigned_16'Image (Value (IFD (0 .. 1))) & " of EXIF header>");
   end Process_IFD;

   -----------------------
   -- Process_Directory --
   -----------------------

   procedure Process_Directory
     (Image  : Store.Image_Id;
      Kind   : IFD_Kind;
      Data   : Storage;
      Offset : Storage_Offset)
   is
      Entries : Unsigned_16 ;
      Ptr     : Storage_Offset := Offset;
      Next    : Storage_Offset;
   begin
      Entries := Value (Data (Ptr .. Ptr + 1));
      Ptr := Ptr + 2;

      for N in 0 .. Entries - 1 loop
         Process_IFD (Image, Kind , Data (Ptr .. Ptr + 11), Data);
         Ptr := Ptr + 12;
      end loop;

      Next := Storage_Offset
        (Unsigned_32'(Value (Data (Ptr .. Ptr + 3))));

      if Next /= 0 then
         if Kind  = Primary_Image_IFD then
            Process_Directory (Image, Thumbnail_IFD, Data, Next);

         elsif Kind = Canon_IFD and Next = 65535 then
            null;  --  The Canon IFD apparently doesn't use 0 for the 20D

         else
            Process_Directory (Image, Kind, Data, Next);
         end if;
      end if;

   end Process_Directory;

   procedure Process_TIFF_Header
    (Header          : Storage;
     First_Directory : out Storage_Offset)
   is
      Order : Storage renames Header (Header'First .. Header'First + 1);
      Magic : Storage renames Header (Header'First + 2 .. Header'First + 3);
      Dir   : Storage renames Header (Header'First + 4 .. Header'First + 7);
   begin
      if Order = HOF_Magic then
         File_Bit_Order := System.High_Order_First;
         Put_Line ("Motorola byte order");


      elsif Order = LOF_Magic then
         File_Bit_Order := System.Low_Order_First;
         Put_Line ("Intel byte order");

      else
         Put_Line ("invalid byte-order: must be Motorola (MM) or Intel (II)");
         raise Data_Error;
      end if;

      if Value (Magic) /= TIFF_Magic then
         Put_Line ("unsupported file format");
         raise Data_Error;
      end if;

      First_Directory := Storage_Offset (Unsigned_32'(Value (Dir)));
   end Process_TIFF_Header;

   procedure Process_Exif
     (Image : Store.Image_Id;
      Input : Stream_IO.File_Type;
      Length : Unsigned_16)
   is
      Previous_Order  : constant System.Bit_Order := File_Bit_Order;
      First_Directory : Storage_Offset;
      Data            : constant Storage_Access :=
                          new Storage (0 .. Storage_Offset (Length - 1));
   begin
      Storage'Read (Stream (Input), Data.all);
      Process_TIFF_Header (Data.all, First_Directory);
      Set_Data (Image, Data);
      Process_Directory (Image, Primary_Image_IFD, Data.all, First_Directory);
      File_Bit_Order := Previous_Order;
   end Process_Exif;

   procedure Process_Meta
     (Image  : Store.Image_Id;
      Input  : Stream_IO.File_Type;
      Length : Unsigned_16)
   is
      Previous_Order  : constant System.Bit_Order := File_Bit_Order;
      Data            : constant Storage_Access :=
                          new Storage (0 .. Storage_Offset (Length - 1));
      First_Directory : Storage_Offset;
   begin
      Storage'Read (Stream (Input), Data.all);

      Process_TIFF_Header (Data.all, First_Directory);
      Process_Directory
        (Image, Kodak_IFD, Data.all, Storage_Offset (First_Directory));

      File_Bit_Order := Previous_Order;
   end Process_Meta;

   procedure Process_Phot
     (Image  : Store.Image_Id;
      Input  : Stream_IO.File_Type;
      Length : Unsigned_16)
   is
      Data : constant Storage_Access :=
                               new Storage (0 .. Storage_Offset (Length - 1));
      Ptr  : Storage_Offset := Data'First;
   begin
      Storage'Read (Stream (Input), Data.all);

      --  Skip rest of identifier
      while Ptr < Data'Last and then Data (Ptr) /= 0 loop
         Ptr := Ptr + 1;
      end loop;

      Process_PSD (Image, Data (Ptr + 1 .. Data'Last));
   end Process_Phot;

   procedure Process_TIFF
     (Image      : Store.Image_Id;
      Input_File : Stream_IO.File_Type)
   is
      Input           : Stream_IO.Stream_Access := Stream (Input_File);
      First_Directory : Stream_IO.Count;
      Dir             : File_32;
   begin
      File_32'Read (Input, Dir);
      First_Directory := Stream_IO.Count (Unsigned_32'(Value (Dir)));
      Set_Index (Input_File, First_Directory + 1);
      declare
         --  Since with TIFF it is not possible to know the length of the
         --  IFD and associated data before analyzing it, overestimate it
         --  and use explicit positioning to find the actual image data.

         Length : Stream_IO.Count := Stream_IO.Count'Min
                    (16#FFFF#, Size (Input_File) - First_Directory);
         Data   : constant Storage_Access
           := new Storage (Storage_Offset (First_Directory) ..
                           Storage_Offset (First_Directory + Length - 1));
      begin
         Storage'Read (Input, Data.all);
         Process_Directory
           (Image, TIFF_IFD, Data.all, Storage_Offset (First_Directory));
      end;
   end Process_Tiff;

   procedure Process_FPXR
     (Image  : Store.Image_Id;
      Input  : Stream_IO.File_Type;
      Length : Unsigned_16)
   is
      Data : constant Storage_Access :=
                               new Storage (0 .. Storage_Offset (Length - 1));
      Ptr   : Storage_Offset := Data'First;
      Start : Storage_Offset;
   begin
      Storage'Read (Stream (Input), Data.all);

      while Ptr < Data'Last and then Data (Ptr .. Ptr + 1) /= JPEG_SOI loop
         Ptr := Ptr + 1;
      end loop;

      Start := Ptr;
      Ptr := Ptr + 2;

      if Ptr < Data'Last and then
        Data (Ptr) = 16#FF# and then
        Data (Ptr + 1) >= 16#C0#
      then
         while Ptr < Data'Last and Data (Ptr .. Ptr + 1) /= JPEG_EOI loop
            Ptr := Ptr + 1;
         end loop;
      end if;

      if Ptr < Data'Last and then Data (Ptr .. Ptr + 1) = JPEG_EOI then
         declare
            Tag      : IFD_Tag := (Flashpix_IFD, JPEG_Image);
            Tag_Data : Storage_Access := new Storage'(Data (Start .. Ptr + 1));
         begin
            String'Write (Output, New_Line);
            Write (Output, Tag);
            Character'Write (Output, ' ');
            Write (Output, Undefined, Tag_Data.all);
         end;
      end if;
   end Process_FPXR;

   procedure Process_JPEG
     (Image      : Store.Image_ID;
      Input_File : Stream_IO.File_Type)
   is
      JPEG_DQT   : constant File_16 := (16#FF#, 16#DB#);
      JPEG_DHT   : constant File_16 := (16#FF#, 16#C4#);
      JPEG_DRI   : constant File_16 := (16#FF#, 16#DD#);
      JPEG_SOF0  : constant File_16 := (16#FF#, 16#C0#);
      JPEG_SOF1  : constant File_16 := (16#FF#, 16#C1#);
      JPEG_SOF2  : constant File_16 := (16#FF#, 16#C2#);
      JPEG_SOS   : constant File_16 := (16#FF#, 16#DA#);
      JPEG_APP0  : constant File_16 := (16#FF#, 16#E0#);
      JPEG_APP1  : constant File_16 := (16#FF#, 16#E1#);
      JPEG_APP2  : constant File_16 := (16#FF#, 16#E2#);
      JPEG_APP3  : constant File_16 := (16#FF#, 16#E3#);
      JPEG_APP14 : constant File_16 := (16#FF#, 16#ED#); -- Photoshop marker
      JPEG_APP15 : constant File_16 := (16#FF#, 16#EF#);
      JPEG_COM   : constant File_16 := (16#FF#, 16#FE#);

      Input   : Stream_IO.Stream_Access := Stream (Input_File);
      Length  : File_16;
      Filler  : File_16;
      Magic   : File_16;
      Len_Val : Unsigned_16;
      Ident   : String (1 .. 4);
   begin
      File_Bit_Order := System.High_Order_First;
      File_16'Read (Input, Magic);
      File_16'Read (Input, Length);

      --  Process any APP headers

      while Magic >= JPEG_APP0 loop
         String'Read (Input, Ident);
         Len_Val := Value (Length) - Length'Length - Ident'Length;

         if Magic = JPEG_APP1 and then Ident = "Exif" then
            File_16'Read (Input, Filler);
            Process_Exif (Image, Input_File, Len_Val - Filler'Length);

         elsif Magic = JPEG_APP3 and then Ident = "Exif" then
            --  Kodak "EXIF" meta data, this doesn't appear to be EXIF standard
            File_16'Read (Input, Filler);
            Process_Meta (Image, Input_File, Len_Val - Filler'Length);

         elsif (Magic = JPEG_APP1 or Magic = JPEG_APP3)
            and then Ident = "Meta"
         then
            File_16'Read (Input, Filler);
            Process_Meta (Image, Input_File, Len_Val - Filler'Length);

         elsif Magic = JPEG_APP2 and then Ident = "FPXR"
            and then Len_Val > 2048
         then
            --  Only process Flashpix segments that may contain a JPEG image
            Process_FPXR (Image, Input_File, Len_Val);

         elsif Magic = JPEG_APP14 and then Ident = "Phot" then
            Process_Phot (Image, Input_File, Len_Val);

         elsif Magic = JPEG_COM then
            declare
               Data : Storage (1 .. Storage_Offset (Len_Val));
            begin
               Put ("JPEG " & Ident & " comment: ");
               Storage'Read (Input, Data);
               Storage'Write (Output, Data);
               String'Write (Output, New_Line);
            end;

         else
            --  Unknown APP header - skip it
            declare
               App_Nr : constant String
                 := Stream_Element'Image (Magic (2) - JPEG_APP0 (2));
            begin
               Put ("Skipping" & Len_Val'Img
                 & " bytes of JPEG APP" & App_Nr (2 .. App_Nr'Last)
                 & " header (" & Ident & ")");
            end;

            Skip_File (Input_File, Len_Val);
         end if;
         Text_IO.New_Line;

         File_16'Read (Input, Magic);
         File_16'Read (Input, Length);
      end loop;

      while Magic = JPEG_DQT or Magic = JPEG_DHT or Magic = JPEG_DRI
        or Magic = JPEG_COM
      loop
         Len_Val := Value (Length);
         Skip_File (Input_File, Len_Val - Length'Length);
         File_16'Read (Input, Magic);
         File_16'Read (Input, Length);
      end loop;

      if Magic = JPEG_SOF0 or Magic = JPEG_SOF1 or Magic = JPEG_SOF2 then
         declare
            Precision  : File_8;
            Vertical   : File_16;
            Horizontal : File_16;
            Components : File_8;
         begin
            File_8'Read (Input, Precision);
            File_16'Read (Input, Vertical);
            File_16'Read (Input, Horizontal);
            File_8'Read (Input, Components);
         end;

      else
         Put_Line ("error: expected SOF0, SOF1 or SOF2 in JPEG stream");
         raise Data_Error;
      end if;
      Put_Line ("Not processing rest of JPEG stream");
   end Process_JPEG;

   --  Try to decode the undocumented Canon CRW format for PowerShot cameras

   procedure Process_CRW
     (Image      : Store.Image_Id;
      Input_File : Stream_IO.File_Type)
   is
      File_Size  : constant Stream_IO.Count := Size (Input_File);
      JPEG_Start : File_32;
      Start_Val  : Unsigned_32;
      Magic      : File_16;
      JPEG_Key   : Info_Key;
      Input      : Stream_IO.Stream_Access := Stream (Input_File);

   begin
      File_Bit_Order := System.Low_Order_First;

      Set_Index (Input_File, File_Size - 18 + 1);
      File_32'Read (Input, JPEG_Start);
      Start_Val := Value (JPEG_Start) + 26;
      Put_Line ("JPEG starts at" & Start_Val'Img);

      JPEG_Key.Image := Image;
      JPEG_Key.Tag := (Canon_Raw_IFD, Jpeg_Image);

      Set (JPEG_Key, Start_Val);

      Set_Index (Input_File, Stream_IO.Count (Start_Val + 1));
      File_16'Read (Input, Magic);
      if Magic /= JPEG_SOI then
         Put_Line ("No JPEG image found in CRW file");
      else
         Process_JPEG (Image, Input_File);
      end if;
   end Process_CRW;

   ------------------
   -- Process_File --
   ------------------

   --  Guess file format based on "magic" bytes at start, and
   --  start processing based on guessed format.

   procedure Process_File (Input_File : Stream_IO.File_Type) is
      Input      : Stream_IO.Stream_Access := Stream (Input_File);
      Format     : File_Format := Unknown;
      Image      : Store.Image_ID;
      Magic      : File_16;

   begin
      File_Bit_Order := System.High_Order_First;

      File_16'Read (Input, Magic);
      if Magic = LOF_Magic or Magic = HOF_Magic then
         --  File starts with "II" or "MM", so it is
         --  probably TIFF or Canon RAW.
         if Magic = LOF_Magic then
            File_Bit_Order := System.Low_Order_First;
         end if;

         --  Read next magic word and confirm our guess

         File_16'Read (Input, Magic);
         if Value (Magic) = TIFF_Magic then
            Format := TIFF;

         elsif Value (Magic) = CRW_Magic then
            Format := CRW;
         end if;

      elsif Magic = JPEG_SOI then
         --  Looks like this will be some JPEG format

         Format := JPEG;

      elsif Magic = PS_Magic (1 .. 2) then
         --  Photoshop file
         File_16'Read (Input, Magic);

         if Magic = PS_Magic (3 .. 4) then
            Format := PSD;
         end if;
      end if;

      if Format = Unknown then
         Put_Line ("Unsupported file format");
         return;
      end if;

      Image := New_Image (Name (Input_File));

      case Known_File_Format'(Format) is
         when CRW  => Process_CRW  (Image, Input_File);
         when JPEG => Process_JPEG (Image, Input_File);
         when PSD  => Process_PSD  (Image, Input_File);
         when TIFF => Process_TIFF (Image, Input_File);
      end case;
   end Process_File;

   ------------------------
   -- Process_Maker_Note --
   ------------------------

   procedure Process_Maker_Note
     (Image  : Store.Image_Id;
      Data   : Storage)
   is
      Maker     : Text := Get ((Image, (Primary_Image_IFD, Make)));
      Nikon_Hex : constant Storage := (16#4E#, 16#69#, 16#6B#, 16#6F#, 16#6E#);
   begin
      if Maker /= null then

         if Maker.all = "Canon" & Ascii.NUL then
            Process_Directory (Image, Canon_IFD, Data, Data'First);

         elsif Maker.all = "CASIO" & Ascii.NUL then
            Process_Directory (Image, Casio_IFD, Data, Data'First);

         elsif Maker.all = "FUJIFILM" & Ascii.NUL then
            -- There are two big differences from the other manufacturers:
            --   1) Fujifilm's Exif data uses Motorola byte order,
            --      but MakerNote ignores it and uses Intel byte order.
            --   2) The other manufacturer's maker notes count the
            --      "offset to data" from the first byte of TIFF header
            --      (same as the other IFD), but Fujifilm counts it from
            --      the first byte of the maker note itself.

            declare
               use System;
               Save_Bit_Order : constant Bit_Order := File_Bit_Order;
               Maker_Data     : constant Storage (0 .. Data'Length - 1)
                                  := Data;
               Offset         : Storage_Offset;

            begin
               File_Bit_Order := Low_Order_First;
               Offset := Storage_Offset
                                 (Unsigned_32'(Value (Maker_Data (8 .. 11))));
               Process_Directory (Image, Fujifilm_IFD, Maker_Data, Offset);
               File_Bit_Order := Save_Bit_Order;
            end;

         elsif Maker.all = "OLYMPUS OPTICAL CO.,LTD" & Ascii.NUL then
            Process_Directory (Image, Olympus_IFD, Data, Data'First + 8);

         elsif Maker.all = "NIKON CORPORATION" & Ascii.NUL
             or Maker.all = "NIKON" & Ascii.NUL
         then
            if Data (Data'First .. Data'First + Nikon_Hex'Length - 1)
                 = Nikon_Hex
            then
               Process_Directory (Image, Nikon_Old_IFD, Data, Data'First + 8);
            else
               Process_Directory (Image, Nikon_IFD, Data, Data'First);
            end if;

         else
            Write (Output, Undefined, Data);
         end if;
      end if;
   end Process_Maker_Note;

begin -- Exifread
   if Argument_Count < 1 then
      Text_IO.Put_Line
        (Standard_Error, "Usage: " & Command_Name & " files...");
      Set_Exit_Status (Failure);
      return;
   end if;

   loop
      case Getopt ("s") is
         when 's' => Run_Web_Server := True;
         when others => exit;
      end case;
   end loop;

   loop
      Do_Argument :
      declare
         Name  : constant String := Get_Argument (Do_Expansion => True);
         File  : Stream_IO.File_Type;
      begin
         Put_Line (Name);
         exit when Name'Length = 0;

         Do_File :
         begin
            Open (File, In_File, Name);
            Process_File (File);
            Close (File);

         exception
            when Text_IO.Name_Error =>
                Put_Line (Standard_Error,  Name & ": file not found");
            when Use_Error =>
                Put_Line (Standard_Error,  Name & ": cannot open file");
            when IO_Exceptions.End_Error =>
                Put_Line (Standard_Error, Name & ": unexpected end of file");
            when Data_Error =>
                Put_Line (Standard_Error, Name & ": data error at byte"
                   & Index (File)'Img);
         end Do_File;
      end Do_Argument;
   end loop;

   if Run_Web_Server then
      declare
         Server_Address : Sock_Addr_Type :=
                            (Family_Inet, Inet_Addr ("127.0.0.1"), 5432);
      begin
         Put_Line ("Serving...");
         Start (Server_Address);
      end;
   else
      List_Images (Output);
   end if;
end Exifread;

