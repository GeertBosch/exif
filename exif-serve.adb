with Ada.Unchecked_Conversion;
with Ada.Streams; use Ada.Streams;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Streams.Stream_IO;
with Ada.Text_IO;
with System;

with GNAT.Sockets; use GNAT.Sockets;

with Exif.Store;   use Exif.Store;

package body Exif.Serve is

   Server_Name : constant String := "PhotoServe";

   NL : constant String := Standard.ASCII.CR & Standard.ASCII.LF;

   procedure Write_JPEG (Stream : Stream_IO.Stream_Access; Data : Storage);

   procedure Receive_Socket
     (Socket : Socket_Type;
      Message : out String;
      Last    : out Natural);

   procedure Receive_Socket
     (Socket : Socket_Type;
      Message : out String;
      Last    : out Natural)
   is
      subtype Storage is Ada.Streams.Stream_Element_Array
        (Stream_Element_Offset (Message'First)
          .. Stream_Element_Offset (Message'Last));
      type Storage_Access is access all Storage;

      function To_Storage is
        new Ada.Unchecked_Conversion (System.Address, Storage_Access);

      Last_Offset : Stream_Element_Offset;

   begin
      Receive_Socket (Socket, To_Storage (Message'Address).all, Last_Offset);
      Last := Natural (Last_Offset);
   end Receive_Socket;

   procedure List_Images (Stream : Ada.Streams.Stream_IO.Stream_Access) is

      procedure List_One (Image : Store.Image_ID) is
         Nr : constant String := Image'Img;
      begin
         String'Write (Stream,
           "<A HREF=IMG" & Nr (2 .. Nr'Last) & ">");

         if Get_Thumbnail (Image) /= null then
            String'Write (Stream, "<IMG SRC=THM" & Nr (2 .. Nr'Last) &
                " ALT=" & Name (Image) & ">");
         else
            String'Write (Stream, Name (Image));
         end if;

         String'Write (Stream, "</A>" & NL);
      end List_One;

      procedure List_All is new Iterate (List_One);
   begin
      List_All;
   end List_Images;

   procedure Serve_File
     (Stream : Ada.Streams.Stream_IO.Stream_Access;
      Name   : String)
   is
      NL : constant String := Standard.ASCII.CR & Standard.ASCII.LF;
      OK : constant String := "HTTP/1.0 200 OK" & NL;

      procedure Error is
      begin
         String'Write (Stream,
           "<HTML><HEAD>" & NL &
           "<TITLE>404 Not Found</TITLE>" & NL &
           "</HEAD><BODY>" & NL &
           "<H1>Not Found</H1>" & NL &
           "The requested URL " & Name &
              " was not found on this server.<P>" & NL &
           "<HR>" & NL &
           "<ADDRESS>" & Server_Name & "</ADDRESS>" & NL &
           "</BODY></HTML>" & NL);
      end Error;

   begin
      if Name = "/" then

         --  List all pictures in the database

         String'Write (Stream,
           "<HTML>" & NL & "<HEAD><TITLE>FotoServ</TITLE></HEAD>" & NL &
           "<BODY>" & NL);

         List_Images (Stream);

         String'Write (Stream,
           "</BODY>" & NL &
           "</HTML>" & NL);

      elsif Name'Length > 4 and then
         Name (Name'First .. Name'First + 3) = "/THM"
      then
         --  Show thumbnail
         declare
            Nr : String renames Name (Name'First + 4 .. Name'Last);
            Id : Store.Image_Id := Store.Image_ID'Value (Nr);
         begin
            if not Valid (Id) then
               Error;
               return;
            end if;

            String'Write (Stream, OK);
            Write_JPEG (Stream, Get_Thumbnail (Id).all);
         end;

      elsif Name'Length > 4 and then
         Name (Name'First .. Name'First + 3) = "/IMG"
      then
         --  Show image
         declare
            use Stream_IO;
            Nr : String renames Name (Name'First + 4 .. Name'Last);
            Id : Store.Image_Id := Store.Image_ID'Value (Nr);
            File : Stream_IO.File_Type;
         begin
            if not Valid (Id) then
               Error;
               return;
            end if;

            Open (File, In_File, File_Name (Id));

            --  For Canon RAW files, position the input file so that
            --  the (smaller) JPEG version can be read.

            declare
               JPEG_Offset : Info_Element :=
                  Get ((Id, (Canon_RAW_IFD, JPEG_Image)));
            begin
               if JPEG_Offset.Data /= null then
                  Set_Index (File, Stream_IO.Count (Value (JPEG_Offset) + 1));
                  Ada.Text_IO.Put_Line ("Skipped to JPEG image at byte " &
                    Stream_IO.Count'Image (Index (File)));
               end if;
            end;

            declare
               Input     : Stream_IO.Stream_Access := Stream_IO.Stream (File);
               File_Size : Stream_IO.Count := Size (File);
               Contents  : Storage_Access := new Storage
                             (Storage_Offset (Index (File)) - 1 ..
                              Storage_Offset (File_Size) - 1);
            begin
               Storage'Read (Input, Contents.all);
               String'Write (Stream, OK);
               Write_JPEG (Stream, Contents.all);
            exception when others =>
               Close (File);
               raise;
            end;
            Close (File);
         end;

      else
         Error;
      end if;
   end Serve_File;

   procedure Start (Address : in out GNAT.Sockets.Sock_Addr_Type) is
      Server   : Socket_Type;
      Socket   : Socket_Type;
      Channel  : Stream_IO.Stream_Access;

   begin
      Initialize;

      --  The first step is to create a socket. Once created, this
      --  socket must be associated to with an address. Usually only a
      --  server (Pong here) needs to bind an address explicitly.
      --  Most of the time clients can skip this step because the
      --  socket routines will bind an arbitrary address to an unbound
      --  socket.

      Create_Socket (Server);

      --  Allow reuse of local addresses.

      Set_Socket_Option
        (Server,
         Socket_Level,
         (Reuse_Address, True));

      Bind_Socket (Server, Address);

      --  A server marks a socket as willing to receive connect events.

      Listen_Socket (Server);

      loop
         --  Once a server calls Listen_Socket, incoming connects events
         --  can be accepted. The returned Socket is a new socket that
         --  represents the server side of the connection. Server remains
         --  available to receive further connections.

         Accept_Socket (Server, Socket, Address);

         Channel := Stream_IO.Stream_Access (Stream (Socket));

         --  Receive and print message from client Ping.

         declare
            Message : String (1 .. 10_000);
            Last    : Natural := 0;
            EOL     : Natural := 0;

         begin
            Receive_Request :
            while Last < Message'Last loop
               Receive_Socket (Socket,
                  Message (Last + 1 .. Message'Last), Last);

               exit Receive_Request when EOL = Last;

               while EOL < Last loop
                  EOL := EOL + 1;
                  exit Receive_Request when Message (EOL) = Standard.ASCII.CR;
               end loop;
            end loop Receive_Request;

            if EOL > 5 and then Message (1 .. 4) = "GET " then

               Process_Request :
               declare
                  Request : String renames Message (5 .. EOL - 1);
                  Space   : Positive := Request'First;
               begin
                  while Space <= Request'Last and Request (Space) = ' ' loop
                     Space := Space + 1;
                  end loop;

                  while Space <= Request'Last and Request (Space) /= ' ' loop
                     Space := Space + 1;
                  end loop;

                  Ada.Text_IO.Put_Line
                    ("Request """ & Request (Request'First .. Space - 1)
                     & """ with method """
                     & Request (Space + 1 .. Request'Last) & """");

                  Serve_File (Channel, Request (Request'First .. Space - 1));
               end Process_Request;
            else
               Ada.Text_IO.Put_Line
                  ("Request too short: " & Message ( 1 .. EOL));
            end if;


         exception
            when E : others => Ada.Text_IO.Put_Line
              (Exception_Name (E) & ": " & Exception_Message (E));
         end;

         Close_Socket (Socket);
      end loop;

      Close_Socket (Server);
      Finalize;

   exception when E : others =>
      Ada.Text_IO.Put_Line
        (Exception_Name (E) & ": " & Exception_Message (E));
      Finalize;
   end Start;

   procedure Write_JPEG (Stream : Stream_IO.Stream_Access; Data : Storage) is
   begin
      String'Write (Stream,
        "Content-Length:" & Natural'Image (Data'Length) & NL &
        "Content-Type: image/jpeg" & NL & NL);

      Storage'Write (Stream, Data);
   end Write_JPEG;

end Exif.Serve;
