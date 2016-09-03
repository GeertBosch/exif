with GNAT.Sockets;
with Ada.Streams;
with Ada.Streams.Stream_IO;
package Exif.Serve is

   procedure List_Images (Stream : Ada.Streams.Stream_IO.Stream_Access);

   procedure Start (Address : in out GNAT.Sockets.Sock_Addr_Type);

end Exif.Serve;
