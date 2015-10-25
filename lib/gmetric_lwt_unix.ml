(* Copyright (C) 2015  Petter A. Urkedal <paurkedal@gmail.com>
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or (at your
 * option) any later version, with the OCaml static compilation exception.
 *
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library.  If not, see <http://www.gnu.org/licenses/>.
 *)

open Lwt.Infix

module Gmetric_io = struct

  let defaults =
    let open Gmetric_protocol in
    {default_hostname = Unix.gethostname ()}

  let send_bytes remote_addr buf =
    let domain = Unix.domain_of_sockaddr remote_addr in
    let local_addr =
      match domain with
      | Unix.PF_UNIX -> invalid_arg "Gmetric_lwt.sendto"
      | Unix.PF_INET -> Unix.ADDR_INET (Unix.inet_addr_any, 0)
      | Unix.PF_INET6 -> Unix.ADDR_INET (Unix.inet6_addr_any, 0) in
    let socket = Lwt_unix.socket domain Lwt_unix.SOCK_DGRAM 0 in
    Lwt_unix.bind socket local_addr;
    Lwt_unix.sendto socket buf 0 (Bytes.length buf) [] remote_addr >>= fun _ ->
    Lwt_unix.close socket

  let send_declaration remote_addr gm =
    let len = Gmetric_protocol.size_declaration defaults gm in
    let buf = Bytes.create len in
    let len' = Gmetric_protocol.write_declaration defaults gm buf 0 in
    assert (len = len');
    send_bytes remote_addr buf

  let send_datapoint remote_addr gm x =
    let len = Gmetric_protocol.size_datapoint defaults gm x in
    let buf = Bytes.create len in
    let len' = Gmetric_protocol.write_datapoint defaults gm x buf 0 in
    assert (len = len');
    send_bytes remote_addr buf

end
