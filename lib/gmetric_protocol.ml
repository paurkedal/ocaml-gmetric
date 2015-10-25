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

open Gmetric

type defaults = {
  default_hostname : string;
}

let write_bool c buf off =
  Bytes.set buf (off + 0) '\x00';
  Bytes.set buf (off + 1) '\x00';
  Bytes.set buf (off + 2) '\x00';
  Bytes.set buf (off + 3) (if c then '\x01' else '\x00');
  off + 4

let write_int i buf off =
  Bytes.set buf (off + 0) (Char.chr (i lsr 24));
  Bytes.set buf (off + 1) (Char.chr (i lsr 16 land 0xff));
  Bytes.set buf (off + 2) (Char.chr (i lsr 8 land 0xff));
  Bytes.set buf (off + 3) (Char.chr (i land 0xff));
  off + 4

let write_int32 i buf off =
  let (lsr) = Int32.shift_right_logical in
  let (land) = Int32.logand in
  let chr i = Char.chr (Int32.to_int i) in
  Bytes.set buf (off + 0) (chr (i lsr 24));
  Bytes.set buf (off + 1) (chr (i lsr 16 land 0xffl));
  Bytes.set buf (off + 2) (chr (i lsr 8 land 0xffl));
  Bytes.set buf (off + 3) (chr (i land 0xffl));
  off + 4

let size_string s = (String.length s + 7) / 4 * 4

let write_string s buf off =
  let bare_off = write_int (String.length s) buf off in
  let bare_size = (String.length s + 3) / 4 * 4 in
  let len = String.length s in
  Bytes.blit_string s 0 buf bare_off len;
  Bytes.blit_string "\x00\x00\x00" 0 buf (bare_off + len) (bare_size - len);
  bare_off + bare_size

(* No, it's not that simple!
let size_value : type a. a Type.t -> a -> int = function
  | Type.String -> size_string
  | Type.Uint16 -> fun _ -> 4
  | Type.Int16 -> fun _ -> 4
  | Type.Uint32 -> fun _ -> 4
  | Type.Int32 -> fun _ -> 4
  | Type.Float -> fun _ -> 4
  | Type.Double -> fun _ -> 8
let write_value : type a. a Type.t -> a -> Bytes.t -> int -> int = function
  | Type.String -> write_string
  | Type.Uint16 -> write_int
  | Type.Int16 -> write_int
  | Type.Uint32 -> write_int32
  | Type.Int32 -> write_int32
  | Type.Float -> write_float
  | Type.Double -> write_double
*)

let size_string_value s = 8 + size_string s
let write_string_value s buf off = off
  |> write_string "%s" buf |> write_string s buf

let size_int_value i = size_string_value (string_of_int i)
let write_int_value i = write_string_value (string_of_int i)

let size_int32_value i = size_string_value (Int32.to_string i)
let write_int32_value i = write_string_value (Int32.to_string i)

let size_float_value x = size_string_value (string_of_float x)
let write_float_value x = write_string_value (string_of_float x)

let size_value : type a. a Type.t -> a -> int = function
  | Type.String -> size_string_value
  | Type.Uint16 -> size_int_value
  | Type.Int16 -> size_int_value
  | Type.Uint32 -> size_int32_value
  | Type.Int32 -> size_int32_value
  | Type.Float -> size_float_value
  | Type.Double -> size_float_value

let write_value : type a. a Type.t -> a -> Bytes.t -> int -> int = function
  | Type.String -> write_string_value
  | Type.Uint16 -> write_int_value
  | Type.Int16 -> write_int_value
  | Type.Uint32 -> write_int32_value
  | Type.Int32 -> write_int32_value
  | Type.Float -> write_float_value
  | Type.Double -> write_float_value

let hostname {default_hostname} = function
  | {gm_hostname = None} -> default_hostname
  | {gm_hostname = Some hostname} -> hostname

let size_declaration defaults gm =
    24
  + size_string (hostname defaults gm)
  + size_string gm.gm_name
  + size_string (Type.to_string gm.gm_type)
  + size_string gm.gm_name
  + size_string gm.gm_units
  + String_map.fold (fun a x acc -> size_string a + size_string x + acc)
		    gm.gm_attrs 0

let write_declaration defaults gm buf off = off
  |> write_int 0x80 buf
  |> write_string (hostname defaults gm) buf
  |> write_string gm.gm_name buf
  |> write_bool (gm.gm_hostname <> None) buf
  |> write_string (Type.to_string gm.gm_type) buf
  |> write_string gm.gm_name buf
  |> write_string gm.gm_units buf
  |> write_int (Slope.to_int gm.gm_slope) buf
  |> write_int gm.gm_tmax buf
  |> write_int gm.gm_dmax buf
  |> write_int (String_map.cardinal gm.gm_attrs) buf
  |> String_map.fold
      (fun a x off -> off
	|> write_string a buf
	|> write_string x buf)
      gm.gm_attrs

let size_datapoint defaults gm x =
    8
  + size_string (hostname defaults gm)
  + size_string gm.gm_name
  + size_value gm.gm_type x

let write_datapoint defaults gm x buf off = off
  |> write_int 0x85 buf
  |> write_string (hostname defaults gm) buf
  |> write_string gm.gm_name buf
  |> write_bool (gm.gm_hostname <> None) buf
  |> write_value gm.gm_type x buf
