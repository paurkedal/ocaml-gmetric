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

module Slope = struct
  type t = Zero | Positive | Negative | Both | Unspecified

  let to_int = function
    | Zero -> 0
    | Positive -> 1
    | Negative -> 2
    | Both -> 3
    | Unspecified -> 4
end

module Type = struct
  type _ t =
    | String : string t
    | Uint16 : int t
    | Int16 : int t
    | Uint32 : Int32.t t
    | Int32 : Int32.t t
    | Float : float t
    | Double : float t

  let to_string : type a. a t -> string = function
    | String -> "string"
    | Uint16 -> "uint16"
    | Int16 -> "int16"
    | Uint32 -> "uint32"
    | Int32 -> "int32"
    | Float -> "float"
    | Double -> "double"
end

module String_map = Map.Make (String)

type 'a t = {
  gm_hostname : string option;
  gm_name : string;
  gm_type : 'a Type.t;
  gm_units : string;
  gm_slope : Slope.t;
  gm_tmax : int;
  gm_dmax : int;
  gm_attrs : string String_map.t;
}

let fold_option f = function
  | None -> fun acc -> acc
  | Some x -> f x

let create ?hostname ?(units = "")
	   ?(tmax = 60) ?(dmax = 3600) ?(slope = Slope.Unspecified)
	   ?group ?cluster ?desc ?title
	   name t =
  let attrs = String_map.empty
    |> fold_option (String_map.add "GROUP") group
    |> fold_option (String_map.add "CLUSTER") cluster
    |> fold_option (String_map.add "DESC") desc
    |> fold_option (String_map.add "TITLE") title in
  { gm_hostname = hostname;
    gm_name = name;
    gm_type = t;
    gm_units = units;
    gm_slope = slope;
    gm_tmax = tmax;
    gm_dmax = dmax;
    gm_attrs = attrs; }
