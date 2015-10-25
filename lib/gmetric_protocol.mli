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

(** Gmetric protocol encoders. *)

type defaults = {
  default_hostname : string;
}

val size_declaration : defaults -> 'a Gmetric.t -> int
val write_declaration : defaults -> 'a Gmetric.t -> Bytes.t -> int -> int

val size_datapoint : defaults -> 'a Gmetric.t -> 'a -> int
val write_datapoint : defaults -> 'a Gmetric.t -> 'a -> Bytes.t -> int -> int
