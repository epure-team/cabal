(******************************************************************************)
(*                                                                            *)
(* Copyright (c) 2026 Epure Team                                              *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(******************************************************************************)

(** Deterministic, runtime-materialized CBL-08 media/schema fixtures. *)

open Cabal

type image_semantics = {
  width : int;
  height : int;
  dominant_color : string;
}

type t = {
  attachment : Backend_types.media_attachment;
  bytes : string;
  response_field : string;
  semantics : image_semantics;
}

type response_error = Schema_rejected | Image_semantics_mismatch
type image_error = Invalid_image | Unsupported_image | Golden_image_mismatch

let uint32_be value =
  let bytes = Bytes.create 4 in
  Bytes.set bytes 0
    (Char.chr Int32.(to_int (logand (shift_right_logical value 24) 0xffl))) ;
  Bytes.set bytes 1
    (Char.chr Int32.(to_int (logand (shift_right_logical value 16) 0xffl))) ;
  Bytes.set bytes 2
    (Char.chr Int32.(to_int (logand (shift_right_logical value 8) 0xffl))) ;
  Bytes.set bytes 3 (Char.chr Int32.(to_int (logand value 0xffl))) ;
  Bytes.unsafe_to_string bytes

let uint16_le value =
  String.init 2 (function
    | 0 -> Char.chr (value land 0xff)
    | _ -> Char.chr ((value lsr 8) land 0xff))

let crc32 value =
  let crc = ref Int32.minus_one in
  String.iter
    (fun character ->
      crc := Int32.logxor !crc (Int32.of_int (Char.code character)) ;
      for _ = 0 to 7 do
        crc :=
          if Int32.logand !crc 1l <> 0l then
            Int32.logxor
              (Int32.shift_right_logical !crc 1)
              0xedb88320l
          else Int32.shift_right_logical !crc 1
      done)
    value ;
  Int32.lognot !crc

let adler32 value =
  let modulo = 65_521 in
  let a = ref 1 in
  let b = ref 0 in
  String.iter
    (fun character ->
      a := (!a + Char.code character) mod modulo ;
      b := (!b + !a) mod modulo)
    value ;
  Int32.logor
    (Int32.shift_left (Int32.of_int !b) 16)
    (Int32.of_int !a)

let png_chunk kind payload =
  let body = kind ^ payload in
  uint32_be (Int32.of_int (String.length payload)) ^ body
  ^ uint32_be (crc32 body)

let stored_zlib value =
  let length = String.length value in
  if length > 0xffff then invalid_arg "CBL-08 fixture deflate block is too large" ;
  "\x78\x01\x01" ^ uint16_le length ^ uint16_le (lnot length land 0xffff)
  ^ value ^ uint32_be (adler32 value)

let solid_png red green blue =
  let width = 64 in
  let height = 64 in
  let pixel =
    String.init 3 (function
      | 0 -> Char.chr red
      | 1 -> Char.chr green
      | _ -> Char.chr blue)
  in
  let scanline = "\x00" ^ String.concat "" (List.init width (fun _ -> pixel)) in
  let pixels = String.concat "" (List.init height (fun _ -> scanline)) in
  let ihdr =
    uint32_be (Int32.of_int width)
    ^ uint32_be (Int32.of_int height)
    ^ "\x08\x02\x00\x00\x00"
  in
  "\x89PNG\r\n\x1a\n" ^ png_chunk "IHDR" ihdr
  ^ png_chunk "IDAT" (stored_zlib pixels)
  ^ png_chunk "IEND" ""

(* A deterministic 64x64 solid-red baseline JPEG. Keeping the 336-byte encoded
   image inline avoids an image-library/runtime dependency and a binary fixture. *)
let red_jpeg =
  String.concat ""
    [
      "\xff\xd8\xff\xe0\x00\x10\x4a\x46\x49\x46\x00\x01\x01\x00\x00\x01\x00\x01\x00\x00\xff\xdb\x00\x43";
      "\x00\x02\x01\x01\x01\x01\x01\x02\x01\x01\x01\x02\x02\x02\x02\x02\x04\x03\x02\x02\x02\x02\x05\x04";
      "\x04\x03\x04\x06\x05\x06\x06\x06\x05\x06\x06\x06\x07\x09\x08\x06\x07\x09\x07\x06\x06\x08\x0b\x08";
      "\x09\x0a\x0a\x0a\x0a\x0a\x06\x08\x0b\x0c\x0b\x0a\x0c\x09\x0a\x0a\x0a\xff\xdb\x00\x43\x01\x02\x02";
      "\x02\x02\x02\x02\x05\x03\x03\x05\x0a\x07\x06\x07\x0a\x0a\x0a\x0a\x0a\x0a\x0a\x0a\x0a\x0a\x0a\x0a";
      "\x0a\x0a\x0a\x0a\x0a\x0a\x0a\x0a\x0a\x0a\x0a\x0a\x0a\x0a\x0a\x0a\x0a\x0a\x0a\x0a\x0a\x0a\x0a\x0a";
      "\x0a\x0a\x0a\x0a\x0a\x0a\x0a\x0a\x0a\x0a\x0a\x0a\x0a\x0a\xff\xc0\x00\x11\x08\x00\x40\x00\x40\x03";
      "\x01\x11\x00\x02\x11\x01\x03\x11\x01\xff\xc4\x00\x15\x00\x01\x01\x00\x00\x00\x00\x00\x00\x00\x00";
      "\x00\x00\x00\x00\x00\x00\x00\x08\xff\xc4\x00\x14\x10\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00";
      "\x00\x00\x00\x00\x00\x00\xff\xc4\x00\x16\x01\x01\x01\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00";
      "\x00\x00\x00\x00\x08\x09\xff\xc4\x00\x14\x11\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00";
      "\x00\x00\x00\x00\xff\xda\x00\x0c\x03\x01\x00\x02\x11\x03\x11\x00\x3f\x00\x98\xd3\xfb\x60\x00\x00";
      "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00";
      "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x07\xff\xd9";
    ]

let uint16_be_at value offset =
  if offset < 0 || offset + 2 > String.length value then None
  else
    Some
      ((Char.code value.[offset] lsl 8) lor Char.code value.[offset + 1])

let uint16_le_at value offset =
  if offset < 0 || offset + 2 > String.length value then None
  else
    Some
      (Char.code value.[offset] lor (Char.code value.[offset + 1] lsl 8))

let uint32_be_at value offset =
  if offset < 0 || offset + 4 > String.length value then None
  else
    Some
      ((Char.code value.[offset] lsl 24)
      lor (Char.code value.[offset + 1] lsl 16)
      lor (Char.code value.[offset + 2] lsl 8)
      lor Char.code value.[offset + 3])

let parse_png_chunks bytes =
  let length = String.length bytes in
  if length < 8 || String.sub bytes 0 8 <> "\x89PNG\r\n\x1a\n" then
    Error Invalid_image
  else
    let rec loop offset ihdr idat =
      if offset + 12 > length then Error Invalid_image
      else
        match uint32_be_at bytes offset with
        | None -> Error Invalid_image
        | Some chunk_length ->
            let payload_offset = offset + 8 in
            let payload_end = payload_offset + chunk_length in
            let chunk_end = payload_end + 4 in
            if chunk_length < 0 || payload_end < payload_offset || chunk_end > length
            then Error Invalid_image
            else
              let kind = String.sub bytes (offset + 4) 4 in
              let payload = String.sub bytes payload_offset chunk_length in
              let expected_crc = uint32_be (crc32 (kind ^ payload)) in
              let actual_crc = String.sub bytes payload_end 4 in
              if actual_crc <> expected_crc then Error Invalid_image
              else
                match kind with
                | "IHDR" when ihdr = None -> loop chunk_end (Some payload) idat
                | "IHDR" -> Error Invalid_image
                | "IDAT" -> loop chunk_end ihdr (payload :: idat)
                | "IEND" when chunk_length = 0 && chunk_end = length -> (
                    match ihdr with
                    | Some header ->
                        Ok (header, String.concat "" (List.rev idat))
                    | None -> Error Invalid_image)
                | "IEND" -> Error Invalid_image
                | _ -> loop chunk_end ihdr idat
    in
    loop 8 None []

let decode_stored_zlib bytes =
  let length = String.length bytes in
  if length < 11 then Error Invalid_image
  else
    let cmf = Char.code bytes.[0] in
    let flg = Char.code bytes.[1] in
    if
      cmf land 0x0f <> 8
      || ((cmf lsl 8) + flg) mod 31 <> 0
      || flg land 0x20 <> 0
    then Error Unsupported_image
    else
      let output = Buffer.create length in
      let rec blocks offset =
        if offset + 5 > length - 4 then Error Invalid_image
        else
          let header = Char.code bytes.[offset] in
          let final = header land 1 = 1 in
          if header land 0xfe <> 0 then Error Unsupported_image
          else
            match
              (uint16_le_at bytes (offset + 1), uint16_le_at bytes (offset + 3))
            with
            | Some block_length, Some complement
              when block_length lxor complement = 0xffff ->
                let payload_offset = offset + 5 in
                let payload_end = payload_offset + block_length in
                if payload_end > length - 4 then Error Invalid_image
                else (
                  Buffer.add_substring output bytes payload_offset block_length ;
                  if final then
                    if payload_end <> length - 4 then Error Invalid_image
                    else
                      let decoded = Buffer.contents output in
                      if
                        String.sub bytes payload_end 4
                        = uint32_be (adler32 decoded)
                      then Ok decoded
                      else Error Invalid_image
                  else blocks payload_end)
            | Some _, Some _ | None, _ | _, None -> Error Invalid_image
      in
      blocks 2

let nearest_color red green blue =
  let first = ("black", 0, 0, 0) in
  let rest =
    [
      ("blue", 0, 0, 255);
      ("green", 0, 128, 0);
      ("orange", 255, 165, 0);
      ("purple", 128, 0, 128);
      ("red", 255, 0, 0);
      ("white", 255, 255, 255);
      ("yellow", 255, 255, 0);
    ]
  in
  let distance (_, candidate_red, candidate_green, candidate_blue) =
    let square value = value * value in
    square (red - candidate_red)
    + square (green - candidate_green)
    + square (blue - candidate_blue)
  in
  let nearest =
    List.fold_left
      (fun nearest candidate ->
        if distance candidate < distance nearest then candidate else nearest)
      first rest
  in
  let name, _, _, _ = nearest in
  name

let inspect_png bytes =
  match parse_png_chunks bytes with
  | Error _ as error -> error
  | Ok (ihdr, compressed) -> (
      if String.length ihdr <> 13 then Error Invalid_image
      else
        match (uint32_be_at ihdr 0, uint32_be_at ihdr 4) with
        | Some width, Some height
          when width > 0
               && height > 0
               && width <= 4096
               && height <= 4096
               && String.sub ihdr 8 5 = "\x08\x02\x00\x00\x00" -> (
            match decode_stored_zlib compressed with
            | Error _ as error -> error
            | Ok pixels ->
                let row_length = 1 + (width * 3) in
                if String.length pixels <> row_length * height then
                  Error Invalid_image
                else
                  let red = ref 0 in
                  let green = ref 0 in
                  let blue = ref 0 in
                  let valid = ref true in
                  for row = 0 to height - 1 do
                    let row_offset = row * row_length in
                    if pixels.[row_offset] <> '\x00' then valid := false
                    else
                      for column = 0 to width - 1 do
                        let pixel_offset = row_offset + 1 + (column * 3) in
                        red := !red + Char.code pixels.[pixel_offset] ;
                        green := !green + Char.code pixels.[pixel_offset + 1] ;
                        blue := !blue + Char.code pixels.[pixel_offset + 2]
                      done
                  done ;
                  if not !valid then Error Unsupported_image
                  else
                    let pixel_count = width * height in
                    Ok
                      {
                        width;
                        height;
                        dominant_color =
                          nearest_color
                            (!red / pixel_count)
                            (!green / pixel_count)
                            (!blue / pixel_count);
                      })
        | Some _, Some _ | None, _ | _, None -> Error Unsupported_image)

let jpeg_dimensions bytes =
  let length = String.length bytes in
  if length < 4 || String.sub bytes 0 2 <> "\xff\xd8" then Error Invalid_image
  else
    let rec skip_fill offset =
      if offset < length && bytes.[offset] = '\xff' then skip_fill (offset + 1)
      else offset
    in
    let rec markers offset =
      if offset >= length || bytes.[offset] <> '\xff' then Error Invalid_image
      else
        let code_offset = skip_fill offset in
        if code_offset >= length then Error Invalid_image
        else
          let code = Char.code bytes.[code_offset] in
          let segment_offset = code_offset + 1 in
          if code = 0xd9 || code = 0xda then Error Unsupported_image
          else if code = 0xd8 || code = 0x01 || (code >= 0xd0 && code <= 0xd7)
          then markers segment_offset
          else
            match uint16_be_at bytes segment_offset with
            | None -> Error Invalid_image
            | Some segment_length when segment_length < 2 -> Error Invalid_image
            | Some segment_length ->
                let segment_end = segment_offset + segment_length in
                if segment_end > length then Error Invalid_image
                else if code = 0xc0 then
                  if segment_length < 8 then Error Invalid_image
                  else
                    match
                      ( uint16_be_at bytes (segment_offset + 3),
                        uint16_be_at bytes (segment_offset + 5) )
                    with
                    | Some height, Some width when width > 0 && height > 0 ->
                        Ok (width, height)
                    | Some _, Some _ | None, _ | _, None -> Error Invalid_image
                else markers segment_end
    in
    markers 2

(** This is the CBL-07 probe's reviewed JPEG golden. Independent ImageMagick
    7.1.2-29 decoding reports 64x64, [srgb(224,31,32)] at pixel (0,0), and
    normalized channel means [(0.878431, 0.121569, 0.12549)]. The structural
    suite deliberately does not depend on ImageMagick: it pins these exact bytes
    by size/SHA-256 and independently parses the SOF dimensions before assigning
    the reviewed red provenance. *)
let red_jpeg_golden_size = 336

let red_jpeg_golden_sha256 =
  "86bf3e5ac9402d1e210db8199d7fb4ea42e567cdf8097e2d18d527d0d77ae1e4"

let inspect_jpeg bytes =
  let digest = Digestif.SHA256.(to_hex (digest_string bytes)) in
  if
    String.length bytes <> red_jpeg_golden_size
    || digest <> red_jpeg_golden_sha256
  then Error Golden_image_mismatch
  else
    match jpeg_dimensions bytes with
    | Error _ as error -> error
    | Ok (width, height) -> Ok {width; height; dominant_color = "red"}

let inspect_image media_type bytes =
  match media_type with
  | Backend_types.Png -> inspect_png bytes
  | Backend_types.Jpeg -> inspect_jpeg bytes

let make ~id ~path ~media_type ~response_field bytes =
  let attachment =
    Backend_types.
      {
        id;
        path;
        media_type;
        sha256 = Digestif.SHA256.(to_hex (digest_string bytes));
        size_bytes = String.length bytes;
      }
  in
  match inspect_image media_type bytes with
  | Ok semantics -> Some {attachment; bytes; response_field; semantics}
  | Error _ -> None

let all =
  List.filter_map Fun.id
    [
      make ~id:"fixture-a" ~path:"fixture-a.png" ~media_type:Backend_types.Png
        ~response_field:"png_dominant_color" (solid_png 20 45 225);
      make ~id:"fixture-b" ~path:"fixture-b.jpg" ~media_type:Backend_types.Jpeg
        ~response_field:"jpeg_dominant_color" red_jpeg;
    ]

let for_media_types media_types =
  List.filter
    (fun fixture -> List.mem fixture.attachment.media_type media_types)
    all

let color_schema =
  `Assoc
    [
      ("type", `String "string");
      ( "enum",
        `List
          (List.map
             (fun color -> `String color)
             ["black"; "blue"; "green"; "orange"; "purple"; "red"; "white"; "yellow"]) );
    ]

let schema fixtures =
  `Assoc
    [
      ( "$schema",
        `String
          ("https://json-schema.org/draft/"
          ^ E2e_harness_config.native_schema_draft
          ^ "/schema") );
      ("type", `String "object");
      ( "properties",
        `Assoc
          (List.map
             (fun fixture -> (fixture.response_field, color_schema))
             fixtures) );
      ( "required",
        `List (List.map (fun fixture -> `String fixture.response_field) fixtures) );
      ("additionalProperties", `Bool false);
    ]

let expected_document fixtures =
  `Assoc
    (List.map
       (fun fixture ->
         (fixture.response_field, `String fixture.semantics.dominant_color))
       fixtures)

let expected_document_text fixtures =
  expected_document fixtures |> Yojson.Safe.to_string ~std:true

let expected_members fixtures =
  List.map
    (fun fixture -> (fixture.response_field, fixture.semantics.dominant_color))
    fixtures

let validate_fixture_semantics fixture =
  let actual_digest = Digestif.SHA256.(to_hex (digest_string fixture.bytes)) in
  if
    fixture.attachment.size_bytes <> String.length fixture.bytes
    || fixture.attachment.sha256 <> actual_digest
  then Error Invalid_image
  else
    match inspect_image fixture.attachment.media_type fixture.bytes with
    | Error _ as error -> error
    | Ok semantics ->
        if semantics = fixture.semantics then Ok ()
        else Error Golden_image_mismatch

let validate_semantics fixtures document =
  try
    match Yojson.Safe.from_string document with
    | `Assoc members ->
        let actual =
          List.map
            (function name, `String value -> Some (name, value) | _ -> None)
            members
        in
        if List.for_all Option.is_some actual then
          let actual = List.filter_map Fun.id actual |> List.sort compare in
          let expected = expected_members fixtures |> List.sort compare in
          if actual = expected then Ok () else Error Image_semantics_mismatch
        else Error Image_semantics_mismatch
    | _ -> Error Image_semantics_mismatch
  with Yojson.Json_error _ -> Error Image_semantics_mismatch

let validate_response fixtures document =
  match Json_schema_validator.validate ~schema:(schema fixtures) ~document with
  | Error _ -> Error Schema_rejected
  | Ok () -> validate_semantics fixtures document

let write_file path bytes =
  let channel = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr channel)
    (fun () -> output_string channel bytes) ;
  Unix.chmod path 0o600

let materialize ~working_dir fixtures =
  List.iter
    (fun fixture ->
      write_file (Filename.concat working_dir fixture.attachment.path) fixture.bytes)
    fixtures ;
  List.map (fun fixture -> fixture.attachment) fixtures

let prompt =
  "Inspect the actual contents of every attached image. Identify the dominant \
   visible color in each image, using one of the color names allowed by the \
   output schema. Return only the JSON object required by the schema. Do not \
   infer colors from attachment identifiers or filenames, and do not include \
   prose or markdown."
